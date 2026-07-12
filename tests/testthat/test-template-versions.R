template_version_test_con <- function(env = parent.frame()) {
  db_path <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  withr::defer({
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(db_path, force = TRUE)
  }, envir = env)
  con
}

load_template_version_dependencies <- function() {
  source_from_root(
    "R/persistence/migrate.R",
    "R/persistence/repositories/study_repository.R",
    "R/persistence/repositories/template_version_repository.R",
    "R/persistence/repositories/ict_costing_repository.R",
    "R/persistence/repositories/posting_line_repository.R",
    "R/addons/custom_activities/ca_queries.R"
  )
}

insert_version_test_study <- function(con) {
  DBI::dbExecute(con, "
    INSERT INTO meta_data
      (cpms_id, study_site, scenario_id, edge_id, study_name, original_filename, saved_file_path)
    VALUES ('12345', 'RDUHT', 'A', 'EDGE-1', 'Version Test', 'baseline.xlsx', '/tmp/baseline.xlsx')
  ")
  DBI::dbGetQuery(con, "SELECT id FROM meta_data WHERE cpms_id = '12345'")$id[[1]]
}

activate_test_version <- function(repo, version_id, study_id) {
  repo$set_edge_zip_path(version_id, paste0("/tmp/version-", version_id, ".zip"), study_id)
  repo$activate(version_id, study_id)
  invisible(version_id)
}

test_that("template resolver selects the latest active version by activity date", {
  load_template_version_dependencies()
  con <- template_version_test_con()
  old_wd <- setwd(rids_repo_root())
  on.exit(setwd(old_wd), add = TRUE)
  run_migrations(con)

  study_id <- insert_version_test_study(con)
  repo <- template_version_repository(con)
  baseline_id <- repo$create(study_id, "baseline", original_filename = "baseline.xlsx",
                             saved_file_path = "/tmp/baseline.xlsx")
  expect_null(repo$resolve_for_activity_date("12345", "RDUHT", "A", as.Date("2025-01-01")))
  expect_error(
    repo$activate(baseline_id, study_id),
    "before its EDGE ZIP"
  )
  activate_test_version(repo, baseline_id, study_id)
  expect_error(
    repo$create(study_id, "baseline", original_filename = "duplicate.xlsx",
                saved_file_path = "/tmp/duplicate.xlsx"),
    "already has a baseline"
  )
  expect_error(repo$archive(baseline_id, as_of_date = as.Date("2025-01-01")), "newer amendment")
  amendment_1 <- repo$create(
    study_id, "substantial_amendment", as.Date("2025-02-01"),
    original_filename = "amendment-1.xlsx", saved_file_path = "/tmp/amendment-1.xlsx"
  )
  expect_equal(
    repo$resolve_for_activity_date("12345", "RDUHT", "A", as.Date("2025-03-01"))$version_id,
    baseline_id
  )
  expect_error(
    repo$create(
      study_id, "distribution_amendment", as.Date("2025-04-01"),
      original_filename = "blocked.xlsx", saved_file_path = "/tmp/blocked.xlsx"
    ),
    "already has an amendment being processed"
  )
  activate_test_version(repo, amendment_1, study_id)
  amendment_2 <- repo$create(
    study_id, "distribution_amendment", as.Date("2025-06-01"),
    original_filename = "amendment-2.xlsx", saved_file_path = "/tmp/amendment-2.xlsx"
  )
  activate_test_version(repo, amendment_2, study_id)

  expect_equal(repo$resolve_for_activity_date("12345", "RDUHT", "A", as.Date("2025-01-31"))$version_id, baseline_id)
  expect_equal(repo$resolve_for_activity_date("12345", "RDUHT", "A", as.Date("2025-02-01"))$version_id, amendment_1)
  expect_equal(repo$resolve_for_activity_date("12345", "RDUHT", "A", as.Date("2025-07-01"))$version_id, amendment_2)

  repo$archive(baseline_id, expected_study_id = study_id, as_of_date = as.Date("2025-07-01"))
  repo$archive(amendment_1, expected_study_id = study_id, as_of_date = as.Date("2025-07-01"))
  expect_equal(repo$resolve_for_activity_date("12345", "RDUHT", "A", as.Date("2025-01-01"))$version_id, baseline_id)
  expect_equal(repo$resolve_for_activity_date("12345", "RDUHT", "A", as.Date("2025-03-01"))$version_id, amendment_1)
  expect_equal(repo$resolve_available_for_activity_date("12345", "RDUHT", "A", as.Date("2025-07-01"))$version_id, amendment_2)
  expect_null(repo$resolve_available_for_activity_date("12345", "RDUHT", "A", as.Date("2025-01-01")))
  expect_error(
    repo$archive(amendment_2, expected_study_id = study_id, as_of_date = as.Date("2025-07-01")),
    "newer amendment"
  )
  expect_error(
    repo$resolve_for_activity_date("12345", "RDUHT", "A", NA),
    "Activity date"
  )
})

test_that("study file lookup falls back to metadata when version creation did not complete", {
  load_template_version_dependencies()
  con <- template_version_test_con()
  old_wd <- setwd(rids_repo_root())
  on.exit(setwd(old_wd), add = TRUE)
  run_migrations(con)

  insert_version_test_study(con)
  files <- study_repository(con)$find_run_files("12345", "RDUHT", "A")
  expect_equal(nrow(files), 1)
  expect_identical(files$saved_file_path[[1]], "/tmp/baseline.xlsx")
})

test_that("costing and posting rows are isolated by template version", {
  load_template_version_dependencies()
  con <- template_version_test_con()
  old_wd <- setwd(rids_repo_root())
  on.exit(setwd(old_wd), add = TRUE)
  run_migrations(con)

  study_id <- insert_version_test_study(con)
  versions <- template_version_repository(con)
  baseline_id <- versions$create(study_id, "baseline", original_filename = "baseline.xlsx",
                                 saved_file_path = "/tmp/baseline.xlsx")
  activate_test_version(versions, baseline_id, study_id)
  amendment_id <- versions$create(
    study_id, "substantial_amendment", as.Date("2025-02-01"),
    original_filename = "amendment.xlsx", saved_file_path = "/tmp/amendment.xlsx"
  )

  make_cost <- function(value) data.frame(
    CPMS_ID = "12345", study_site = "RDUHT", scenario_id = "A", Study = "Study",
    Visit_Number = "VISIT - 001", Study_Arm = "Arm", Arm_Identity = "Arm",
    Visit_Label = "Visit 1", Activity_Name = "Blood test", ICT_Cost = value,
    Contract_Cost = value, activity_occurrence_id = 1L, staff_group = 1L
  )
  costing <- ict_costing_repository(con)
  expect_error(
    costing$replace_run(make_cost(99), "12345", "RDUHT", "A"),
    "requires a template version ID"
  )
  costing$replace_run(make_cost(10), "12345", "RDUHT", "A", baseline_id)
  costing$replace_run(make_cost(20), "12345", "RDUHT", "A", amendment_id)
  expect_equal(costing$find_by_run("12345", "RDUHT", "A", baseline_id)$ICT_Cost, 10)
  expect_equal(costing$find_by_run("12345", "RDUHT", "A", amendment_id)$ICT_Cost, 20)

  posting <- posting_line_repository(con)
  make_posting <- function(value) data.frame(
    row_id = 1L, scenario_id = "A", cpms_id = "12345", study_site = "RDUHT",
    Study_Arm = "Arm", posting_amount = value
  )
  expect_error(
    posting$replace_for_run(make_posting(99), "12345", "RDUHT", "A"),
    "requires a template version ID"
  )
  posting$replace_for_run(make_posting(10), "12345", "RDUHT", "A", baseline_id)
  posting$replace_for_run(make_posting(20), "12345", "RDUHT", "A", amendment_id)
  expect_equal(posting$find_by_run("12345", "RDUHT", "A", baseline_id)$posting_amount, 10)
  expect_equal(posting$find_by_run("12345", "RDUHT", "A", amendment_id)$posting_amount, 20)
})

test_that("only the selected study can mutate or discard a version", {
  load_template_version_dependencies()
  con <- template_version_test_con()
  old_wd <- setwd(rids_repo_root())
  on.exit(setwd(old_wd), add = TRUE)
  run_migrations(con)

  study_id <- insert_version_test_study(con)
  DBI::dbExecute(con, "
    INSERT INTO meta_data
      (cpms_id, study_site, scenario_id, edge_id, study_name, original_filename, saved_file_path)
    VALUES ('99999', 'RDUHT', 'A', 'EDGE-2', 'Other Study', 'other.xlsx', '/tmp/other.xlsx')
  ")
  other_study_id <- DBI::dbGetQuery(con, "SELECT id FROM meta_data WHERE cpms_id = '99999'")$id[[1]]

  repo <- template_version_repository(con)
  version_id <- repo$create(
    study_id, "substantial_amendment", as.Date("2025-02-01"),
    original_filename = "amendment.xlsx", saved_file_path = "/tmp/amendment.xlsx"
  )
  expect_error(repo$set_edge_zip_path(version_id, "/tmp/version.zip", other_study_id), "selected study")
  expect_error(repo$discard(version_id, other_study_id), "selected study")
  expect_equal(repo$discard(version_id, study_id)$template_versions, 1L)
})

test_that("custom activity deletion is scoped to the selected template version", {
  load_template_version_dependencies()
  con <- template_version_test_con()
  old_wd <- setwd(rids_repo_root())
  on.exit(setwd(old_wd), add = TRUE)
  run_migrations(con)

  study_id <- insert_version_test_study(con)
  repo <- template_version_repository(con)
  baseline_id <- repo$create(
    study_id, "baseline", original_filename = "baseline.xlsx",
    saved_file_path = "/tmp/baseline.xlsx"
  )
  activate_test_version(repo, baseline_id, study_id)
  amendment_id <- repo$create(
    study_id, "substantial_amendment", as.Date("2025-02-01"),
    original_filename = "amendment.xlsx", saved_file_path = "/tmp/amendment.xlsx"
  )

  for (version_id in c(baseline_id, amendment_id)) {
    rids_dbExecute(con, "
      INSERT INTO addon_custom_activities
        (custom_activity_id, cpms_id, study_site, scenario_id, version_id,
         Study_Arm, Activity, mode, slot_num, cost_centre, amount)
      VALUES ('12345-001', '12345', 'RDUHT', 'A', ?,
              'Arm', 'Test', 'single_cc', 1, 'CC1', 10)
    ", params = list(version_id))
  }

  expect_equal(
    ca_delete(
      "12345-001", con = con, cpms_id = "12345", study_site = "RDUHT",
      scenario_id = "A", version_id = amendment_id
    ),
    1L
  )
  remaining <- rids_dbGetQuery(con, "SELECT version_id FROM addon_custom_activities")
  expect_identical(remaining$version_id[[1]], baseline_id)
})

test_that("migration backfills existing studies and child rows to a baseline", {
  load_template_version_dependencies()
  con <- template_version_test_con()
  old_wd <- setwd(rids_repo_root())
  on.exit(setwd(old_wd), add = TRUE)

  migration_dir <- withr::local_tempdir()
  old_dir <- file.path(migration_dir, "duckdb")
  dir.create(old_dir)
  existing <- list.files(
    file.path(rids_repo_root(), "R/persistence/migrations/duckdb"),
    pattern = "^000[1-6]_", full.names = TRUE
  )
  file.copy(existing, old_dir)
  run_migrations(con, migrations_dir = old_dir)

  insert_version_test_study(con)
  DBI::dbExecute(con, "INSERT INTO ict_costing_tbl (CPMS_ID, study_site, scenario_id) VALUES ('12345', 'RDUHT', 'A')")
  DBI::dbExecute(con, "INSERT INTO posting_lines (cpms_id, study_site, scenario_id) VALUES ('12345', 'RDUHT', 'A')")
  DBI::dbExecute(con, "INSERT INTO addon_custom_activities
    (custom_activity_id, cpms_id, study_site, scenario_id, Study_Arm, Activity, mode, slot_num, cost_centre, amount)
    VALUES ('12345-001', '12345', 'RDUHT', 'A', 'Arm', 'Test', 'single_cc', 1, 'CC1', 10)")

  run_migrations(con)
  baseline <- DBI::dbGetQuery(con, "SELECT * FROM template_versions")
  expect_equal(nrow(baseline), 1)
  expect_equal(baseline$version_type, "baseline")
  for (table in c("ict_costing_tbl", "posting_lines", "addon_custom_activities")) {
    row <- DBI::dbGetQuery(con, paste("SELECT version_id FROM", table))
    expect_equal(row$version_id, baseline$version_id)
  }
})
