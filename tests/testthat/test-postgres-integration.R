# PostgreSQL integration tests. Skipped unless RIDS_TEST_PG_URL points at a
# disposable PostgreSQL database (its public schema is dropped and
# recreated). Run in CI/dev with e.g.:
#   RIDS_TEST_PG_URL=postgres://rids:rids@127.0.0.1:5432/rids_test

pg_test_url <- function() {
  trimws(Sys.getenv("RIDS_TEST_PG_URL", ""))
}

pg_test_con <- function(env = parent.frame()) {
  url <- pg_test_url()
  if (!nzchar(url)) {
    testthat::skip("RIDS_TEST_PG_URL not set; skipping PostgreSQL integration tests")
  }
  testthat::skip_if_not_installed("RPostgres")

  source_from_root("R/utils/deployment_config.R")
  source_from_root("R/utils/auth.r")
  source_from_root("R/utils/user_credentials.R")
  source_from_root("R/auth/mfa.R")

  con <- open_postgres_connection(list(database_url = url))
  DBI::dbExecute(con, "DROP SCHEMA public CASCADE")
  DBI::dbExecute(con, "CREATE SCHEMA public")

  old_con <- if (exists("CON", inherits = TRUE)) get("CON", inherits = TRUE) else NULL
  assign("CON", con, envir = .GlobalEnv)
  assign("app_log_exception", function(...) NULL, envir = .GlobalEnv)
  assign("log_event", function(...) NULL, envir = .GlobalEnv)
  assign("CREDENTIAL_SECRET", "pg-integration-secret-value", envir = .GlobalEnv)

  withr::defer({
    if (is.null(old_con)) {
      if (exists("CON", envir = .GlobalEnv, inherits = FALSE)) rm("CON", envir = .GlobalEnv)
    } else {
      assign("CON", old_con, envir = .GlobalEnv)
    }
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, envir = env)

  old_wd <- setwd(rids_repo_root())
  withr::defer(setwd(old_wd), envir = env)
  run_migrations(con)

  con
}

test_that("postgres schema is structurally equivalent to duckdb", {
  con <- pg_test_con()

  duck_path <- tempfile(fileext = ".duckdb")
  duck <- DBI::dbConnect(duckdb::duckdb(), dbdir = duck_path)
  on.exit({
    DBI::dbDisconnect(duck, shutdown = TRUE)
    unlink(duck_path, force = TRUE)
  }, add = TRUE)
  run_migrations(duck)

  app_tables <- function(x) sort(setdiff(tolower(x), "schema_migrations"))
  duck_tables <- app_tables(DBI::dbListTables(duck))
  pg_tables <- app_tables(DBI::dbListTables(con))
  expect_identical(pg_tables, duck_tables)

  for (tbl in duck_tables) {
    duck_cols <- sort(tolower(DBI::dbListFields(duck, tbl)))
    pg_cols <- sort(tolower(DBI::dbListFields(con, tbl)))
    expect_identical(pg_cols, duck_cols, label = sprintf("columns of %s", tbl))
  }
})

test_that("auth flow works end-to-end on postgres", {
  con <- pg_test_con()

  created <- create_user_account(
    name = "PG User", username = "pg.user",
    temporary_password = "PgPass12345", active = TRUE
  )
  expect_true(created$success)
  uid <- created$user$user_id[[1]]

  expect_true(authenticate_user("pg.user", "PgPass12345")$success)
  expect_identical(authenticate_user("pg.user", "wrong")$reason, "invalid_credentials")

  sess <- create_auth_session(uid, user_agent = "pg-test", duration_hours = 1)
  restored <- restore_auth_session(sess$token)
  expect_true(restored$success)
  expect_identical(restored$session$username[[1]], "pg.user")
  revoke_auth_session(sess$session_id)
  expect_identical(restore_auth_session(sess$token)$status, "revoked")

  # MFA enroll + challenge + reset on postgres
  enrollment <- start_mfa_enrollment(uid, "pg.user")
  confirmed <- confirm_mfa_enrollment(
    uid, totp_code_for_step(enrollment$secret, totp_current_step())
  )
  expect_true(confirmed$success)

  code <- totp_code_for_step(enrollment$secret, totp_current_step() + 1)
  reset <- reset_user_password_with_mfa("pg.user", code, "NewPgPass99")
  expect_true(reset$success)
  expect_true(authenticate_user("pg.user", "NewPgPass99")$success)
})

test_that("study/ict/posting round-trips keep canonical column names on postgres", {
  con <- pg_test_con()
  repos <- build_repositories(con)

  repos$studies$insert_meta(
    cpms_id = "77001", study_site = "RDUHT", scenario_id = "A",
    edge_id = "E1", study_name = "PG Study", notes = NA_character_,
    uploaded_by = "pg.user", original_filename = "x.xlsx",
    saved_file_path = "/tmp/x.xlsx", speciality_id = 1L,
    mff_split_enabled = FALSE, mff_split_pct = 0
  )
  expect_true(repos$studies$exists_run("77001", "RDUHT", "A"))
  study_id <- repos$studies$last_upload_id()
  expect_true(is.numeric(study_id) || is.integer(study_id))
  version_id <- repos$template_versions$create(
    study_id, "baseline", original_filename = "x.xlsx", saved_file_path = "/tmp/x.xlsx"
  )
  repos$template_versions$set_edge_zip_path(version_id, "/tmp/x.zip", study_id)
  repos$template_versions$activate(version_id, study_id)

  meta <- repos$studies$find_meta("77001", "RDUHT", "A")
  expect_equal(nrow(meta), 1)
  expect_identical(meta$study_name[[1]], "PG Study")

  ict_df <- data.frame(
    CPMS_ID = "77001", study_site = "RDUHT", scenario_id = "A",
    Study = "PG Study", Visit_Number = "V1", Study_Arm = "Arm A",
    Visit_Label = "Screening", Activity_Name = "Blood Test",
    ICT_Cost = 100, Contract_Cost = 120,
    activity_occurrence_id = 1L, staff_group = 1L,
    stringsAsFactors = FALSE
  )
  repos$ict_costing$replace_run(ict_df, "77001", "RDUHT", "A", version_id)
  fetched <- repos$ict_costing$find_by_run("77001", "RDUHT", "A", version_id)
  expect_identical(
    names(fetched),
    RIDS_CANONICAL_COLUMNS$ict_costing_tbl
  )
  expect_identical(fetched$Activity_Name[[1]], "Blood Test")

  visit <- repos$ict_costing$visit_lookup("77001", "RDUHT", "A", version_id)
  expect_true(all(c("Study", "Study_Arm", "Visit_Label", "Visit_Number") %in% names(visit)))

  pl_df <- data.frame(
    row_id = 1L, scenario_id = "A", cpms_id = "77001", study_site = "RDUHT",
    study_name = "PG Study", Study_Arm = "Arm A", Activity = "Blood Test",
    Visit = "V1", posting_line_type_id = "DIRECT", posting_amount = 100,
    Visit_Label = "Screening", staff_group = 1L,
    stringsAsFactors = FALSE
  )
  repos$posting_lines$replace_for_run(pl_df, "77001", "RDUHT", "A", version_id)
  expect_equal(repos$posting_lines$count_for_run("77001", "RDUHT", "A", version_id), 1)
  pl_back <- repos$posting_lines$find_by_run("77001", "RDUHT", "A", version_id)
  expect_true(all(c("Study_Arm", "Activity", "Visit", "Visit_Label") %in% names(pl_back)))

  # settings + rules reads
  repos$settings$set("pg_check", "1")
  expect_identical(repos$settings$find_value("pg_check"), "1")

  # cascade delete
  counts <- repos$studies$delete_run("77001", "RDUHT", "A")
  expect_equal(counts$meta_data, 1)
  expect_equal(counts$posting_lines, 1)
  expect_equal(counts$ict_costing_tbl, 1)
  expect_false(repos$studies$exists_run("77001", "RDUHT", "A"))
})

test_that("stage A ICT persistence uses repositories in postgres mode", {
  con <- pg_test_con()
  repos <- build_repositories(con)

  source_from_root("R/utils/pipeline_fixed.r")
  assign("STORAGE_MODE", "postgres", envir = .GlobalEnv)

  repos$studies$insert_meta(
    cpms_id = "88001", study_site = "RDUHT", scenario_id = "A",
    edge_id = "E2", study_name = "PG Stage A", notes = NA_character_,
    uploaded_by = "pg.user", original_filename = "stage-a.xlsx",
    saved_file_path = "/tmp/stage-a.xlsx", speciality_id = 1L,
    mff_split_enabled = FALSE, mff_split_pct = 0
  )
  study_id <- repos$studies$last_upload_id()
  version_id <- repos$template_versions$create(
    study_id, "baseline", original_filename = "stage-a.xlsx",
    saved_file_path = "/tmp/stage-a.xlsx"
  )

  ict_df <- data.frame(
    CPMS_ID = "88001", study_site = "RDUHT", scenario_id = "A",
    Study = "PG Stage A", Visit_Number = "V1", Study_Arm = "Arm A",
    Visit_Label = "Screening", Activity_Name = "Blood Test",
    ICT_Cost = 100, activity_occurrence_id = 1L, staff_group = 1L,
    stringsAsFactors = FALSE
  )

  expect_no_error(
    persist_ict_to_duckdb("/definitely/not/a/duckdb/file.duckdb", ict_df, version_id)
  )

  fetched <- repos$ict_costing$find_by_run("88001", "RDUHT", "A", version_id)
  expect_equal(nrow(fetched), 1)
  expect_identical(fetched$Activity_Name[[1]], "Blood Test")
})

test_that("seed functions run cleanly on postgres", {
  con <- pg_test_con()

  assign("ICT_UPLOAD_DIR", tempdir(), envir = .GlobalEnv)
  assign("EDGE_OUTPUT_DIR", tempdir(), envir = .GlobalEnv)
  on.exit({
    rm("ICT_UPLOAD_DIR", envir = .GlobalEnv)
    rm("EDGE_OUTPUT_DIR", envir = .GlobalEnv)
  }, add = TRUE)

  source_from_root("R/setup.r")
  expect_no_error(db_main())

  repos <- build_repositories(con)
  expect_true(length(repos$rules$posting_line_type_ids()) >= 8)
  bundle <- repos$rules$ruleset_bundle("COMM_AH_V1")
  expect_true(nrow(bundle$dist_rules) > 0)
  expect_true(nrow(bundle$routing_rules) > 0)
  expect_true(nrow(repos$specialities$list_active()) >= 13)

  # seeds are idempotent
  expect_no_error(db_main())
})
