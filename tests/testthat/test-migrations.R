# Tests for the versioned migration runner (R/persistence/migrate.R).

migration_test_con <- function(env = parent.frame()) {
  db_path <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  withr::defer({
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(db_path, force = TRUE)
  }, envir = env)
  con
}

test_that("fresh database gets the full schema and versions are recorded", {
  source_from_root("R/persistence/migrate.R")
  con <- migration_test_con()

  old_wd <- setwd(rids_repo_root())
  on.exit(setwd(old_wd), add = TRUE)

  ran <- run_migrations(con)
  expect_true(length(ran) >= 4)

  tables <- DBI::dbListTables(con)
  expected <- c(
    "ict_costing_tbl", "meta_data", "users", "auth_sessions", "auth_audit_log",
    "user_api_credentials", "rulesets", "provider_orgs", "posting_line_types",
    "dist_rules", "amount_map", "routing_rules", "app_settings", "app_logs",
    "specialities", "posting_lines", "addon_custom_activities",
    "ref_custom_activities", "schema_migrations"
  )
  expect_length(setdiff(expected, tables), 0)
  expect_true("arm_identity" %in% tolower(DBI::dbListFields(con, "ict_costing_tbl")))
  expect_true("arm_identity" %in% tolower(DBI::dbListFields(con, "posting_lines")))
  expect_true("activity_occurrence_id" %in% tolower(DBI::dbListFields(con, "posting_lines")))

  applied <- DBI::dbGetQuery(con, "SELECT version FROM schema_migrations ORDER BY version")$version
  expect_identical(applied, sort(ran))
})

test_that("second run is a no-op", {
  source_from_root("R/persistence/migrate.R")
  con <- migration_test_con()

  old_wd <- setwd(rids_repo_root())
  on.exit(setwd(old_wd), add = TRUE)

  first <- run_migrations(con)
  second <- run_migrations(con)
  expect_true(length(first) >= 4)
  expect_length(second, 0)
})

test_that("a failing migration is rolled back and not recorded", {
  source_from_root("R/persistence/migrate.R")
  con <- migration_test_con()

  dir <- withr::local_tempdir()
  dialect_dir <- file.path(dir, "duckdb")
  dir.create(dialect_dir, recursive = TRUE)
  writeLines("CREATE TABLE good_one (id INTEGER);", file.path(dialect_dir, "0001_good.sql"))
  writeLines(c(
    "CREATE TABLE bad_partial (id INTEGER);",
    "THIS IS NOT SQL;"
  ), file.path(dialect_dir, "0002_bad.sql"))

  expect_error(run_migrations(con, migrations_dir = dialect_dir))

  tables <- DBI::dbListTables(con)
  expect_true("good_one" %in% tables)          # 0001 committed
  expect_false("bad_partial" %in% tables)      # 0002 rolled back
  applied <- DBI::dbGetQuery(con, "SELECT version FROM schema_migrations")$version
  expect_identical(applied, "0001_good")
})

test_that("migrations run in filename order", {
  source_from_root("R/persistence/migrate.R")
  con <- migration_test_con()

  dir <- withr::local_tempdir()
  dialect_dir <- file.path(dir, "duckdb")
  dir.create(dialect_dir, recursive = TRUE)
  writeLines("CREATE TABLE t1 (id INTEGER);", file.path(dialect_dir, "0001_first.sql"))
  writeLines("INSERT INTO t1 VALUES (42);", file.path(dialect_dir, "0002_second.sql"))
  writeLines(c(
    "migrate <- function(con) {",
    "  n <- DBI::dbGetQuery(con, 'SELECT COUNT(*) AS n FROM t1')$n[[1]]",
    "  stopifnot(n == 1)",
    "  DBI::dbExecute(con, 'CREATE TABLE t2 (id INTEGER);')",
    "}"
  ), file.path(dialect_dir, "0003_third.R"))

  ran <- run_migrations(con, migrations_dir = dialect_dir)
  expect_identical(ran, c("0001_first", "0002_second", "0003_third"))
  expect_true("t2" %in% DBI::dbListTables(con))
})
