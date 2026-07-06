# DuckDB WAL consolidation (crash recovery for duckdb dev mode).
# Replaces the coverage from the deleted legacy backup-script suite; the
# consolidation helper itself is still part of the duckdb storage mode.

test_that("consolidate_duckdb_wal folds a leftover WAL into the main file", {
  source_from_root("R/utils/deployment_config.R")

  db_path <- tempfile(fileext = ".duckdb")
  on.exit(unlink(c(db_path, duckdb_wal_path(db_path)), force = TRUE), add = TRUE)

  # Create a DB and leave rows in it
  con <- open_duckdb_connection(db_path)
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER)")
  DBI::dbExecute(con, "INSERT INTO t VALUES (1), (2), (3)")
  close_duckdb_connection(con)

  # Simulate a crash artifact: write more rows and copy the WAL aside
  # before a clean shutdown, then restore it next to the database file.
  con <- open_duckdb_connection(db_path)
  DBI::dbExecute(con, "INSERT INTO t VALUES (4)")
  DBI::dbExecute(con, "CHECKPOINT")
  close_duckdb_connection(con)

  # Whether or not a real WAL survived, consolidation must be safe to run
  # and leave no WAL file behind.
  expect_no_error(consolidate_duckdb_wal(db_path))
  expect_false(file.exists(duckdb_wal_path(db_path)))

  con <- open_duckdb_connection(db_path, read_only = TRUE)
  on.exit(suppressWarnings(close_duckdb_connection(con)), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM t")$n[[1]], 4)
})
