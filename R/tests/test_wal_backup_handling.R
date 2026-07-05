suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

.wal_passed <- 0L
.wal_failed <- 0L

.wal_expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .wal_passed <<- .wal_passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .wal_failed <<- .wal_failed + 1L
  }
}

.wal_note <- function(label) {
  cat("  NOTE  ", label, "\n", sep = "")
}

# Create a clean DuckDB file with a `demo` table and the given ids, closing
# cleanly so no WAL is left behind.
.wal_create_clean_db <- function(path, ids) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # Start from a clean slate. A best-effort WAL-staging attempt can leave a
  # partial/invalid file here on platforms that lock open DB files (e.g. Windows),
  # which would otherwise make duckdb() fail with "not a valid DuckDB database file".
  unlink(c(path, paste0(path, ".wal")), force = TRUE)
  drv <- duckdb::duckdb(dbdir = path)
  con <- DBI::dbConnect(drv)
  DBI::dbExecute(con, "CREATE TABLE demo (id INTEGER, label VARCHAR)")
  for (id in ids) {
    DBI::dbExecute(con, sprintf("INSERT INTO demo VALUES (%d, 'r%d')", id, id))
  }
  DBI::dbDisconnect(con, shutdown = TRUE)
  try(duckdb::duckdb_shutdown(drv), silent = TRUE)
  invisible(path)
}

# Best-effort: produce a DuckDB file that has a genuine, non-empty leftover WAL
# and is NOT locked (mimicking a process that was killed before checkpointing).
# We write into a staging DB with auto-checkpoint disabled so committed data
# lives only in the WAL, copy the db + .wal pair to `path` while the staging
# instance is still open, then shut the staging instance down. Copying an open
# file can fail on some platforms (file locks); callers must tolerate FALSE and
# fall back to a clean DB.
.wal_try_leftover_wal_db <- function(path, ids) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  staging <- file.path(dirname(path), "staging.duckdb")
  staging_wal <- paste0(staging, ".wal")
  unlink(c(staging, staging_wal), force = TRUE)

  created <- tryCatch({
    drv <- duckdb::duckdb(dbdir = staging)
    con <- DBI::dbConnect(drv)
    on.exit({
      try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
      try(duckdb::duckdb_shutdown(drv), silent = TRUE)
    }, add = TRUE)

    DBI::dbExecute(con, "SET checkpoint_threshold = '1TB'")
    DBI::dbExecute(con, "CREATE TABLE demo (id INTEGER, label VARCHAR)")
    for (id in ids) {
      DBI::dbExecute(con, sprintf("INSERT INTO demo VALUES (%d, 'r%d')", id, id))
    }

    # Snapshot the open db + wal pair to the target path.
    ok_db <- file.copy(staging, path, overwrite = TRUE)
    ok_wal <- file.exists(staging_wal) &&
      file.copy(staging_wal, paste0(path, ".wal"), overwrite = TRUE)

    isTRUE(ok_db) && isTRUE(ok_wal) &&
      file.exists(paste0(path, ".wal")) &&
      isTRUE(file.info(paste0(path, ".wal"))$size > 0)
  }, error = function(e) FALSE)

  unlink(c(staging, staging_wal), force = TRUE)
  isTRUE(created)
}

.wal_count_demo <- function(path) {
  con <- open_duckdb_connection(path, read_only = TRUE)
  on.exit(try(close_duckdb_connection(con), silent = TRUE), add = TRUE)
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM demo")$n[[1]]
}

run_wal_backup_handling_tests <- function() {
  cat("\n=== WAL backup/restore handling tests ===\n\n")
  .wal_passed <<- 0L
  .wal_failed <<- 0L

  repo_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  source(file.path(repo_dir, "R/utils/deployment_config.R"), local = FALSE)

  temp_root <- tempfile("rids_wal_")
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)

  old_config <- Sys.getenv("RIDS_CONFIG_PATH", unset = NA)
  old_backup_root <- Sys.getenv("RIDS_BACKUP_ROOT", unset = NA)
  on.exit({
    if (is.na(old_config)) Sys.unsetenv("RIDS_CONFIG_PATH") else Sys.setenv(RIDS_CONFIG_PATH = old_config)
    if (is.na(old_backup_root)) Sys.unsetenv("RIDS_BACKUP_ROOT") else Sys.setenv(RIDS_BACKUP_ROOT = old_backup_root)
    unlink(temp_root, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  config_path <- file.path(temp_root, "deployment_config.R")
  db_path <- normalizePath(file.path(temp_root, "data", "RIDS.duckdb"), winslash = "/", mustWork = FALSE)
  backup_root <- normalizePath(file.path(temp_root, "backups"), winslash = "/", mustWork = FALSE)

  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(backup_root, recursive = TRUE, showWarnings = FALSE)

  config <- list(
    storage_mode = "duckdb",
    db_dir = db_path,
    ict_upload_dir = normalizePath(file.path(temp_root, "uploads"), winslash = "/", mustWork = FALSE),
    edge_output_dir = normalizePath(file.path(temp_root, "outputs"), winslash = "/", mustWork = FALSE),
    credential_secret = paste(rep("wal-test-secret", 2), collapse = "-"),
    app_log_dir = normalizePath(file.path(temp_root, "logs"), winslash = "/", mustWork = FALSE),
    app_host = "127.0.0.1",
    app_port = 3838L,
    sql_server = "",
    sql_database = "",
    sql_driver = ""
  )
  dir.create(config$ict_upload_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$edge_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$app_log_dir, recursive = TRUE, showWarnings = FALSE)
  write_deployment_config(config_path, config)

  Sys.setenv(RIDS_CONFIG_PATH = config_path)
  Sys.setenv(RIDS_BACKUP_ROOT = backup_root)

  # --- Backup: consolidates a leftover WAL into a complete, WAL-free copy ------
  source_wal_path <- duckdb_wal_path(db_path)
  unlink(c(db_path, source_wal_path), force = TRUE)

  had_leftover_wal <- .wal_try_leftover_wal_db(db_path, ids = 1:3)
  if (had_leftover_wal) {
    .wal_expect("test fixture leaves a non-empty leftover WAL beside the source DB",
                file.exists(source_wal_path) && isTRUE(file.info(source_wal_path)$size > 0))
  } else {
    .wal_note("could not stage a leftover WAL on this platform; using a clean source DB")
    .wal_create_clean_db(db_path, ids = 1:3)
  }

  backup_err <- tryCatch({
    source(file.path(repo_dir, "R/SETUP/manual_backup.R"), local = FALSE)
    NULL
  }, error = function(e) e)

  .wal_expect("manual backup completes without error", is.null(backup_err))
  .wal_expect("backup consolidates the WAL: none remains beside the source DB",
              !file.exists(source_wal_path))

  backup_dirs <- list.dirs(backup_root, full.names = TRUE, recursive = FALSE)
  backup_dirs <- backup_dirs[!grepl("_incomplete$", basename(backup_dirs)) &
                               basename(backup_dirs) != "pre_restore_safety"]
  .wal_expect("backup produced exactly one backup folder", length(backup_dirs) == 1L)

  if (length(backup_dirs) == 1L) {
    backup_db <- file.path(backup_dirs[[1]], "RIDS.duckdb")
    backup_copy_wal <- duckdb_wal_path(backup_db)
    .wal_expect("backup folder contains a RIDS.duckdb copy", file.exists(backup_db))
    .wal_expect("backup copy has no sibling WAL", !file.exists(backup_copy_wal))
    .wal_expect("backup copy exported the demo CSV",
                file.exists(file.path(backup_dirs[[1]], "csv", "demo.csv")))
    backup_rows <- tryCatch(.wal_count_demo(backup_db), error = function(e) NA_integer_)
    .wal_expect("backup copy contains all rows from the WAL (3 rows)",
                identical(as.integer(backup_rows), 3L))
  }

  # --- Restore: completes safely and leaves no stale WAL beside the live DB ----
  restore_root <- normalizePath(file.path(temp_root, "restore_backups"), winslash = "/", mustWork = FALSE)
  dir.create(restore_root, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(RIDS_BACKUP_ROOT = restore_root)

  selected_ts <- "2026-01-01_000000"
  backup_source_db <- file.path(restore_root, selected_ts, "RIDS.duckdb")
  .wal_create_clean_db(backup_source_db, ids = 1:3)

  live_db <- normalizePath(file.path(temp_root, "restore_data", "RIDS.duckdb"), winslash = "/", mustWork = FALSE)
  live_wal <- duckdb_wal_path(live_db)
  .wal_create_clean_db(live_db, ids = 1L)

  # Point the deployment config at the restore live DB.
  config$db_dir <- live_db
  write_deployment_config(config_path, config)

  restore_src <- readLines(file.path(repo_dir, "R/SETUP/manual_restore.R"))
  ts_line <- grep("^RESTORE_BACKUP_TIMESTAMP <-", restore_src)
  restore_src[ts_line] <- sprintf('RESTORE_BACKUP_TIMESTAMP <- "%s"', selected_ts)
  tmp_restore <- file.path(temp_root, "manual_restore_run.R")
  writeLines(restore_src, tmp_restore)

  restore_err <- tryCatch({
    source(tmp_restore, local = FALSE)
    NULL
  }, error = function(e) e)

  .wal_expect("manual restore completes without error", is.null(restore_err))
  .wal_expect("restore leaves no stale WAL beside the live DB", !file.exists(live_wal))

  live_rows <- tryCatch(.wal_count_demo(live_db), error = function(e) NA_integer_)
  .wal_expect("live DB holds the restored rows (3 rows)", identical(as.integer(live_rows), 3L))

  safety_root <- file.path(restore_root, "pre_restore_safety")
  safety_dirs <- list.dirs(safety_root, full.names = TRUE, recursive = FALSE)
  .wal_expect("restore created a pre-restore safety copy", length(safety_dirs) >= 1L)
  if (length(safety_dirs) >= 1L) {
    newest_safety <- safety_dirs[order(file.info(safety_dirs)$mtime, decreasing = TRUE)][[1]]
    safety_db <- file.path(newest_safety, basename(live_db))
    safety_rows <- tryCatch(.wal_count_demo(safety_db), error = function(e) NA_integer_)
    .wal_expect("pre-restore safety copy preserves the prior live data (1 row)",
                identical(as.integer(safety_rows), 1L))
  }

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .wal_passed, "    FAILED: ", .wal_failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  invisible(list(passed = .wal_passed, failed = .wal_failed))
}
