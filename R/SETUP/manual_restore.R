suppressPackageStartupMessages({
  library(DBI)
})

source("R/utils/deployment_config.R")

RESTORE_BACKUP_TIMESTAMP <- ""
BACKUP_ROOT <- Sys.getenv("RIDS_BACKUP_ROOT", unset = "P:/RESEARCH SYSTEMS/RIDS_BACKUP")
SAFETY_ROOT_NAME <- "pre_restore_safety"

fail_restore <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

ensure_directory <- function(path, label) {
  if (dir.exists(path)) {
    return(invisible(path))
  }

  ok <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(ok) && !dir.exists(path)) {
    fail_restore("Unable to create %s: %s", label, path)
  }

  invisible(path)
}

write_restore_manifest <- function(path, lines) {
  writeLines(enc2utf8(lines), con = path, useBytes = TRUE)
  invisible(path)
}

list_available_backups <- function(backup_root) {
  backup_dirs <- list.dirs(backup_root, full.names = TRUE, recursive = FALSE)
  if (!length(backup_dirs)) {
    return(character())
  }

  keep <- !grepl("_incomplete$", basename(backup_dirs)) &
    basename(backup_dirs) != SAFETY_ROOT_NAME &
    file.exists(file.path(backup_dirs, "RIDS.duckdb"))

  sort(basename(backup_dirs[keep]), decreasing = TRUE)
}

run_manual_restore <- function() {
  config <- load_runtime_config(getwd())

  if (!identical(tolower(config$storage_mode), "duckdb")) {
    fail_restore(
      "Manual restore only supports DuckDB. Current storage mode is '%s'.",
      config$storage_mode
    )
  }

  if (!dir.exists(BACKUP_ROOT)) {
    fail_restore("Backup root was not found: %s", BACKUP_ROOT)
  }

  available_backups <- list_available_backups(BACKUP_ROOT)
  if (!length(available_backups)) {
    fail_restore("No backup folders containing RIDS.duckdb were found in: %s", BACKUP_ROOT)
  }

  selected_backup <- trimws(RESTORE_BACKUP_TIMESTAMP)
  if (!nzchar(selected_backup)) {
    fail_restore(
      paste(
        "RESTORE_BACKUP_TIMESTAMP is blank.",
        "Set it at the top of R/SETUP/manual_restore.R to one of:",
        paste(available_backups, collapse = ", ")
      )
    )
  }

  if (!selected_backup %in% available_backups) {
    fail_restore(
      paste(
        sprintf("Backup timestamp '%s' was not found.", selected_backup),
        "Available backups:",
        paste(available_backups, collapse = ", ")
      )
    )
  }

  db_path <- normalizePath(config$db_dir, winslash = "/", mustWork = FALSE)
  if (!file.exists(db_path)) {
    fail_restore("Production DuckDB file was not found: %s", db_path)
  }

  # Fold any leftover write-ahead log into the live file before copying it, so the
  # pre-restore safety copy below captures the full current state.
  tryCatch(
    consolidate_duckdb_wal(db_path),
    error = function(e) {
      fail_restore(
        "Could not consolidate the live DuckDB write-ahead log before restore. Ensure RIDS is closed and try again. Details: %s",
        conditionMessage(e)
      )
    }
  )

  live_wal_path <- duckdb_wal_path(db_path)

  source_backup_dir <- file.path(BACKUP_ROOT, selected_backup)
  source_backup_db <- file.path(source_backup_dir, "RIDS.duckdb")
  if (!file.exists(source_backup_db)) {
    fail_restore("Backup DuckDB file was not found: %s", source_backup_db)
  }

  safety_root <- file.path(BACKUP_ROOT, SAFETY_ROOT_NAME)
  ensure_directory(safety_root, "pre-restore safety root")

  restore_timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  safety_dir <- file.path(safety_root, restore_timestamp)
  ensure_directory(safety_dir, "pre-restore safety folder")

  safety_db_path <- file.path(safety_dir, basename(db_path))
  restore_manifest_path <- file.path(safety_dir, "restore_manifest.txt")

  copied_live <- file.copy(from = db_path, to = safety_db_path, overwrite = FALSE, copy.mode = TRUE)
  if (!isTRUE(copied_live) || !file.exists(safety_db_path)) {
    fail_restore("Failed to create the pre-restore safety copy: %s", safety_db_path)
  }

  safety_info <- file.info(safety_db_path)
  if (is.na(safety_info$size) || safety_info$size <= 0) {
    fail_restore("The pre-restore safety copy is empty: %s", safety_db_path)
  }

  copied_backup <- file.copy(from = source_backup_db, to = db_path, overwrite = TRUE, copy.mode = TRUE)
  if (!isTRUE(copied_backup) || !file.exists(db_path)) {
    fail_restore("Failed to restore backup into the live DB path: %s", db_path)
  }

  # Remove any stale write-ahead log left beside the live DB. Left in place it
  # would belong to the previous database file and corrupt the restored one on
  # the next open.
  if (file.exists(live_wal_path)) {
    unlink(live_wal_path, force = TRUE)
    if (file.exists(live_wal_path)) {
      fail_restore("Failed to remove stale write-ahead log: %s", live_wal_path)
    }
  }

  live_con <- tryCatch(
    open_duckdb_connection(db_path, read_only = TRUE),
    error = function(e) {
      fail_restore("Restored DuckDB file could not be opened: %s", conditionMessage(e))
    }
  )
  on.exit(try(close_duckdb_connection(live_con), silent = TRUE), add = TRUE)

  restored_tables <- tryCatch(
    sort(DBI::dbListTables(live_con)),
    error = function(e) {
      fail_restore("Restored DuckDB file opened but table listing failed: %s", conditionMessage(e))
    }
  )

  manifest_lines <- c(
    sprintf("restore_timestamp: %s", restore_timestamp),
    sprintf("restore_created_at: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("source_config: %s", config$source_path),
    sprintf("selected_backup_timestamp: %s", selected_backup),
    sprintf("source_backup_dir: %s", source_backup_dir),
    sprintf("source_backup_db: %s", source_backup_db),
    sprintf("live_db_path: %s", db_path),
    sprintf("pre_restore_safety_copy: %s", safety_db_path),
    sprintf("restored_table_count: %d", length(restored_tables)),
    "restored_tables:"
  )

  if (length(restored_tables)) {
    manifest_lines <- c(
      manifest_lines,
      sprintf("  - %s", restored_tables)
    )
  }

  write_restore_manifest(restore_manifest_path, manifest_lines)

  message("RIDS restore completed successfully.")
  message("Restored backup: ", selected_backup)
  message("Live DB path: ", db_path)
  message("Safety copy saved to: ", safety_db_path)
  message("Tables found after restore: ", length(restored_tables))

  invisible(db_path)
}

run_manual_restore()
