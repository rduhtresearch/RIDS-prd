suppressPackageStartupMessages({
  library(DBI)
})

source("R/utils/deployment_config.R")

BACKUP_ROOT <- Sys.getenv("RIDS_BACKUP_ROOT", unset = "P:/RESEARCH SYSTEMS/RIDS_BACKUP")
KEEP_BACKUP_RUNS <- 2L

fail_backup <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

ensure_directory <- function(path, label) {
  if (dir.exists(path)) {
    return(invisible(path))
  }

  ok <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(ok) && !dir.exists(path)) {
    fail_backup("Unable to create %s: %s", label, path)
  }

  invisible(path)
}

write_manifest <- function(path, lines) {
  writeLines(enc2utf8(lines), con = path, useBytes = TRUE)
  invisible(path)
}

prune_old_backups <- function(backup_root, keep_runs) {
  backup_dirs <- list.dirs(backup_root, full.names = TRUE, recursive = FALSE)
  if (!length(backup_dirs)) {
    return(invisible(character()))
  }

  backup_dirs <- backup_dirs[!grepl("_incomplete$", basename(backup_dirs))]
  if (length(backup_dirs) <= keep_runs) {
    return(invisible(character()))
  }

  info <- file.info(backup_dirs)
  ordered_dirs <- backup_dirs[order(info$mtime, decreasing = TRUE)]
  dirs_to_remove <- ordered_dirs[seq.int(keep_runs + 1L, length(ordered_dirs))]

  removed <- character()
  for (dir_path in dirs_to_remove) {
    ok <- unlink(dir_path, recursive = TRUE, force = TRUE)
    if (ok != 0L || dir.exists(dir_path)) {
      fail_backup("Failed to remove old backup folder: %s", dir_path)
    }
    removed <- c(removed, dir_path)
  }

  invisible(removed)
}

run_manual_backup <- function() {
  config <- load_runtime_config(getwd())

  if (!identical(tolower(config$storage_mode), "duckdb")) {
    fail_backup(
      "Manual backup only supports DuckDB. Current storage mode is '%s'.",
      config$storage_mode
    )
  }

  db_path <- normalizePath(config$db_dir, winslash = "/", mustWork = FALSE)
  if (!file.exists(db_path)) {
    fail_backup("Production DuckDB file was not found: %s", db_path)
  }

  # Fold any leftover write-ahead log into the main file before copying, so the
  # binary copy is complete and consistent with the CSV export taken below.
  tryCatch(
    consolidate_duckdb_wal(db_path),
    error = function(e) {
      fail_backup(
        "Could not consolidate the DuckDB write-ahead log before backup. Ensure RIDS is closed and try again. Details: %s",
        conditionMessage(e)
      )
    }
  )

  wal_path <- duckdb_wal_path(db_path)
  if (file.exists(wal_path) && isTRUE(file.info(wal_path)$size > 0)) {
    fail_backup("A non-empty write-ahead log remains after checkpoint: %s", wal_path)
  }

  ensure_directory(BACKUP_ROOT, "backup root")

  timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  backup_dir <- file.path(BACKUP_ROOT, timestamp)
  temp_backup_dir <- paste0(backup_dir, "_incomplete")
  csv_dir <- file.path(temp_backup_dir, "csv")

  if (dir.exists(backup_dir) || dir.exists(temp_backup_dir)) {
    fail_backup("Backup folder already exists for timestamp %s", timestamp)
  }

  ensure_directory(temp_backup_dir, "temporary backup folder")
  ensure_directory(csv_dir, "CSV export folder")

  backup_db_path <- file.path(temp_backup_dir, "RIDS.duckdb")
  manifest_path <- file.path(temp_backup_dir, "backup_manifest.txt")
  final_backup_db_path <- file.path(backup_dir, "RIDS.duckdb")
  final_csv_dir <- file.path(backup_dir, "csv")

  on.exit({
    if (dir.exists(temp_backup_dir)) {
      unlink(temp_backup_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  copied <- file.copy(from = db_path, to = backup_db_path, overwrite = FALSE, copy.mode = TRUE)
  if (!isTRUE(copied) || !file.exists(backup_db_path)) {
    fail_backup("Failed to copy DuckDB file to backup folder.")
  }

  backup_db_info <- file.info(backup_db_path)
  if (is.na(backup_db_info$size) || backup_db_info$size <= 0) {
    fail_backup("Copied DuckDB backup file is empty: %s", backup_db_path)
  }

  con <- tryCatch(
    open_duckdb_connection(db_path, read_only = TRUE),
    error = function(e) {
      fail_backup("Failed to open production DuckDB in read-only mode: %s", conditionMessage(e))
    }
  )
  on.exit(try(close_duckdb_connection(con), silent = TRUE), add = TRUE)

  tables <- tryCatch(
    sort(DBI::dbListTables(con)),
    error = function(e) {
      fail_backup("Failed to list tables from production DuckDB: %s", conditionMessage(e))
    }
  )

  exported_tables <- character()
  table_row_counts <- integer()

  for (table_name in tables) {
    csv_path <- file.path(csv_dir, paste0(table_name, ".csv"))
    table_data <- tryCatch(
      DBI::dbReadTable(con, table_name),
      error = function(e) {
        fail_backup("Failed to read table '%s': %s", table_name, conditionMessage(e))
      }
    )

    tryCatch(
      utils::write.csv(table_data, file = csv_path, row.names = FALSE, na = ""),
      error = function(e) {
        fail_backup("Failed to export table '%s' to CSV: %s", table_name, conditionMessage(e))
      }
    )

    exported_tables <- c(exported_tables, table_name)
    table_row_counts <- c(table_row_counts, nrow(table_data))
  }

  manifest_lines <- c(
    sprintf("backup_timestamp: %s", timestamp),
    sprintf("backup_created_at: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("source_config: %s", config$source_path),
    sprintf("source_db: %s", db_path),
    sprintf("storage_mode: %s", config$storage_mode),
    sprintf("backup_db_copy: %s", final_backup_db_path),
    sprintf("csv_export_dir: %s", final_csv_dir),
    sprintf("exported_table_count: %d", length(exported_tables)),
    "exported_tables:"
  )

  if (length(exported_tables)) {
    manifest_lines <- c(
      manifest_lines,
      sprintf("  - %s (%d rows)", exported_tables, table_row_counts)
    )
  }

  write_manifest(manifest_path, manifest_lines)

  renamed <- file.rename(temp_backup_dir, backup_dir)
  if (!isTRUE(renamed) || !dir.exists(backup_dir)) {
    fail_backup("Failed to finalize backup folder: %s", backup_dir)
  }

  removed_dirs <- prune_old_backups(BACKUP_ROOT, KEEP_BACKUP_RUNS)

  message("RIDS backup completed successfully.")
  message("Backup folder: ", backup_dir)
  message("Tables exported: ", length(exported_tables))
  if (length(removed_dirs)) {
    message("Old backups removed: ", paste(basename(removed_dirs), collapse = ", "))
  }

  invisible(backup_dir)
}

run_manual_backup()
