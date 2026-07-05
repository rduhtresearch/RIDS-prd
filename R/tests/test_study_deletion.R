suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(y)
  }
  x
}

.sd_passed <- 0L
.sd_failed <- 0L

.sd_expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .sd_passed <<- .sd_passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .sd_failed <<- .sd_failed + 1L
  }
}

run_study_deletion_tests <- function() {
  cat("\n=== study deletion tests ===\n\n")
  .sd_passed <<- 0L
  .sd_failed <<- 0L

  source("R/utils/study_deletion.R")

  db_path <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
  old_con <- if (exists("CON", inherits = TRUE)) get("CON", inherits = TRUE) else NULL
  assign("CON", con, envir = .GlobalEnv)

  upload_file <- tempfile(fileext = ".xlsx")
  zip_file <- tempfile(fileext = ".zip")
  writeLines("upload", upload_file)
  writeLines("zip", zip_file)

  on.exit({
    if (is.null(old_con)) {
      rm("CON", envir = .GlobalEnv)
    } else {
      assign("CON", old_con, envir = .GlobalEnv)
    }
    dbDisconnect(con, shutdown = TRUE)
    if (file.exists(db_path)) unlink(db_path)
    if (file.exists(upload_file)) unlink(upload_file, force = TRUE)
    if (file.exists(zip_file)) unlink(zip_file, force = TRUE)
  }, add = TRUE)

  dbExecute(con, "
    CREATE TABLE meta_data (
      id INTEGER,
      cpms_id TEXT,
      study_site TEXT,
      scenario_id TEXT,
      saved_file_path TEXT,
      edge_zip_path TEXT
    )
  ")
  dbExecute(con, "
    CREATE TABLE ict_costing_tbl (
      CPMS_ID TEXT,
      study_site TEXT,
      scenario_id TEXT
    )
  ")
  dbExecute(con, "
    CREATE TABLE posting_lines (
      cpms_id TEXT,
      study_site TEXT,
      scenario_id TEXT
    )
  ")
  dbExecute(con, "
    CREATE TABLE addon_custom_activities (
      cpms_id TEXT,
      study_site TEXT,
      scenario_id TEXT
    )
  ")
  dbExecute(con, "
    CREATE TABLE app_logs (
      upload_id TEXT
    )
  ")

  dbExecute(
    con,
    "INSERT INTO meta_data (id, cpms_id, study_site, scenario_id, saved_file_path, edge_zip_path) VALUES (1, '12345', 'RDUHT', 'A', ?, ?)",
    params = list(upload_file, zip_file)
  )
  dbExecute(con, "INSERT INTO ict_costing_tbl (CPMS_ID, study_site, scenario_id) VALUES ('12345', 'RDUHT', 'A')")
  dbExecute(con, "INSERT INTO posting_lines (cpms_id, study_site, scenario_id) VALUES ('12345', 'RDUHT', 'A')")
  dbExecute(con, "INSERT INTO addon_custom_activities (cpms_id, study_site, scenario_id) VALUES ('12345', 'RDUHT', 'A')")
  dbExecute(con, "INSERT INTO app_logs (upload_id) VALUES ('1')")

  result <- delete_study_run("12345", "RDUHT", "A", con = con, delete_files = TRUE)

  .sd_expect("meta_data rows deleted", dbGetQuery(con, "SELECT COUNT(*) AS n FROM meta_data")$n[[1]] == 0L)
  .sd_expect("ict_costing_tbl rows deleted", dbGetQuery(con, "SELECT COUNT(*) AS n FROM ict_costing_tbl")$n[[1]] == 0L)
  .sd_expect("posting_lines rows deleted", dbGetQuery(con, "SELECT COUNT(*) AS n FROM posting_lines")$n[[1]] == 0L)
  .sd_expect("custom activities rows deleted", dbGetQuery(con, "SELECT COUNT(*) AS n FROM addon_custom_activities")$n[[1]] == 0L)
  .sd_expect("app log rows deleted", dbGetQuery(con, "SELECT COUNT(*) AS n FROM app_logs")$n[[1]] == 0L)
  .sd_expect("uploaded workbook deleted", !file.exists(upload_file))
  .sd_expect("zip file deleted", !file.exists(zip_file))
  .sd_expect("deleted file list includes workbook", upload_file %in% result$files$deleted)
  .sd_expect("deleted file list includes zip", zip_file %in% result$files$deleted)

  list(passed = .sd_passed, failed = .sd_failed)
}
