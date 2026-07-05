# Smoke test: the full app startup path (R/setup.r + global.R) runs cleanly
# in a fresh R process against a throwaway config/DB. This exercises the
# entire source() chain, config loading, DB connection, and db_main() schema
# bootstrap — anything the app needs before serving its first request.
# Runs twice: once via a legacy deployment_config.R file, once via pure
# RIDS_* environment variables (the containerized path).

startup_smoke_dirs <- function(temp_root) {
  dirs <- list(
    db_dir = file.path(temp_root, "data", "RIDS.duckdb"),
    ict_upload_dir = file.path(temp_root, "uploads"),
    edge_output_dir = file.path(temp_root, "outputs"),
    app_log_dir = file.path(temp_root, "logs")
  )
  for (d in c(dirname(dirs$db_dir), dirs$ict_upload_dir,
              dirs$edge_output_dir, dirs$app_log_dir)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  dirs
}

run_startup_smoke <- function(temp_root, env) {
  root <- rids_repo_root()
  script <- file.path(temp_root, "startup_check.R")
  writeLines(c(
    sprintf('setwd("%s")', root),
    'source("R/setup.r")',
    'source("global.R")',
    'stopifnot(DBI::dbIsValid(CON))',
    'tables <- DBI::dbGetQuery(CON, "SELECT table_name FROM information_schema.tables")$table_name',
    'required <- c("users", "auth_sessions", "auth_audit_log", "meta_data",',
    '              "ict_costing_tbl", "posting_lines", "app_settings", "app_logs")',
    'missing <- setdiff(required, tables)',
    'if (length(missing) > 0) stop("missing tables after startup: ", paste(missing, collapse = ", "))',
    'close_duckdb_connection(CON)',
    'cat("STARTUP_OK\\n")'
  ), script)

  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), script,
    stdout = TRUE, stderr = TRUE,
    env = env
  ))
  status <- attr(output, "status") %||% 0L

  testthat::expect_equal(
    status, 0L,
    label = paste0("startup script exit status (output tail: ",
                   paste(utils::tail(output, 8), collapse = " | "), ")")
  )
  testthat::expect_true(any(grepl("STARTUP_OK", output, fixed = TRUE)))
}

test_that("app startup works via a legacy deployment_config.R file", {
  temp_root <- withr::local_tempdir("rids_startup_file_")
  dirs <- startup_smoke_dirs(temp_root)

  config <- c(
    list(
      storage_mode = "duckdb",
      credential_secret = "startup-smoke-secret-startup-smoke-secret",
      app_host = "127.0.0.1",
      app_port = 3838L
    ),
    dirs
  )
  config_path <- file.path(temp_root, "deployment_config.R")
  write_deployment_config(config_path, config)

  run_startup_smoke(temp_root, env = c(sprintf("RIDS_CONFIG_PATH=%s", config_path)))
})

test_that("app startup works via RIDS_* environment variables only", {
  temp_root <- withr::local_tempdir("rids_startup_env_")
  dirs <- startup_smoke_dirs(temp_root)

  run_startup_smoke(temp_root, env = c(
    sprintf("RIDS_DB_DIR=%s", dirs$db_dir),
    sprintf("RIDS_ICT_UPLOAD_DIR=%s", dirs$ict_upload_dir),
    sprintf("RIDS_EDGE_OUTPUT_DIR=%s", dirs$edge_output_dir),
    sprintf("RIDS_APP_LOG_DIR=%s", dirs$app_log_dir),
    "RIDS_CREDENTIAL_SECRET=startup-smoke-secret-startup-smoke-secret",
    "RIDS_STORAGE_MODE=duckdb",
    "RIDS_APP_STATUS=dev",
    # ensure no config file from the outer environment leaks in
    "RIDS_CONFIG_PATH=/nonexistent/deployment_config.R"
  ))
})
