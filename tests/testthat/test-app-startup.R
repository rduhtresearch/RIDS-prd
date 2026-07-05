# Smoke test: the full app startup path (R/setup.r + global.R) runs cleanly
# in a fresh R process against a throwaway config/DB. This exercises the
# entire source() chain, config loading, DB connection, and db_main() schema
# bootstrap — anything the app needs before serving its first request.

test_that("app startup chain sources cleanly and bootstraps the schema", {
  root <- rids_repo_root()
  temp_root <- tempfile("rids_startup_smoke_")
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)

  config <- list(
    storage_mode = "duckdb",
    db_dir = file.path(temp_root, "data", "RIDS.duckdb"),
    ict_upload_dir = file.path(temp_root, "uploads"),
    edge_output_dir = file.path(temp_root, "outputs"),
    credential_secret = "startup-smoke-secret-startup-smoke-secret",
    app_log_dir = file.path(temp_root, "logs"),
    app_host = "127.0.0.1",
    app_port = 3838L,
    sql_server = "",
    sql_database = "",
    sql_driver = ""
  )
  for (d in c(dirname(config$db_dir), config$ict_upload_dir,
              config$edge_output_dir, config$app_log_dir)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  config_path <- file.path(temp_root, "deployment_config.R")
  write_deployment_config(config_path, config)

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
    env = c(sprintf("RIDS_CONFIG_PATH=%s", config_path))
  ))
  status <- attr(output, "status") %||% 0L

  expect_equal(
    status, 0L,
    label = paste0("startup script exit status (output tail: ",
                   paste(utils::tail(output, 8), collapse = " | "), ")")
  )
  expect_true(any(grepl("STARTUP_OK", output, fixed = TRUE)))
})
