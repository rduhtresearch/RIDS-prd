# Runtime configuration, environment-variable first.
#
# Resolution order for every key:
#   1. RIDS_* environment variable (the containerized/hosted path)
#   2. legacy deployment_config.R file, located via the existing candidate
#      search in R/utils/deployment_config.R (the shared-drive path)
#   3. built-in default, where one is safe
#
# If all required keys are provided via environment variables, no config file
# is needed at all. Validation rules are identical to the legacy reader:
# db_dir / ict_upload_dir / edge_output_dir / credential_secret required,
# credential_secret >= 16 chars, app_status one of dev/test/live.

rids_env <- function(name, default = "") {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (nzchar(value)) value else default
}

load_app_config <- function(app_dir = getwd()) {
  file_cfg <- NULL
  candidates <- deployment_config_candidates(app_dir)
  for (candidate in candidates) {
    if (file.exists(candidate)) {
      file_cfg <- read_runtime_config(candidate)
      break
    }
  }

  from_file <- function(key, default = "") {
    if (!is.null(file_cfg) && !is.null(file_cfg[[key]])) file_cfg[[key]] else default
  }

  cfg <- list(
    storage_mode = tolower(rids_env("RIDS_STORAGE_MODE", from_file("storage_mode", "duckdb"))),
    database_url = rids_env("RIDS_DATABASE_URL", from_file("database_url")),
    db_dir = rids_env("RIDS_DB_DIR", from_file("db_dir")),
    ict_upload_dir = rids_env("RIDS_ICT_UPLOAD_DIR", from_file("ict_upload_dir")),
    edge_output_dir = rids_env("RIDS_EDGE_OUTPUT_DIR", from_file("edge_output_dir")),
    credential_secret = rids_env("RIDS_CREDENTIAL_SECRET", from_file("credential_secret")),
    app_status = tolower(rids_env("RIDS_APP_STATUS", from_file("app_status", "live"))),
    app_log_dir = rids_env("RIDS_APP_LOG_DIR", from_file("app_log_dir", file.path(app_dir, "logs"))),
    app_host = rids_env("RIDS_APP_HOST", from_file("app_host", "127.0.0.1")),
    app_port = suppressWarnings(as.integer(rids_env("RIDS_APP_PORT", from_file("app_port", 3838L)))),
    source_path = if (!is.null(file_cfg)) file_cfg$source_path else "environment"
  )

  if (!nzchar(cfg$storage_mode)) {
    cfg$storage_mode <- "duckdb"
  }

  missing_key_hint <- paste(
    "Set the RIDS_* environment variables or provide a deployment_config.R.",
    "Looked for config files in:",
    paste(candidates, collapse = ", ")
  )

  if (identical(cfg$storage_mode, "duckdb") && !nzchar(cfg$db_dir)) {
    stop("Missing database location (RIDS_DB_DIR / DB_DIR). ", missing_key_hint)
  }

  if (identical(cfg$storage_mode, "postgres") &&
      !nzchar(cfg$database_url) &&
      !nzchar(Sys.getenv("PGHOST", ""))) {
    stop(
      "Storage mode 'postgres' needs RIDS_DATABASE_URL ",
      "(postgres://user:pass@host:port/dbname) or the standard PG* ",
      "environment variables (PGHOST, PGDATABASE, PGUSER, PGPASSWORD)."
    )
  }
  if (!nzchar(cfg$ict_upload_dir)) {
    stop("Missing upload directory (RIDS_ICT_UPLOAD_DIR / ICT_UPLOAD_DIR). ", missing_key_hint)
  }
  if (!nzchar(cfg$edge_output_dir)) {
    stop("Missing output directory (RIDS_EDGE_OUTPUT_DIR / EDGE_OUTPUT_DIR). ", missing_key_hint)
  }

  cfg$credential_secret <- trimws(cfg$credential_secret)
  if (!nzchar(cfg$credential_secret)) {
    stop("Missing credential secret (RIDS_CREDENTIAL_SECRET / CREDENTIAL_SECRET). ", missing_key_hint)
  }
  if (nchar(cfg$credential_secret) < 16) {
    stop("The credential secret must be at least 16 characters.")
  }

  if (!cfg$app_status %in% c("dev", "test", "live")) {
    cfg$app_status <- "live"
  }

  if (!nzchar(cfg$app_log_dir)) {
    cfg$app_log_dir <- file.path(app_dir, "logs")
  }

  if (is.na(cfg$app_port) || cfg$app_port <= 0) {
    cfg$app_port <- 3838L
  }

  cfg
}
