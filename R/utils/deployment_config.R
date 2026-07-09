# Legacy config-file support and primary database connection management.
#
# The Windows shared-drive deployment machinery (release publishing, .bat
# launcher generation, local release caching, manual backup/restore) was
# removed in the containerization phase — see REFACTOR_LOG.md. What remains:
#   - reading (and, for tests, writing) the legacy deployment_config.R file
#     format, still supported as a fallback behind the RIDS_* environment
#     variables (R/config/runtime_config.R)
#   - opening/closing the primary database connection per storage mode
#   - DuckDB WAL consolidation (crash recovery for duckdb dev mode)

`%||%` <- get0("%||%", ifnotfound = function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(y)
  }

  x
})

deployment_config_candidates <- function(app_dir = getwd()) {
  env_path <- trimws(Sys.getenv("RIDS_CONFIG_PATH", ""))
  candidates <- c()

  if (nzchar(env_path)) {
    candidates <- c(candidates, env_path)
  }

  candidates <- c(
    candidates,
    file.path(app_dir, "shared", "deployment_config.R"),
    file.path(app_dir, "deployment", "deployment_config.R"),
    file.path(dirname(dirname(app_dir)), "shared", "deployment_config.R"),
    file.path(app_dir, "config.R")
  )

  unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
}

read_runtime_config <- function(path) {
  cfg_env <- new.env(parent = baseenv())
  tryCatch(
    sys.source(path, envir = cfg_env),
    error = function(e) {
      file_lines <- tryCatch(
        readLines(path, warn = FALSE),
        error = function(read_error) {
          sprintf("<unable to read %s: %s>", path, conditionMessage(read_error))
        }
      )

      numbered_lines <- paste(
        sprintf("%4d: %s", seq_along(file_lines), file_lines),
        collapse = "\n"
      )

      stop(
        paste0(
          "Failed to parse deployment config: ",
          normalizePath(path, winslash = "/", mustWork = FALSE),
          "\n",
          "Source contents:\n",
          numbered_lines,
          "\n",
          "Original error: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  get_value <- function(name, default = NULL) {
    if (exists(name, envir = cfg_env, inherits = FALSE)) {
      get(name, envir = cfg_env, inherits = FALSE)
    } else {
      default
    }
  }

  storage_mode <- tolower(trimws(as.character(get_value("STORAGE_MODE", "duckdb"))))
  if (!nzchar(storage_mode)) {
    storage_mode <- "duckdb"
  }

  cfg <- list(
    storage_mode = storage_mode,
    database_url = as.character(get_value("DATABASE_URL", "")),
    db_dir = as.character(get_value("DB_DIR", "")),
    ict_upload_dir = as.character(get_value("ICT_UPLOAD_DIR", "")),
    edge_output_dir = as.character(get_value("EDGE_OUTPUT_DIR", "")),
    credential_secret = as.character(get_value("CREDENTIAL_SECRET", "")),
    app_status = as.character(get_value("APP_STATUS", "live")),
    app_log_dir = as.character(get_value("APP_LOG_DIR", file.path(getwd(), "logs"))),
    app_host = as.character(get_value("APP_HOST", "127.0.0.1")),
    app_port = suppressWarnings(as.integer(get_value("APP_PORT", 3838L))),
    source_path = normalizePath(path, winslash = "/", mustWork = FALSE)
  )

  if (identical(cfg$storage_mode, "duckdb") && !nzchar(cfg$db_dir)) {
    stop("The deployment config is missing DB_DIR: ", path)
  }

  if (!nzchar(cfg$ict_upload_dir)) {
    stop("The deployment config is missing ICT_UPLOAD_DIR: ", path)
  }

  if (!nzchar(cfg$edge_output_dir)) {
    stop("The deployment config is missing EDGE_OUTPUT_DIR: ", path)
  }

  cfg$credential_secret <- trimws(cfg$credential_secret)
  if (!nzchar(cfg$credential_secret)) {
    stop("The deployment config is missing CREDENTIAL_SECRET: ", path)
  }

  if (nchar(cfg$credential_secret) < 16) {
    stop("The deployment config CREDENTIAL_SECRET must be at least 16 characters: ", path)
  }

  cfg$app_status <- tolower(trimws(cfg$app_status))
  if (!cfg$app_status %in% c("dev", "test", "live")) {
    cfg$app_status <- "live"
  }

  if (!nzchar(cfg$app_log_dir)) {
    cfg$app_log_dir <- file.path(getwd(), "logs")
  }

  if (is.na(cfg$app_port) || cfg$app_port <= 0) {
    cfg$app_port <- 3838L
  }

  cfg
}

load_runtime_config <- function(app_dir = getwd()) {
  candidates <- deployment_config_candidates(app_dir)

  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(read_runtime_config(candidate))
    }
  }

  stop(
    paste(
      "No deployment config found.",
      "Set the RIDS_* environment variables (see .env.example) or provide",
      "a deployment_config.R file. Looked in:",
      paste(candidates, collapse = ", ")
    )
  )
}

write_deployment_config <- function(path, config) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  encode_r_string <- function(value) {
    value <- as.character(value %||% "")
    value <- gsub("\\\\", "/", value)
    value <- gsub('"', '\\"', value, fixed = TRUE)
    value
  }

  encode_path <- function(value) {
    value <- as.character(value %||% "")
    if (!nzchar(value)) {
      return("")
    }

    encode_r_string(normalizePath(value, winslash = "/", mustWork = FALSE))
  }

  lines <- c(
    "# RIDS deployment config (legacy file format; RIDS_* environment",
    "# variables take precedence over every value here)",
    paste0('STORAGE_MODE   <- "', encode_r_string(config$storage_mode), '"'),
    paste0('DATABASE_URL   <- "', encode_r_string(config$database_url %||% ""), '"'),
    paste0('DB_DIR         <- "', encode_path(config$db_dir), '"'),
    paste0('ICT_UPLOAD_DIR <- "', encode_path(config$ict_upload_dir), '"'),
    paste0('EDGE_OUTPUT_DIR <- "', encode_path(config$edge_output_dir), '"'),
    paste0('CREDENTIAL_SECRET <- "', encode_r_string(config$credential_secret), '"'),
    paste0('APP_STATUS     <- "', encode_r_string(config$app_status %||% "live"), '"'),
    paste0('APP_LOG_DIR    <- "', encode_path(config$app_log_dir), '"'),
    paste0('APP_HOST       <- "', encode_r_string(config$app_host), '"'),
    paste0("APP_PORT       <- ", as.integer(config$app_port))
  )

  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

connect_primary_database <- function(config) {
  if (identical(config$storage_mode, "postgres")) {
    return(open_postgres_connection(config))
  }

  if (identical(config$storage_mode, "sqlserver")) {
    stop(
      "SQL Server support is designed for but not yet implemented. ",
      "See docs/sql-server-adapter.md for the adapter plan. ",
      "Supported storage modes: duckdb, postgres."
    )
  }

  if (!identical(config$storage_mode, "duckdb")) {
    stop("Unsupported storage mode: ", config$storage_mode,
         ". Supported: duckdb, postgres.")
  }

  open_duckdb_connection(config$db_dir)
}

open_postgres_connection <- function(config) {
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("The RPostgres package is required for storage mode 'postgres'.")
  }

  database_url <- trimws(config$database_url %||% "")
  if (nzchar(database_url)) {
    parsed <- parse_postgres_url(database_url)
    return(DBI::dbConnect(
      RPostgres::Postgres(),
      host = parsed$host,
      port = parsed$port,
      dbname = parsed$dbname,
      user = parsed$user,
      password = parsed$password,
      sslmode = parsed$sslmode
    ))
  }

  # Fall back to libpq's standard PG* environment variables
  DBI::dbConnect(RPostgres::Postgres())
}

# Parse postgres://user:pass@host:port/dbname?sslmode=... without extra deps.
parse_postgres_url <- function(url) {
  stripped <- sub("^postgres(ql)?://", "", url)

  query <- ""
  if (grepl("?", stripped, fixed = TRUE)) {
    query <- sub("^[^?]*\\?", "", stripped)
    stripped <- sub("\\?.*$", "", stripped)
  }

  auth <- ""
  hostpart <- stripped
  if (grepl("@", stripped, fixed = TRUE)) {
    auth <- sub("@[^@]*$", "", stripped)
    hostpart <- sub("^.*@", "", stripped)
  }

  user <- utils::URLdecode(sub(":.*$", "", auth))
  password <- if (grepl(":", auth, fixed = TRUE)) utils::URLdecode(sub("^[^:]*:", "", auth)) else ""

  dbname <- sub("^[^/]*/?", "", hostpart)
  hostport <- sub("/.*$", "", hostpart)
  host <- sub(":.*$", "", hostport)
  port <- if (grepl(":", hostport, fixed = TRUE)) as.integer(sub("^[^:]*:", "", hostport)) else 5432L

  sslmode <- "prefer"
  if (grepl("sslmode=", query, fixed = TRUE)) {
    sslmode <- sub("^.*sslmode=([^&]*).*$", "\\1", query)
  }

  list(
    host = host, port = port,
    dbname = if (nzchar(dbname)) dbname else "postgres",
    user = if (nzchar(user)) user else NULL,
    password = if (nzchar(password)) password else NULL,
    sslmode = sslmode
  )
}

open_duckdb_connection <- function(db_dir, read_only = FALSE, config = list()) {
  # Keep a stable reference to the driver object for older DuckDB R package
  # builds that can invalidate connections when the anonymous driver is GC'd.
  drv <- duckdb::duckdb(
    dbdir = db_dir,
    read_only = read_only,
    config = config
  )
  con <- DBI::dbConnect(drv)
  attr(con, "duckdb_driver") <- drv
  con
}

close_duckdb_connection <- function(con) {
  drv <- attr(con, "duckdb_driver", exact = TRUE)

  try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)

  if (!is.null(drv)) {
    try(suppressWarnings(duckdb::duckdb_shutdown(drv)), silent = TRUE)
  }

  invisible(TRUE)
}

close_primary_database <- function(con) {
  if (inherits(con, "PqConnection")) {
    try(DBI::dbDisconnect(con), silent = TRUE)
    return(invisible(TRUE))
  }

  close_duckdb_connection(con)
}

# Fold any leftover write-ahead log (DB.wal) back into the main DuckDB file.
#
# A WAL is left on disk whenever the app process is killed before DuckDB
# checkpoints. Opening the file read-write replays the WAL, and a clean
# shutdown (CHECKPOINT + shutdown = TRUE) folds it into the main file and
# removes the .wal. This requires exclusive access, so it errors if another
# process still holds the database rather than producing a stale copy.
duckdb_wal_path <- function(db_path) {
  paste0(db_path, ".wal")
}

consolidate_duckdb_wal <- function(db_path) {
  con <- open_duckdb_connection(db_path, read_only = FALSE)
  on.exit(try(close_duckdb_connection(con), silent = TRUE), add = TRUE)

  DBI::dbExecute(con, "CHECKPOINT")
  invisible(TRUE)
}
