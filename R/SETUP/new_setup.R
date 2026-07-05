# ==============================================================================
# RIDS - Shared Drive Setup
# ==============================================================================
# Run this script from the shared app folder before anyone starts using RIDS.
#
# What it does:
# 1. Installs any missing R packages
# 2. Creates the shared runtime and releases folders if needed
# 3. Creates or updates the shared deployment config
# 4. Creates the central Windows launcher and preparation scripts
# 5. Initialises the central DuckDB database if needed
# 6. Bootstraps the first active release automatically
# ==============================================================================

source("R/dependencies.R")
source("R/utils/deployment_config.R")
source("R/utils/release_management.R")

# ------------------------------------------------------------------------------
# 1. Shared deployment settings
# Edit these values, then run the whole script.
# ------------------------------------------------------------------------------
APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
DEPLOYMENT_DIR <- file.path(APP_DIR, "deployment")
RELEASES_DIR <- file.path(APP_DIR, "releases")
SHARED_DIR <- file.path(APP_DIR, "shared")

DB_DIR <- file.path(SHARED_DIR, "data", "RIDS.duckdb")
ICT_UPLOAD_DIR <- file.path(SHARED_DIR, "uploads")
EDGE_OUTPUT_DIR <- file.path(SHARED_DIR, "outputs")
APP_LOG_DIR <- file.path(SHARED_DIR, "logs")

APP_HOST <- "127.0.0.1"
APP_PORT <- 3838L
APP_STATUS <- "live"

# Reserved for a future SQL Server migration.
SQL_SERVER <- ""
SQL_DATABASE <- ""
SQL_DRIVER <- ""

existing_credential_secret <- ""
if (file.exists(file.path(SHARED_DIR, "deployment_config.R"))) {
  existing_cfg_env <- new.env(parent = baseenv())
  try(sys.source(file.path(SHARED_DIR, "deployment_config.R"), envir = existing_cfg_env), silent = TRUE)
  if (exists("CREDENTIAL_SECRET", envir = existing_cfg_env, inherits = FALSE)) {
    existing_credential_secret <- trimws(as.character(get("CREDENTIAL_SECRET", envir = existing_cfg_env)))
  }
}

CREDENTIAL_SECRET <- if (nzchar(existing_credential_secret)) {
  existing_credential_secret
} else {
  sodium::bin2hex(sodium::random(32))
}

# ------------------------------------------------------------------------------
# 2. Validation helpers
# ------------------------------------------------------------------------------
must_be_present <- function(path, label) {
  if (!nzchar(trimws(path))) {
    stop(label, " is required.")
  }
}

upsert_setting <- function(con, key, value) {
  existing <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM app_settings WHERE key = ?",
    params = list(key)
  )$n[[1]]

  if (existing > 0) {
    dbExecute(
      con,
      "UPDATE app_settings SET value = ? WHERE key = ?",
      params = list(value, key)
    )
  } else {
    dbExecute(
      con,
      "INSERT INTO app_settings (key, value) VALUES (?, ?)",
      params = list(key, value)
    )
  }
}

# ------------------------------------------------------------------------------
# 3. Validate the shared app folder and requested paths
# ------------------------------------------------------------------------------
must_be_present(APP_DIR, "APP_DIR")
must_be_present(DB_DIR, "DB_DIR")
must_be_present(ICT_UPLOAD_DIR, "ICT_UPLOAD_DIR")
must_be_present(EDGE_OUTPUT_DIR, "EDGE_OUTPUT_DIR")
must_be_present(APP_LOG_DIR, "APP_LOG_DIR")
ensure_required_app_files(APP_DIR)

deployment_config_path <- file.path(SHARED_DIR, "deployment_config.R")
current_release_path <- file.path(SHARED_DIR, "current_release.txt")
deploy_log_path <- file.path(SHARED_DIR, "deploy_log.tsv")
launcher_r_path <- file.path(DEPLOYMENT_DIR, "launch_app.R")
prep_r_path <- file.path(DEPLOYMENT_DIR, "prepare_app.R")
launcher_bat_path <- file.path(DEPLOYMENT_DIR, "Launch RIDS.bat")
prepare_bat_path <- file.path(DEPLOYMENT_DIR, "Prepare RIDS.bat")

dir.create(DEPLOYMENT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RELEASES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SHARED_DIR, recursive = TRUE, showWarnings = FALSE)

dirs_to_create <- c(dirname(DB_DIR), ICT_UPLOAD_DIR, EDGE_OUTPUT_DIR, APP_LOG_DIR)
for (path in dirs_to_create) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    message("Created folder: ", path)
  } else {
    message("Folder already exists: ", path)
  }
}

# ------------------------------------------------------------------------------
# 4. Write the shared deployment config and launcher files
# ------------------------------------------------------------------------------
config <- list(
  storage_mode = "duckdb",
  db_dir = normalizePath(DB_DIR, winslash = "/", mustWork = FALSE),
  ict_upload_dir = normalizePath(ICT_UPLOAD_DIR, winslash = "/", mustWork = FALSE),
  edge_output_dir = normalizePath(EDGE_OUTPUT_DIR, winslash = "/", mustWork = FALSE),
  credential_secret = CREDENTIAL_SECRET,
  app_status = APP_STATUS,
  app_log_dir = normalizePath(APP_LOG_DIR, winslash = "/", mustWork = FALSE),
  app_host = APP_HOST,
  app_port = as.integer(APP_PORT),
  sql_server = SQL_SERVER,
  sql_database = SQL_DATABASE,
  sql_driver = SQL_DRIVER
)

write_deployment_config(deployment_config_path, config)
write_launcher_r_script(
  path = launcher_r_path,
  releases_dir = RELEASES_DIR,
  current_release_path = current_release_path,
  config_path = deployment_config_path,
  app_host = APP_HOST,
  app_port = APP_PORT
)
write_prepare_r_script(
  path = prep_r_path,
  releases_dir = RELEASES_DIR,
  current_release_path = current_release_path
)
write_launcher_bat(
  path = launcher_bat_path,
  launcher_r_path = launcher_r_path,
  app_port = APP_PORT
)
write_prepare_bat(
  path = prepare_bat_path,
  prep_r_path = prep_r_path
)

message("Deployment config written: ", deployment_config_path)
message("Launcher created: ", launcher_bat_path)
message("Preparation launcher created: ", prepare_bat_path)

# ------------------------------------------------------------------------------
# 5. Initialise or refresh the central database safely
# ------------------------------------------------------------------------------
library(DBI)
library(duckdb)

db_already_exists <- file.exists(DB_DIR)

run_database_setup <- function(db_dir, config) {
  setup_env <- new.env(parent = globalenv())
  sys.source("R/utils/deployment_config.R", envir = setup_env)
  sys.source("R/utils/auth.r", envir = setup_env)
  sys.source("R/addons/custom_activities/ca_schema.R", envir = setup_env)
  sys.source("R/addons/custom_activities/ca_ref_activities.R", envir = setup_env)
  sys.source("R/setup.r", envir = setup_env)

  con <- setup_env$open_duckdb_connection(db_dir)
  on.exit(try(setup_env$close_duckdb_connection(con), silent = TRUE), add = TRUE)

  is_ready <- tryCatch({
    DBI::dbIsValid(con)
  }, error = function(e) FALSE)

  if (!isTRUE(is_ready)) {
    stop("DuckDB connection was not valid immediately after opening.")
  }

  tryCatch(
    DBI::dbGetQuery(con, "SELECT 1"),
    error = function(e) {
      stop("DuckDB connection test failed before schema setup: ", conditionMessage(e))
    }
  )

  setup_env$CON <- con
  setup_env$ict_table()
  setup_env$meta_table()
  setup_env$init_db()
  setup_env$user_tables()
  setup_env$build_rules_tables()
  setup_env$settings_table()
  setup_env$app_logs_table()
  setup_env$specialities_table()
  setup_env$posting_lines_table()
  setup_env$ca_init_table()
  setup_env$ca_init_ref_activities()

  upsert_setting(con, "ict_upload_dir", config$ict_upload_dir)
  upsert_setting(con, "edge_output_dir", config$edge_output_dir)
  upsert_setting(con, "app_log_dir", config$app_log_dir)

  invisible(TRUE)
}

run_database_setup(DB_DIR, config)

if (db_already_exists) {
  message("Existing database found. Schema and settings have been checked.")
} else {
  message("New database created and initialised.")
}

# ------------------------------------------------------------------------------
# 6. Bootstrap the first release from the current working tree
# ------------------------------------------------------------------------------
current_release <- read_release_pointer(current_release_path)

if (!nzchar(current_release)) {
  bootstrap_version <- default_bootstrap_release_version()
  bootstrap_release_dir <- file.path(RELEASES_DIR, bootstrap_version)

  export_working_tree_snapshot(APP_DIR, bootstrap_release_dir, overwrite = TRUE)
  run_release_smoke_check(bootstrap_release_dir, deployment_config_path)
  write_release_pointer(current_release_path, bootstrap_version)
  append_deploy_log(
    deploy_log_path,
    action = "bootstrap",
    version = bootstrap_version,
    status = "success",
    message = "Initial release created from the current working tree during setup."
  )
  message("Initial release bootstrapped from the current working tree: ", bootstrap_version)
} else {
  message("Current active release already set to: ", current_release)
}

# ------------------------------------------------------------------------------
# 7. Finish
# ------------------------------------------------------------------------------
message("")
message("Setup complete.")
message("")
message("Next steps:")
message("1. Open: ", launcher_bat_path)
message("2. Wait for the browser to open.")
message("3. On first launch, create the first admin account in the app.")
message("4. On each Windows laptop, run deployment/Prepare RIDS.bat once before daily use.")
message("5. Later, publish a new manual release with R/SETUP/release_publish.R publish-local --version vX.Y.Z if needed.")
