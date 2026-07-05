# ==============================================================================
# LIBRARIES
# ==============================================================================
library(DBI)
library(duckdb)
library(sodium)
library(shiny)
library(bs4Dash)
library(waiter)
library(shinyFeedback)
library(shinyjs)
library(reactable)
library(DT)
library(jsonlite)
library(zip)

# ==============================================================================
# SOURCE UTILS
# ==============================================================================
source("R/load_app.R", local = FALSE)
rids_source_utils()

# ==============================================================================
# GLOBAL CONFIGURATION & INITIALIZATION
# ==============================================================================
# Environment variables first, legacy deployment_config.R file as fallback
APP_CONFIG <- load_app_config(getwd())
CONFIG_SOURCE_PATH <- APP_CONFIG$source_path
STORAGE_MODE <- APP_CONFIG$storage_mode
DB_DIR <- APP_CONFIG$db_dir
ICT_UPLOAD_DIR <- APP_CONFIG$ict_upload_dir
EDGE_OUTPUT_DIR <- APP_CONFIG$edge_output_dir
APP_HOST <- APP_CONFIG$app_host
APP_PORT <- APP_CONFIG$app_port
APP_RUN_LOG_DIR <- APP_CONFIG$app_log_dir
CREDENTIAL_SECRET <- APP_CONFIG$credential_secret
APP_STATUS <- APP_CONFIG$app_status
APP_RUN_LOG_FILE <- initialize_app_run_logging(APP_RUN_LOG_DIR)

app_log_info("startup", "Global initialization started")

# Connect to database
CON <- connect_primary_database(APP_CONFIG)
app_log_info("startup", "Primary database connected")

# Load paths from DB settings (admin may have updated them); fall back to config values
ICT_UPLOAD_DIR_DEFAULT <- ICT_UPLOAD_DIR

ICT_UPLOAD_DIR <- tryCatch({
  val <- dbGetQuery(CON, "SELECT value FROM app_settings WHERE key = 'ict_upload_dir'")
  if (nrow(val) > 0) val$value else ICT_UPLOAD_DIR_DEFAULT
}, error = function(e) ICT_UPLOAD_DIR_DEFAULT)

EDGE_OUTPUT_DIR_DEFAULT <- EDGE_OUTPUT_DIR

EDGE_OUTPUT_DIR <- tryCatch({
  val <- dbGetQuery(CON, "SELECT value FROM app_settings WHERE key = 'edge_output_dir'")
  if (nrow(val) > 0) val$value else EDGE_OUTPUT_DIR_DEFAULT
}, error = function(e) EDGE_OUTPUT_DIR_DEFAULT)
db_main()
# ==============================================================================
# SHINY SESSION CLEANUP
# ==============================================================================
onStop(function() {
  app_log_info("shutdown", "Closing primary database")
  close_duckdb_connection(CON)
  close_app_run_logging()
})

# ==============================================================================
# Version consts
# ==============================================================================
APP_VERSION <- trimws(Sys.getenv("RIDS_APP_VERSION", "v1.0.0"))
APP_VERSION_LABEL <- if (grepl("^[vV]", APP_VERSION)) APP_VERSION else paste0("v", APP_VERSION)
APP_TITLE <- paste("RIDS", APP_VERSION_LABEL)
APP_LAST_UPDATED <- trimws(Sys.getenv("RIDS_APP_LAST_UPDATED", "2026-05-06"))
AUTH_SESSION_HOURS <- suppressWarnings(as.numeric(Sys.getenv("RIDS_AUTH_SESSION_HOURS", "10")))
if (is.na(AUTH_SESSION_HOURS) || AUTH_SESSION_HOURS <= 0) {
  AUTH_SESSION_HOURS <- 10
}
