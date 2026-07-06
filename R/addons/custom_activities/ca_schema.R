# ==============================================================================
# R/addons/custom_activities/ca_schema.R
#
# Addon table: addon_custom_activities.
#
# Stores user-entered custom activity inputs (NOT the derived posting lines).
# One row per (custom_activity, slot). At export time, ca_load() reads this
# table and ca_build_custom_rows() derives the posting_lines-shaped rows.
#
# Source-of-truth:
#   - addon_custom_activities  →  the user's input (this table)
#   - posting_lines            →  derived at export, includes custom rows
#
# Removal: DROP TABLE addon_custom_activities; delete this folder.
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
})

source("R/persistence/migrate.R", local = FALSE)

#' Initialise the addon_custom_activities table.
#'
#' Idempotent. Safe to call from db_main() on every app start. The table DDL
#' lives in the versioned migrations (R/persistence/migrations/duckdb/); this
#' entry point ensures the schema is current.
#'
#' @param con  DuckDB connection (defaults to the global CON, matching how
#'             other RIDS DB init functions reference it).
#' @return     Invisibly TRUE on success.
ca_init_table <- function(con = CON) {
  run_migrations(con)
  invisible(TRUE)
}
