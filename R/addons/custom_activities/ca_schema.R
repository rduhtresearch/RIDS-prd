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

#' Initialise the addon_custom_activities table.
#'
#' Idempotent. Safe to call from db_main() on every app start. Creates the
#' table and its supporting sequence if they don't already exist.
#'
#' @param con  DuckDB connection (defaults to the global CON, matching how
#'             other RIDS DB init functions reference it).
#' @return     Invisibly TRUE on success.
ca_init_table <- function(con = CON) {
  
  # Sequence used as a fallback / safety net only — primary keying is the
  # readable composite id (see ca_next_id() in ca_queries.R). The sequence
  # gives us a guaranteed-unique integer per row for the surrogate id column.
  dbExecute(con, "
    CREATE SEQUENCE IF NOT EXISTS addon_ca_row_seq START 1;
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS addon_custom_activities (
      id                  INTEGER PRIMARY KEY DEFAULT nextval('addon_ca_row_seq'),
      custom_activity_id  VARCHAR NOT NULL,    -- e.g. '59904-001'
      cpms_id             VARCHAR NOT NULL,
      study_site          VARCHAR,
      study_name          VARCHAR,
      scenario_id         VARCHAR,
      Study_Arm           VARCHAR NOT NULL,
      Activity            VARCHAR NOT NULL,
      mode                VARCHAR NOT NULL,    -- 'single_cc' | 'baseline'
      slot_num            INTEGER NOT NULL,    -- 1..N (1 for single_cc, 1..5 for baseline)
      cost_centre         VARCHAR NOT NULL,
      amount              DOUBLE  NOT NULL,
      created_by          INTEGER,             -- auth_state$user_id (FK-ish to users.id)
      created_at          TIMESTAMP DEFAULT current_timestamp
    );
  ")
  
  addon_cols <- dbListFields(con, "addon_custom_activities")
  if (!"study_site" %in% addon_cols) {
    dbExecute(con, "ALTER TABLE addon_custom_activities ADD COLUMN study_site VARCHAR;")
  }

  # An index on the study identity plus custom_activity_id speeds up the
  # common queries: load-all-for-run, delete-by-activity, next-id lookup.
  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_addon_ca_cpms
      ON addon_custom_activities (cpms_id, study_site, scenario_id, custom_activity_id);
  ")
  
  invisible(TRUE)
}
