# ==============================================================================
# R/addons/custom_activities/ca_ref_activities.R
#
# Reference table of activity names available in the "Add custom activity"
# dropdown. Seed-only for v1 — no admin UI yet, but the data model is in
# place so an admin screen can be added later without migration.
#
# Schema:
#   ref_custom_activities(id, name, archived_at, created_at)
#
# Activities can be soft-archived (archived_at IS NOT NULL) so historical
# custom activities still resolve to a name, but the entry stops appearing
# in the dropdown.
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
  library(tibble)
})

# ── Seed data ────────────────────────────────────────────────────────────────
# To add or rename entries: update this vector and re-run ca_init_ref_activities().
# Existing entries stay; new ones are inserted; renaming requires a manual
# UPDATE (don't change names of in-use entries — they're referenced by historical
# custom activities by name).

.CA_REF_ACTIVITIES_SEED <- c(
  "Patient Expenses",
  "Carer Expenses",
  "Inconvenience Fee",
  "Accommodation: overnight",
  "Screen Failure"
)

# ── Schema ───────────────────────────────────────────────────────────────────

#' Ensure the ref_custom_activities table exists and seed/top-up entries.
#'
#' Idempotent. Safe to call from db_main() on every app start. The table DDL
#' lives in the versioned migrations; this seeds new entries from
#' .CA_REF_ACTIVITIES_SEED (ON CONFLICT no-op for existing, so admin edits
#' and historical entries are never overwritten).
#'
#' @param con  DuckDB connection (defaults to global CON).
#' @return     Invisibly TRUE.
ca_init_ref_activities <- function(con = CON) {
  run_migrations(con, dialect = "duckdb")

  for (nm in .CA_REF_ACTIVITIES_SEED) {
    dbExecute(con,
              "INSERT INTO ref_custom_activities (name) VALUES (?)
               ON CONFLICT (name) DO NOTHING",
              params = list(nm))
  }

  invisible(TRUE)
}

# ── Queries ──────────────────────────────────────────────────────────────────

#' Load the list of active (non-archived) activity names.
#'
#' Used by the custom activity modal to populate the Activity dropdown.
#'
#' @param con  DBI connection.
#' @return     Character vector of names, ordered alphabetically.
ca_load_ref_activities <- function(con = CON) {
  
  df <- dbGetQuery(con, "
    SELECT name
    FROM ref_custom_activities
    WHERE archived_at IS NULL
    ORDER BY name
  ")
  
  df$name
}
