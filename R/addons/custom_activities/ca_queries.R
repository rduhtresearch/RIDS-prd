# ==============================================================================
# R/addons/custom_activities/ca_queries.R
#
# Parameterised DBI queries against addon_custom_activities. No string
# interpolation. All functions take `con` as their last argument with a
# default of `CON` to match RIDS conventions.
#
# Function contract:
#   ca_next_id(cpms_id, study_site, scenario_id, con)
#'                                       -> "<cpms_id>-NNN" (readable, per-study)
#   ca_insert(activity, con)             -> custom_activity_id (the id just written)
#   ca_load(cpms_id, study_site, scenario_id, con)
#'                                       -> tibble: all custom activities for run
#   ca_delete(custom_activity_id, con)   -> integer: rows deleted
#   ca_clear_run(cpms_id, study_site, scenario_id, con)
#'                                       -> integer: rows deleted
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
  library(tibble)
})

# ── ID generation ────────────────────────────────────────────────────────────

#' Compute the next custom_activity_id for a given run.
#'
#' Format: "<cpms_id>-NNN" where NNN is zero-padded to 3 digits.
#' Looks at existing rows for this cpms_id and returns max+1. Starts at 001.
#'
#' Concurrency note: this is read-then-write without a lock. For RIDS' single-
#' user-per-run pattern this is fine. If you ever need multi-user concurrent
#' editing of the same study, switch to a transaction or a per-cpms_id sequence.
#'
#' @param cpms_id  Character. The study's CPMS ID.
#' @param study_site  Character. The study site.
#' @param scenario_id Character. The scenario ID.
#' @param con         DBI connection.
#' @return         Character. The next available id, e.g. "59904-001".
ca_next_id <- function(cpms_id, study_site, scenario_id, con = CON) {
  
  if (!is.character(cpms_id) || length(cpms_id) != 1L ||
      is.na(cpms_id) || !nzchar(cpms_id)) {
    stop("ca_next_id(): `cpms_id` must be a non-empty single string.")
  }
  if (!is.character(study_site) || length(study_site) != 1L ||
      is.na(study_site) || !nzchar(study_site)) {
    stop("ca_next_id(): `study_site` must be a non-empty single string.")
  }
  if (!is.character(scenario_id) || length(scenario_id) != 1L ||
      is.na(scenario_id) || !nzchar(scenario_id)) {
    stop("ca_next_id(): `scenario_id` must be a non-empty single string.")
  }
  
  existing <- rids_dbGetQuery(con, "
    SELECT DISTINCT custom_activity_id
    FROM addon_custom_activities
    WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?
  ", params = list(
    as.character(cpms_id),
    as.character(study_site),
    as.character(scenario_id)
  ))
  
  if (nrow(existing) == 0) {
    next_n <- 1L
  } else {
    # Extract trailing NNN, ignoring anything that doesn't parse.
    suffixes <- sub(paste0("^", cpms_id, "-"), "", existing$custom_activity_id)
    nums     <- suppressWarnings(as.integer(suffixes))
    nums     <- nums[!is.na(nums)]
    next_n   <- if (length(nums) == 0) 1L else max(nums) + 1L
  }
  
  sprintf("%s-%03d", cpms_id, next_n)
}

# ── Insert ───────────────────────────────────────────────────────────────────

#' Insert one custom activity (1 row for single_cc, 5 rows for baseline).
#'
#' Mints a custom_activity_id via ca_next_id() and writes all slot rows in a
#' single transaction so partial writes can't leave orphan rows.
#'
#' @param activity  Named list with fields:
#'                    cpms_id, study_site, study_name, scenario_id, Study_Arm, Activity,
#'                    mode ("single_cc" | "baseline"),
#'                    rows (data.frame: cost_centre, amount),
#'                    created_by (integer; nullable).
#' @param con       DBI connection.
#' @return          Character. The custom_activity_id just minted.
ca_insert <- function(activity, con = CON) {
  
  required <- c("cpms_id", "study_site", "scenario_id", "Study_Arm", "Activity", "mode", "rows")
  missing  <- setdiff(required, names(activity))
  if (length(missing) > 0) {
    stop("ca_insert(): `activity` missing fields: ", paste(missing, collapse = ", "))
  }
  
  if (!activity$mode %in% c("single_cc", "baseline")) {
    stop("ca_insert(): `mode` must be 'single_cc' or 'baseline'.")
  }
  
  expected_n <- if (activity$mode == "single_cc") 1L else 5L
  if (!is.data.frame(activity$rows) || nrow(activity$rows) != expected_n) {
    stop("ca_insert(): `rows` must be a data.frame with ", expected_n,
         " row(s) for mode '", activity$mode, "'.")
  }
  
  if (!all(c("cost_centre", "amount") %in% names(activity$rows))) {
    stop("ca_insert(): `rows` must have columns: cost_centre, amount.")
  }
  
  custom_activity_id <- ca_next_id(
    cpms_id = activity$cpms_id,
    study_site = activity$study_site,
    scenario_id = activity$scenario_id,
    con = con
  )
  
  # Build the insert tibble — one row per slot.
  insert_df <- tibble(
    custom_activity_id = custom_activity_id,
    cpms_id            = as.character(activity$cpms_id),
    study_site         = as.character(activity$study_site),
    study_name         = as.character(activity$study_name %||% NA_character_),
    scenario_id        = as.character(activity$scenario_id %||% NA_character_),
    version_id         = if (is.null(activity$version_id)) NA_integer_ else as.integer(activity$version_id),
    Study_Arm          = as.character(activity$Study_Arm),
    Activity           = as.character(activity$Activity),
    mode               = as.character(activity$mode),
    slot_num           = seq_len(nrow(activity$rows)),
    cost_centre        = as.character(activity$rows$cost_centre),
    amount             = as.numeric(activity$rows$amount),
    created_by         = if (is.null(activity$created_by)) NA_integer_
    else as.integer(activity$created_by)
  )

  if (!"version_id" %in% tolower(dbListFields(con, "addon_custom_activities"))) {
    insert_df$version_id <- NULL
  }
  
  # Transaction: either all slots land, or none do.
  dbWithTransaction(con, {
    dbAppendTable(con, "addon_custom_activities", rids_prepare_append(con, insert_df))
  })
  
  custom_activity_id
}

# ── Load ─────────────────────────────────────────────────────────────────────

#' Load all custom activities for a run.
#'
#' Returns the raw user-input rows from addon_custom_activities. Use this
#' result with ca_build_custom_rows() per (custom_activity_id) group to derive
#' the posting-line-shaped rows for export.
#'
#' @param cpms_id      Character. The study's CPMS ID.
#' @param study_site   Character. The study site.
#' @param scenario_id  Character. The scenario ID.
#' @param con          DBI connection.
#' @return             Tibble. Empty if no custom activities exist for this run.
ca_load <- function(cpms_id, study_site, scenario_id, con = CON, version_id = NULL) {
  
  if (!is.character(cpms_id) || length(cpms_id) != 1L ||
      is.na(cpms_id) || !nzchar(cpms_id)) {
    stop("ca_load(): `cpms_id` must be a non-empty single string.")
  }
  if (!is.character(study_site) || length(study_site) != 1L ||
      is.na(study_site) || !nzchar(study_site)) {
    stop("ca_load(): `study_site` must be a non-empty single string.")
  }
  if (!is.character(scenario_id) || length(scenario_id) != 1L ||
      is.na(scenario_id) || !nzchar(scenario_id)) {
    stop("ca_load(): `scenario_id` must be a non-empty single string.")
  }
  
  has_version_id <- "version_id" %in% tolower(dbListFields(con, "addon_custom_activities"))
  version_select <- if (has_version_id) ", version_id" else ""
  version_clause <- if (has_version_id && !is.null(version_id)) " AND version_id = ?" else ""
  params <- list(
    as.character(cpms_id),
    as.character(study_site),
    as.character(scenario_id)
  )
  if (nzchar(version_clause)) params <- c(params, list(as.integer(version_id)))

  df <- rids_dbGetQuery(con, paste0("
    SELECT id, custom_activity_id, cpms_id, study_site, study_name, scenario_id,
           Study_Arm, Activity, mode, slot_num, cost_centre, amount,
           created_by, created_at", version_select, "
    FROM addon_custom_activities
    WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?
    ", version_clause, "
    ORDER BY custom_activity_id, slot_num
  "), params = params)
  
  as_tibble(rids_canonicalize_names(df, "addon_custom_activities"))
}

# ── Delete one ───────────────────────────────────────────────────────────────

#' Delete a single custom activity (all its slot rows).
#'
#' Hard delete — no soft-delete / audit table in v1. The export ZIP is the
#' persisted record of what was actually submitted.
#'
#' @param custom_activity_id  Character. The id returned from ca_insert().
#' @param con                 DBI connection.
#' @param cpms_id             Optional study identity scope.
#' @param study_site          Optional study identity scope.
#' @param scenario_id         Optional study identity scope.
#' @param version_id          Optional template version scope.
#' @return                    Integer. Number of rows deleted.
ca_delete <- function(custom_activity_id, con = CON, cpms_id = NULL,
                      study_site = NULL, scenario_id = NULL, version_id = NULL) {
  
  if (!is.character(custom_activity_id) || length(custom_activity_id) != 1L ||
      is.na(custom_activity_id) || !nzchar(custom_activity_id)) {
    stop("ca_delete(): `custom_activity_id` must be a non-empty single string.")
  }
  
  clauses <- c("custom_activity_id = ?")
  params <- list(as.character(custom_activity_id))
  scopes <- list(
    cpms_id = cpms_id,
    study_site = study_site,
    scenario_id = scenario_id
  )
  for (column in names(scopes)) {
    value <- scopes[[column]]
    if (!is.null(value)) {
      clauses <- c(clauses, paste0(column, " = ?"))
      params <- c(params, list(as.character(value)))
    }
  }
  if (!is.null(version_id) &&
      "version_id" %in% tolower(dbListFields(con, "addon_custom_activities"))) {
    clauses <- c(clauses, "version_id = ?")
    params <- c(params, list(as.integer(version_id)))
  }

  rids_dbExecute(
    con,
    paste("DELETE FROM addon_custom_activities WHERE", paste(clauses, collapse = " AND ")),
    params = params
  )
}

# ── Clear all for run ────────────────────────────────────────────────────────

#' Delete all custom activities for a run.
#'
#' Called when the user navigates back to step 3 (per the v1 simplicity rule:
#' going back wipes custom activities). Also useful on fresh entry to step 4.
#'
#' @param cpms_id      Character. The study's CPMS ID.
#' @param study_site   Character. The study site.
#' @param scenario_id  Character. The scenario ID.
#' @param con          DBI connection.
#' @return             Integer. Number of rows deleted.
ca_clear_run <- function(cpms_id, study_site, scenario_id, con = CON, version_id = NULL) {
  
  if (!is.character(cpms_id) || length(cpms_id) != 1L ||
      is.na(cpms_id) || !nzchar(cpms_id)) {
    stop("ca_clear_run(): `cpms_id` must be a non-empty single string.")
  }
  if (!is.character(study_site) || length(study_site) != 1L ||
      is.na(study_site) || !nzchar(study_site)) {
    stop("ca_clear_run(): `study_site` must be a non-empty single string.")
  }
  if (!is.character(scenario_id) || length(scenario_id) != 1L ||
      is.na(scenario_id) || !nzchar(scenario_id)) {
    stop("ca_clear_run(): `scenario_id` must be a non-empty single string.")
  }
  
  has_version_id <- "version_id" %in% tolower(dbListFields(con, "addon_custom_activities"))
  version_clause <- if (has_version_id && !is.null(version_id)) " AND version_id = ?" else ""
  params <- list(
    as.character(cpms_id),
    as.character(study_site),
    as.character(scenario_id)
  )
  if (nzchar(version_clause)) params <- c(params, list(as.integer(version_id)))

  rids_dbExecute(con, paste0("
    DELETE FROM addon_custom_activities
    WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?
  ", version_clause), params = params)
}

# Null-coalesce helper (kept local to the addon)
`%||%` <- function(a, b) if (is.null(a)) b else a
