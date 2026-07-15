# ==============================================================================
# R/addons/custom_activities/apply_custom_activities.R
#
# The single integration seam between the addon and the rest of RIDS.
#
# Called from step4_Server AFTER the pipeline's assign_edge_keys() and BEFORE
# dbAppendTable("posting_lines"). One line in step 4:
#
#   adjusted <- apply_custom_activities(adjusted, shared_state)
#
# What it does:
#   1. Load custom activities for this run (ca_load).
#   2. Short-circuit and return pipeline rows unchanged if none.
#   3. Mint addon-local edge keys (ca_assign_edge_keys).
#   4. Build posting_lines-shaped rows per activity (ca_build_custom_rows).
#   5. bind_rows with the pipeline output.
#
# Why this design:
#   - One call site in step 4 = clean removal path.
#   - Returns unchanged input when no custom activities exist (no-op safety
#     property — pipeline output is byte-identical to "no addon installed").
#   - All addon coupling is one-directional (addon → pipeline), never reverse.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})

#' Merge user-entered custom activities into the pipeline's posting lines.
#'
#' This is the function step4_Server calls. Idempotent in the sense that
#' calling it with no custom activities returns the input unchanged.
#'
#' @param pipeline_rows  Tibble. Output of the pipeline post-assign_edge_keys.
#'                       Must be posting_lines-schema compliant.
#' @param shared_state   The Shiny shared_state reactiveValues. Reads:
#'                         shared_state$cpms_id      (required)
#'                         shared_state$study_name   (optional)
#'                         shared_state$scenario_id  (optional)
#' @param con            DBI connection (defaults to global CON).
#' @return  Tibble. `pipeline_rows` with custom activity rows appended.
#'          Same column set as input.
apply_custom_activities <- function(pipeline_rows, shared_state, con = CON) {
  
  if (!is.data.frame(pipeline_rows)) {
    stop("apply_custom_activities(): `pipeline_rows` must be a data.frame.")
  }
  
  cpms_id <- shared_state$cpms_id
  if (is.null(cpms_id) || is.na(cpms_id) || !nzchar(as.character(cpms_id))) {
    stop("apply_custom_activities(): shared_state$cpms_id is missing or empty.")
  }
  
  # ── 1. Load custom activities for this run ────────────────────────────────
  study_site <- shared_state$study_site %||% NA_character_
  scenario_id <- shared_state$scenario_id %||% NA_character_
  customs <- ca_load(
    cpms_id = as.character(cpms_id),
    study_site = as.character(study_site),
    scenario_id = as.character(scenario_id),
    version_id = shared_state$template_version_id,
    con = con
  )
  
  # ── 2. No-op short-circuit ────────────────────────────────────────────────
  # Pipeline output unchanged when nothing's been added — gives the addon
  # the "byte-identical to no-addon" property.
  if (nrow(customs) == 0) {
    return(pipeline_rows)
  }
  
  # ── 3. Mint edge keys (one per activity, shared across its slots) ─────────
  customs_keyed <- ca_assign_edge_keys(customs)
  
  # ── 4. Build posting-lines-shaped rows ─────────────────────────────────────
  # row_id is offset by the activity's position so different activities get
  # distinct row_id ranges. We use the activity's index in the distinct id
  # list * 100 as a coarse offset. Plenty of headroom (max 5 slots).
  activity_index <- customs_keyed |>
    distinct(custom_activity_id) |>
    arrange(custom_activity_id) |>
    mutate(activity_idx = row_number())
  
  customs_keyed <- customs_keyed |>
    left_join(activity_index, by = "custom_activity_id")
  
  custom_rows <- customs_keyed |>
    group_by(custom_activity_id) |>
    group_split() |>
    map_dfr(function(g) {
      
      mode_val <- unique(g$mode)
      if (length(mode_val) != 1L) {
        stop("apply_custom_activities(): inconsistent mode within ",
             unique(g$custom_activity_id))
      }
      
      context <- list(
        cpms_id     = unique(g$cpms_id),
        study_site  = unique(g$study_site)  %||% shared_state$study_site %||% NA_character_,
        study_name  = unique(g$study_name)  %||% shared_state$study_name  %||% NA_character_,
        scenario_id = unique(g$scenario_id) %||% shared_state$scenario_id %||% NA_character_,
        Study_Arm   = unique(g$Study_Arm),
        Activity    = unique(g$Activity),
        edge_key    = unique(g$edge_key),
        row_id_base = 9000000L + unique(g$activity_idx) * 100L
      )
      
      ca_build_custom_rows(
        rows    = g |> arrange(slot_num) |> select(cost_centre, amount),
        mode    = mode_val,
        context = context
      )
    })
  
  # ── 5. Bind with pipeline output ───────────────────────────────────────────
  # Use bind_rows defensively — it will align columns by name and fill any
  # missing column on either side with NA. The two row sets SHOULD have the
  # same columns (custom_rows is built to the posting_lines schema), but the
  # defensiveness costs nothing.
  bind_rows(pipeline_rows, custom_rows)
}

# Null-coalesce helper (local to addon)
`%||%` <- function(a, b) if (is.null(a)) b else a
