# ==============================================================================
# R/addons/custom_activities/ca_assign_edge_keys.R
#
# Mints EDGE keys for custom activities. One key per custom_activity_id so
# that all slot rows of a baseline activity (5 rows) share the same key and
# collapse into ONE row in the EDGE template (via .build_special in
# build_all_edge_templates).
#
# Key format: "CA-NNNN" (zero-padded to 4 digits, starting at 0001).
#
# Why a separate function from the pipeline's assign_edge_keys():
#   - The pipeline keys by (sheet_name, Activity, row_id, staff_group,
#     Study_Arm) or (Study_Arm, Visit). Neither grouping is right for custom
#     activities — we want one key per logical activity.
#   - Keeping this addon-local means assign_edge_keys() stays untouched.
#   - The "CA-" prefix makes provenance visible in the EDGE export by eye.
#
# Pure function. No DB, no Shiny.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
})

.CA_EDGE_PREFIX <- "CA"
.CA_EDGE_WIDTH  <- 4L      # CA-0001 .. CA-9999

#' Mint EDGE keys for a set of custom activities.
#'
#' @param custom_activities  Tibble from ca_load(): one row per (activity, slot).
#'                           Must contain `custom_activity_id`.
#' @return  The same tibble with an added `edge_key` column. All rows sharing
#'          a `custom_activity_id` get the same `edge_key`. Returns the input
#'          unchanged (with empty `edge_key` column) if `nrow == 0`.
ca_assign_edge_keys <- function(custom_activities) {
  
  if (!is.data.frame(custom_activities)) {
    stop("ca_assign_edge_keys(): input must be a data.frame / tibble.")
  }
  
  if (nrow(custom_activities) == 0) {
    return(custom_activities |> mutate(edge_key = character(0)))
  }
  
  if (!"custom_activity_id" %in% names(custom_activities)) {
    stop("ca_assign_edge_keys(): input is missing `custom_activity_id` column.")
  }
  
  # Build the key map: one key per distinct custom_activity_id, in stable
  # sort order so keys are deterministic for a given input.
  distinct_ids <- custom_activities |>
    distinct(custom_activity_id) |>
    arrange(custom_activity_id) |>
    mutate(
      edge_key = paste0(
        .CA_EDGE_PREFIX, "-",
        str_pad(row_number(), width = .CA_EDGE_WIDTH, pad = "0")
      )
    )
  
  custom_activities |>
    left_join(distinct_ids, by = "custom_activity_id")
}