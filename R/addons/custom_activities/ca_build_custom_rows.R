# ==============================================================================
# R/addons/custom_activities/ca_build_custom_rows.R
#
# Bolt-on addon for RIDS: builds posting-line rows for user-entered custom
# activities. Pure function — no DB, no Shiny, no apportionment math. The user
# enters per-row amounts and cost centres directly; this function just shapes
# them to match the posting_lines schema so they can be bind_rows'd with the
# pipeline output before dbAppendTable.
#
# Two modes:
#   single_cc  : 1 row,  user enters 1 cost centre and 1 amount
#   baseline   : 5 rows, user enters 5 cost centres and 5 amounts (locked at 5)
#
# Removal note: this file lives entirely under R/addons/custom_activities/.
# Deleting that folder, removing the call site in step4_Server, and dropping
# the addon_custom_activities table is the full removal path.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(stringr)
})

# ── Constants ────────────────────────────────────────────────────────────────

.CA_BASELINE_N         <- 5L
.CA_VALID_MODES        <- c("single_cc", "baseline")
.CA_SHEET_NAME         <- "Custom Activities"
.CA_PLACEHOLDER_VISIT  <- "VISIT - 001"
.CA_ROW_ID_BASE        <- 9000000L

# Audit markers on the posting_lines rows. These are the ONLY signal in
# posting_lines that a row is custom — keep them stable, downstream SQL may
# filter on them.
.CA_ROW_CATEGORY <- c(
  single_cc = "CUSTOM_SINGLE_CC",
  baseline  = "CUSTOM_BASELINE"
)
.CA_DEST_BUCKET <- "CUSTOM"
.CA_POSTING_LINE_TYPES <- list(
  single_cc = "DIRECT",
  baseline = c(
    "DIRECT",
    "CAPACITY_RD",
    "INDIRECT_50_DELIVERY",
    "INDIRECT_25_TRUST",
    "INDIRECT_25_PI"
  )
)

# ── Validation ───────────────────────────────────────────────────────────────

.ca_validate_inputs <- function(rows, mode, context) {
  
  if (!is.character(mode) || length(mode) != 1L || !mode %in% .CA_VALID_MODES) {
    stop("ca_build_custom_rows(): `mode` must be one of: ",
         paste(.CA_VALID_MODES, collapse = ", "))
  }
  
  expected_n <- if (mode == "single_cc") 1L else .CA_BASELINE_N
  if (!is.data.frame(rows) || nrow(rows) != expected_n) {
    stop("ca_build_custom_rows(): `rows` must be a data.frame with exactly ",
         expected_n, " row(s) for mode '", mode, "'. Got ",
         if (is.data.frame(rows)) nrow(rows) else "non-dataframe", ".")
  }
  
  required_cols <- c("cost_centre", "amount")
  missing_cols  <- setdiff(required_cols, names(rows))
  if (length(missing_cols) > 0) {
    stop("ca_build_custom_rows(): `rows` is missing columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # cost_centre: non-empty character on every row
  cc <- rows$cost_centre
  if (!is.character(cc) || any(is.na(cc)) || any(!nzchar(str_trim(cc)))) {
    stop("ca_build_custom_rows(): every row must have a non-empty cost_centre.")
  }
  
  # amount: finite numeric, may be zero or negative in principle but warn for now
  amt <- rows$amount
  if (!is.numeric(amt) || any(is.na(amt)) || any(!is.finite(amt))) {
    stop("ca_build_custom_rows(): every row must have a finite numeric amount.")
  }
  
  required_context <- c("cpms_id", "study_site", "study_name", "Study_Arm", "Activity",
                        "scenario_id", "edge_key")
  missing_ctx <- setdiff(required_context, names(context))
  if (length(missing_ctx) > 0) {
    stop("ca_build_custom_rows(): `context` is missing fields: ",
         paste(missing_ctx, collapse = ", "))
  }
  
  invisible(TRUE)
}

.ca_posting_line_types <- function(mode, n) {
  posting_line_types <- .CA_POSTING_LINE_TYPES[[mode]]
  if (is.null(posting_line_types) || length(posting_line_types) != n) {
    stop("ca_build_custom_rows(): posting line type mapping is invalid for mode '", mode, "'.")
  }

  posting_line_types
}

# ── Main ─────────────────────────────────────────────────────────────────────

#' Build posting-line rows for a single custom activity.
#'
#' Pure function. No DB, no apportionment. The user has already entered the
#' per-row cost centres and amounts; this just shapes them to the posting_lines
#' schema. Every row shares the same `edge_key` so they collapse into one row
#' in the EDGE template (via .build_special in build_all_edge_templates).
#'
#' @param rows    data.frame with columns:
#'                  cost_centre (chr, non-empty)
#'                  amount      (numeric, finite)
#'                Must be nrow = 1 for mode "single_cc", nrow = 5 for "baseline".
#' @param mode    "single_cc" or "baseline".
#' @param context Named list with required fields:
#'                  cpms_id, study_name, Study_Arm, Activity,
#'                  scenario_id, edge_key
#'                Optional:
#'                  row_id_base (integer; default 9000000)
#'                    — first row_id used; subsequent rows increment by 1.
#'
#' @return Tibble of posting_lines-schema rows. All non-essential columns
#'         are NA of the correct type. `adjusted_amount` and `posting_amount`
#'         both carry the user-entered amount. `destination_entity` is the
#'         user-entered cost centre. All rows share `edge_key`.
ca_build_custom_rows <- function(rows, mode, context) {
  
  .ca_validate_inputs(rows, mode, context)
  
  row_id_base <- context$row_id_base %||% .CA_ROW_ID_BASE
  n           <- nrow(rows)
  posting_line_types <- .ca_posting_line_types(mode, n)
  
  tibble(
    # ── Identity & run context ──────────────────────────────────────────────
    row_id               = as.integer(row_id_base + seq_len(n) - 1L),
    scenario_id          = as.character(context$scenario_id),
    cpms_id              = as.character(context$cpms_id),
    study_site           = as.character(context$study_site),
    study_name           = as.character(context$study_name),
    Study_Arm            = as.character(context$Study_Arm),
    Activity             = as.character(context$Activity),
    
    # ── Routing / audit markers ─────────────────────────────────────────────
    row_category_auto    = NA_character_,
    calc_tag             = NA_character_,
    row_category         = .CA_ROW_CATEGORY[[mode]],
    is_medic             = NA,                          # logical NA
    posting_line_type_id = posting_line_types,
    destination_bucket   = .CA_DEST_BUCKET,
    destination_entity   = as.character(rows$cost_centre),
    cost_code            = NA_character_,
    
    # ── Pipeline-schema bookkeeping (required for .build_special grouping) ──
    sheet_name           = .CA_SHEET_NAME,
    Visit                = .CA_PLACEHOLDER_VISIT,
    Visit_Label          = NA_character_,
    staff_group          = 1L,
    Department           = NA_character_,
    Staff_Role           = NA_character_,
    contract_cost        = NA_real_,
    
    # ── Amount columns ──────────────────────────────────────────────────────
    # No multiplier, no MFF, no scaling — user entered the final number.
    posting_amount       = as.numeric(rows$amount),
    contract_price       = NA_real_,
    base_sum             = as.numeric(rows$amount),
    multiplier           = 1.0,
    adjusted_amount      = as.numeric(rows$amount),
    residual             = NA_real_,
    is_residual_row      = FALSE,
    adjusted_sum_check   = NA_real_,
    diff_check           = NA_real_,
    
    # ── Edge key (shared across all rows of one custom activity) ────────────
    edge_key             = as.character(context$edge_key)
  )
}

# Null-coalesce helper (rlang has %||% but keeping addon self-contained)
`%||%` <- function(a, b) if (is.null(a)) b else a
