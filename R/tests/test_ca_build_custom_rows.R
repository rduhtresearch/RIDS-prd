# ==============================================================================
# tests/test_ca_build_custom_rows.R
#
# Run this in the console after source()'ing ca_build_custom_rows.R.
# No Shiny, no DB — pure function checks.
#
# Usage:
#   source("R/addons/custom_activities/ca_build_custom_rows.R")
#   source("tests/test_ca_build_custom_rows.R")
#   run_ca_tests()
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# ── Tiny test helpers (no testthat dependency) ───────────────────────────────

.passed <- 0L
.failed <- 0L

.expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .passed <<- .passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .failed <<- .failed + 1L
  }
}

.expect_error <- function(label, expr) {
  err <- tryCatch({ force(expr); NULL }, error = function(e) e)
  if (!is.null(err)) {
    cat("  PASS  ", label, "  (errored as expected)\n", sep = "")
    .passed <<- .passed + 1L
  } else {
    cat("  FAIL  ", label, "  (expected error, got none)\n", sep = "")
    .failed <<- .failed + 1L
  }
}

# ── Fixtures ─────────────────────────────────────────────────────────────────

ctx <- list(
  cpms_id     = "59904",
  study_site  = "RDUHT",
  study_name  = "POLARIS-AD",
  Study_Arm   = "Treatment",
  Activity    = "External consultancy fee",
  scenario_id = "A",
  edge_key    = "CA-0001"
)

single_row <- tibble(
  cost_centre = "RDH-FIN-001",
  amount      = 1000.00
)

baseline_rows <- tibble(
  cost_centre = c("RDH-FIN-001", "RDH-RD-002", "RDH-OH-003",
                  "RDH-PI-004",  "RDH-SUP-005"),
  amount      = c(400, 200, 200, 100, 100)   # totals 1000
)

# ── Tests ────────────────────────────────────────────────────────────────────

run_ca_tests <- function() {
  cat("\n=== ca_build_custom_rows tests ===\n\n")
  .passed <<- 0L; .failed <<- 0L
  
  # ── Single CC mode ─────────────────────────────────────────────────────────
  cat("[ single_cc mode ]\n")
  out <- ca_build_custom_rows(single_row, mode = "single_cc", context = ctx)
  
  .expect("returns 1 row",                      nrow(out) == 1L)
  .expect("adjusted_amount = 1000",             out$adjusted_amount[1] == 1000)
  .expect("posting_amount = 1000",              out$posting_amount[1] == 1000)
  .expect("base_sum = 1000",                    out$base_sum[1] == 1000)
  .expect("multiplier = 1.0",                   out$multiplier[1] == 1.0)
  .expect("destination_entity = cost_centre",   out$destination_entity[1] == "RDH-FIN-001")
  .expect("destination_bucket = CUSTOM",        out$destination_bucket[1] == "CUSTOM")
  .expect("row_category = CUSTOM_SINGLE_CC",    out$row_category[1] == "CUSTOM_SINGLE_CC")
  .expect("sheet_name = Custom Activities",     out$sheet_name[1] == "Custom Activities")
  .expect("Visit placeholder set",              out$Visit[1] == "VISIT - 001")
  .expect("edge_key carried through",           out$edge_key[1] == "CA-0001")
  .expect("cpms_id carried through",            out$cpms_id[1] == "59904")
  .expect("study_site carried through",         out$study_site[1] == "RDUHT")
  .expect("Study_Arm carried through",          out$Study_Arm[1] == "Treatment")
  .expect("Activity carried through",           out$Activity[1] == "External consultancy fee")
  .expect("staff_group = 1L",                   out$staff_group[1] == 1L)
  .expect("is_residual_row = FALSE",            isFALSE(out$is_residual_row[1]))
  .expect("is_medic is NA (logical)",           is.na(out$is_medic[1]) && is.logical(out$is_medic))
  .expect("row_id is integer",                  is.integer(out$row_id))
  .expect("row_id starts at 9000000",           out$row_id[1] == 9000000L)
  .expect("posting_line_type_id correct",       out$posting_line_type_id[1] == "DIRECT")
  
  # ── Baseline mode ──────────────────────────────────────────────────────────
  cat("\n[ baseline mode ]\n")
  out2 <- ca_build_custom_rows(baseline_rows, mode = "baseline", context = ctx)
  
  .expect("returns 5 rows",                     nrow(out2) == 5L)
  .expect("sum(adjusted_amount) = 1000",        sum(out2$adjusted_amount) == 1000)
  .expect("amounts match input",                all(out2$adjusted_amount == baseline_rows$amount))
  .expect("all rows share edge_key",            length(unique(out2$edge_key)) == 1L)
  .expect("edge_key is CA-0001",                unique(out2$edge_key) == "CA-0001")
  .expect("all rows = CUSTOM_BASELINE",         all(out2$row_category == "CUSTOM_BASELINE"))
  .expect("row_ids are sequential",             all(diff(out2$row_id) == 1L))
  .expect("posting_line_type_ids distinct",     length(unique(out2$posting_line_type_id)) == 5L)
  .expect("posting_line_type_ids match split order",
          identical(
            out2$posting_line_type_id,
            c(
              "DIRECT",
              "CAPACITY_RD",
              "INDIRECT_50_DELIVERY",
              "INDIRECT_25_TRUST",
              "INDIRECT_25_PI"
            )
          ))
  .expect("cost centres match input",           all(out2$destination_entity == baseline_rows$cost_centre))
  .expect("all sheet_name = Custom Activities", all(out2$sheet_name == "Custom Activities"))
  .expect("all Study_Arm = Treatment",          all(out2$Study_Arm == "Treatment"))
  .expect("all Activity match",                 all(out2$Activity == "External consultancy fee"))
  .expect("all staff_group = 1L",               all(out2$staff_group == 1L))
  
  # ── Column schema check (matches posting_lines table) ──────────────────────
  cat("\n[ schema check ]\n")
  expected_cols <- c(
    "row_id", "scenario_id", "row_category_auto", "calc_tag", "row_category",
    "is_medic", "cpms_id", "study_site", "study_name", "Study_Arm", "Activity", "Visit",
    "posting_line_type_id", "posting_amount", "destination_bucket",
    "destination_entity", "cost_code", "sheet_name", "Visit_Label",
    "staff_group", "contract_cost", "Department", "Staff_Role",
    "contract_price", "base_sum", "multiplier", "adjusted_amount",
    "residual", "is_residual_row", "adjusted_sum_check", "diff_check",
    "edge_key"
  )
  .expect("all posting_lines columns present", all(expected_cols %in% names(out2)))
  .expect("no unexpected columns",             length(setdiff(names(out2), expected_cols)) == 0L)
  
  # ── Validation errors ──────────────────────────────────────────────────────
  cat("\n[ validation errors ]\n")
  
  .expect_error("rejects wrong mode",
                ca_build_custom_rows(single_row, "wrong_mode", ctx))
  
  .expect_error("rejects nrow != 1 for single_cc",
                ca_build_custom_rows(baseline_rows, "single_cc", ctx))
  
  .expect_error("rejects nrow != 5 for baseline",
                ca_build_custom_rows(single_row, "baseline", ctx))
  
  .expect_error("rejects empty cost_centre",
                ca_build_custom_rows(
                  tibble(cost_centre = "", amount = 100),
                  "single_cc", ctx
                ))
  
  .expect_error("rejects NA cost_centre",
                ca_build_custom_rows(
                  tibble(cost_centre = NA_character_, amount = 100),
                  "single_cc", ctx
                ))
  
  .expect_error("rejects NA amount",
                ca_build_custom_rows(
                  tibble(cost_centre = "X", amount = NA_real_),
                  "single_cc", ctx
                ))
  
  .expect_error("rejects non-finite amount",
                ca_build_custom_rows(
                  tibble(cost_centre = "X", amount = Inf),
                  "single_cc", ctx
                ))
  
  .expect_error("rejects missing context field",
                ca_build_custom_rows(single_row, "single_cc",
                                     context = ctx[setdiff(names(ctx), "edge_key")]))
  
  .expect_error("rejects missing cost_centre column",
                ca_build_custom_rows(
                  tibble(amount = 100),
                  "single_cc", ctx
                ))
  
  # ── bind_rows compatibility (smoke test) ───────────────────────────────────
  cat("\n[ bind_rows compatibility ]\n")
  # Build a minimal pipeline-shaped row with the same column set
  pipeline_like <- out2[1, ] %>% mutate(sheet_name = "Pharmacy", row_category = "BASELINE")
  combined <- bind_rows(pipeline_like, out2)
  .expect("bind_rows produces 6 rows",           nrow(combined) == 6L)
  .expect("no column drift after bind_rows",     all(names(combined) %in% names(out2)))
  
  # ── Summary ────────────────────────────────────────────────────────────────
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .passed, "    FAILED: ", .failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")
  invisible(list(passed = .passed, failed = .failed))
}
