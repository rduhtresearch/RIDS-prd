# ==============================================================================
# tests/test_ca_chunk3.R
#
# End-to-end tests for chunk 3 (edge keys + merge function). Uses an in-memory
# DuckDB and synthetic pipeline rows. Does NOT touch the real RIDS DB.
#
# Usage:
#   source("R/addons/custom_activities/ca_build_custom_rows.R")
#   source("R/addons/custom_activities/ca_schema.R")
#   source("R/addons/custom_activities/ca_queries.R")
#   source("R/addons/custom_activities/ca_assign_edge_keys.R")
#   source("R/addons/custom_activities/apply_custom_activities.R")
#   source("tests/test_ca_chunk3.R")
#   run_ca_chunk3_tests()
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
})

# ── Tiny test helpers ────────────────────────────────────────────────────────

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

# Synthetic pipeline output row, posting_lines-schema compliant.
.make_pipeline_row <- function(row_id = 1L, edge_key = "EDGE-0001",
                               sheet_name = "Pharmacy") {
  tibble(
    row_id               = as.integer(row_id),
    scenario_id          = "A",
    row_category_auto    = "BASELINE",
    calc_tag             = NA_character_,
    row_category         = "BASELINE",
    is_medic             = FALSE,
    cpms_id              = "59904",
    study_site           = "RDUHT",
    study_name           = "POLARIS-AD",
    Study_Arm            = "Pharmacy",
    Activity             = "Pharmacy.Dispensing",
    Visit                = "VISIT - 001",
    posting_line_type_id = "DIRECT",
    posting_amount       = 250.00,
    destination_bucket   = "DEST_PROVIDER",
    destination_entity   = "RDUHT",
    cost_code            = NA_character_,
    sheet_name           = sheet_name,
    Visit_Label          = NA_character_,
    staff_group          = 1L,
    contract_cost        = NA_real_,
    Department           = "Pharmacy",
    Staff_Role           = "Pharmacist",
    contract_price       = NA_real_,
    base_sum             = 250.00,
    multiplier           = 1.0,
    adjusted_amount      = 250.00,
    residual             = NA_real_,
    is_residual_row      = FALSE,
    adjusted_sum_check   = NA_real_,
    diff_check           = NA_real_,
    edge_key             = edge_key
  )
}

.make_pipeline_rows <- function(n = 3) {
  purrr::map_dfr(seq_len(n), function(i) {
    .make_pipeline_row(row_id = i,
                       edge_key = sprintf("EDGE-%04d", i))
  })
}

.make_single_activity <- function(cpms_id = "59904", amount = 1000,
                                  activity_name = "External consultancy") {
  list(
    cpms_id     = cpms_id,
    study_site  = "RDUHT",
    study_name  = "POLARIS-AD",
    scenario_id = "A",
    Study_Arm   = "Treatment",
    Activity    = activity_name,
    mode        = "single_cc",
    rows        = tibble(cost_centre = "RDH-FIN-001", amount = amount),
    created_by  = 1L
  )
}

.make_baseline_activity <- function(cpms_id = "59904",
                                    activity_name = "Screening failure") {
  list(
    cpms_id     = cpms_id,
    study_site  = "RDUHT",
    study_name  = "POLARIS-AD",
    scenario_id = "A",
    Study_Arm   = "Treatment",
    Activity    = activity_name,
    mode        = "baseline",
    rows        = tibble(
      cost_centre = c("CC1", "CC2", "CC3", "CC4", "CC5"),
      amount      = c(200, 200, 200, 200, 200)
    ),
    created_by  = 1L
  )
}

.make_shared_state <- function(cpms_id = "59904") {
  list(
    cpms_id     = cpms_id,
    study_site  = "RDUHT",
    study_name  = "POLARIS-AD",
    scenario_id = "A"
  )
}

# ── Test runner ──────────────────────────────────────────────────────────────

run_ca_chunk3_tests <- function() {
  
  cat("\n=== ca chunk 3 tests (edge keys + merge) ===\n\n")
  .passed <<- 0L; .failed <<- 0L
  
  CON <<- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit({
    dbDisconnect(CON, shutdown = TRUE)
    rm(CON, envir = .GlobalEnv)
  }, add = TRUE)
  
  ca_init_table()
  
  # ════════════════════════════════════════════════════════════════════════════
  # ca_assign_edge_keys
  # ════════════════════════════════════════════════════════════════════════════
  cat("[ ca_assign_edge_keys: empty input ]\n")
  empty <- tibble(custom_activity_id = character(0))
  out_empty <- ca_assign_edge_keys(empty)
  .expect("returns 0 rows",       nrow(out_empty) == 0L)
  .expect("has edge_key column",  "edge_key" %in% names(out_empty))
  
  cat("\n[ ca_assign_edge_keys: single activity, single slot ]\n")
  single <- tibble(custom_activity_id = "59904-001", slot_num = 1L)
  out_single <- ca_assign_edge_keys(single)
  .expect("returns 1 row",        nrow(out_single) == 1L)
  .expect("edge_key = CA-0001",   out_single$edge_key[1] == "CA-0001")
  
  cat("\n[ ca_assign_edge_keys: single activity, 5 slots share key ]\n")
  baseline <- tibble(
    custom_activity_id = rep("59904-001", 5),
    slot_num           = 1:5
  )
  out_baseline <- ca_assign_edge_keys(baseline)
  .expect("returns 5 rows",                  nrow(out_baseline) == 5L)
  .expect("all 5 share same edge_key",       length(unique(out_baseline$edge_key)) == 1L)
  .expect("edge_key = CA-0001",              unique(out_baseline$edge_key) == "CA-0001")
  
  cat("\n[ ca_assign_edge_keys: multiple activities get distinct keys ]\n")
  multi <- tibble(
    custom_activity_id = c(rep("59904-001", 5), "59904-002", rep("59904-003", 5)),
    slot_num           = c(1:5, 1L, 1:5)
  )
  out_multi <- ca_assign_edge_keys(multi)
  .expect("returns 11 rows",                 nrow(out_multi) == 11L)
  .expect("3 distinct edge_keys",            length(unique(out_multi$edge_key)) == 3L)
  .expect("activity 001 → CA-0001",
          unique(out_multi$edge_key[out_multi$custom_activity_id == "59904-001"]) == "CA-0001")
  .expect("activity 002 → CA-0002",
          unique(out_multi$edge_key[out_multi$custom_activity_id == "59904-002"]) == "CA-0002")
  .expect("activity 003 → CA-0003",
          unique(out_multi$edge_key[out_multi$custom_activity_id == "59904-003"]) == "CA-0003")
  
  cat("\n[ ca_assign_edge_keys: deterministic ]\n")
  out_again <- ca_assign_edge_keys(multi)
  .expect("same input → same keys",
          identical(out_multi$edge_key, out_again$edge_key))
  
  cat("\n[ ca_assign_edge_keys: validation ]\n")
  .expect_error("rejects non-dataframe",         ca_assign_edge_keys("not a df"))
  .expect_error("rejects missing id column",     ca_assign_edge_keys(tibble(x = 1)))
  
  # ════════════════════════════════════════════════════════════════════════════
  # apply_custom_activities
  # ════════════════════════════════════════════════════════════════════════════
  cat("\n[ apply_custom_activities: no customs → returns input unchanged ]\n")
  pipeline <- .make_pipeline_rows(3)
  shared   <- .make_shared_state()
  out <- apply_custom_activities(pipeline, shared)
  .expect("returns 3 rows (unchanged)",      nrow(out) == 3L)
  .expect("identical to input",              identical(out, pipeline))
  
  cat("\n[ apply_custom_activities: 1 single_cc activity ]\n")
  id1 <- ca_insert(.make_single_activity())
  out2 <- apply_custom_activities(pipeline, shared)
  .expect("returns 4 rows (3 + 1)",          nrow(out2) == 4L)
  .expect("custom row has CA-0001 edge_key", "CA-0001" %in% out2$edge_key)
  .expect("custom row has CUSTOM bucket",    "CUSTOM" %in% out2$destination_bucket)
  .expect("custom row sum = 1000",           sum(out2$adjusted_amount[out2$destination_bucket == "CUSTOM"]) == 1000)
  .expect("pipeline rows preserved",
          all(c("EDGE-0001","EDGE-0002","EDGE-0003") %in% out2$edge_key))
  .expect("column count unchanged",          ncol(out2) == ncol(pipeline))
  
  cat("\n[ apply_custom_activities: + 1 baseline activity ]\n")
  id2 <- ca_insert(.make_baseline_activity())
  out3 <- apply_custom_activities(pipeline, shared)
  .expect("returns 9 rows (3 + 1 + 5)",      nrow(out3) == 9L)
  .expect("both edge keys present",
          all(c("CA-0001", "CA-0002") %in% out3$edge_key))
  .expect("5 rows share CA-0002",            sum(out3$edge_key == "CA-0002") == 5L)
  .expect("baseline sum = 1000",
          sum(out3$adjusted_amount[out3$edge_key == "CA-0002"]) == 1000)
  .expect("total custom = 2000",
          sum(out3$adjusted_amount[out3$destination_bucket == "CUSTOM"]) == 2000)
  
  cat("\n[ apply_custom_activities: row_ids don't collide ]\n")
  custom_row_ids <- out3$row_id[out3$destination_bucket == "CUSTOM"]
  pipeline_row_ids <- out3$row_id[out3$destination_bucket != "CUSTOM"]
  .expect("custom row_ids all > 9000000",    all(custom_row_ids > 9000000L))
  .expect("no overlap with pipeline ids",    length(intersect(custom_row_ids, pipeline_row_ids)) == 0L)
  .expect("custom row_ids unique",           length(unique(custom_row_ids)) == length(custom_row_ids))
  
  cat("\n[ apply_custom_activities: schema integrity ]\n")
  .expect("sheet_name 'Custom Activities' present",
          "Custom Activities" %in% out3$sheet_name)
  .expect("custom rows have row_category_auto = NA",
          all(is.na(out3$row_category_auto[out3$destination_bucket == "CUSTOM"])))
  .expect("pipeline rows untouched",
          all(out3$destination_bucket[out3$sheet_name == "Pharmacy"] == "DEST_PROVIDER"))
  
  cat("\n[ apply_custom_activities: isolates by cpms_id ]\n")
  # Insert a custom activity for a DIFFERENT study, then run for original study.
  # The other study's custom shouldn't appear.
  ca_insert(.make_single_activity(cpms_id = "12345", amount = 9999))
  out4 <- apply_custom_activities(pipeline, shared)
  .expect("other study's custom NOT included", !(9999 %in% out4$adjusted_amount))
  .expect("still 9 rows for our study",        nrow(out4) == 9L)
  
  cat("\n[ apply_custom_activities: ready for build_all_edge_templates ]\n")
  # The function .build_special groups by (Study_Arm, sheet_name, Activity,
  # row_id, staff_group, edge_key, Department, study_name, cpms_id) and
  # summarises adjusted_amount. Verify our custom rows have all those columns.
  required_for_special <- c("Study_Arm", "sheet_name", "Activity", "row_id",
                            "staff_group", "edge_key", "Department",
                            "study_name", "cpms_id", "adjusted_amount")
  .expect("all .build_special grouping cols present",
          all(required_for_special %in% names(out4)))
  
  # Simulate what .build_special does: for each custom edge_key, sum
  # adjusted_amount. The result is what one EDGE template row would show.
  edge_sums <- out4 |>
    filter(destination_bucket == "CUSTOM") |>
    group_by(edge_key) |>
    summarise(total = sum(adjusted_amount), .groups = "drop")
  
  .expect("CA-0001 EDGE row would show 1000", edge_sums$total[edge_sums$edge_key == "CA-0001"] == 1000)
  .expect("CA-0002 EDGE row would show 1000", edge_sums$total[edge_sums$edge_key == "CA-0002"] == 1000)
  .expect("2 distinct EDGE rows for custom",  nrow(edge_sums) == 2L)
  
  cat("\n[ apply_custom_activities: validation ]\n")
  .expect_error("rejects non-dataframe",
                apply_custom_activities("not a df", shared))
  .expect_error("rejects missing cpms_id",
                apply_custom_activities(pipeline, list(cpms_id = NULL)))
  .expect_error("rejects empty cpms_id",
                apply_custom_activities(pipeline, list(cpms_id = "")))
  
  cat("\n[ apply_custom_activities: round-trip after clear_run ]\n")
  ca_clear_run("59904", "RDUHT", "A")
  out_after_clear <- apply_custom_activities(pipeline, shared)
  .expect("post-clear returns 3 rows (just pipeline)",  nrow(out_after_clear) == 3L)
  .expect("post-clear identical to original pipeline",   identical(out_after_clear, pipeline))
  
  # ── Summary ────────────────────────────────────────────────────────────────
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .passed, "    FAILED: ", .failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")
  invisible(list(passed = .passed, failed = .failed))
}
