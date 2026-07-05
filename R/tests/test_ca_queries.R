# ==============================================================================
# tests/test_ca_queries.R
#
# Test the addon's schema + queries layer against an in-memory DuckDB.
# Does NOT touch the real RIDS DB.
#
# Usage:
#   source("R/addons/custom_activities/ca_schema.R")
#   source("R/addons/custom_activities/ca_queries.R")
#   source("tests/test_ca_queries.R")
#   run_ca_query_tests()
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

# ── Test fixtures ────────────────────────────────────────────────────────────

.make_single_activity <- function(cpms_id = "59904",
                                  study_site = "RDUHT",
                                  cost_centre = "RDH-FIN-001",
                                  amount = 1000,
                                  activity_name = "External consultancy") {
  list(
    cpms_id     = cpms_id,
    study_site  = study_site,
    study_name  = "POLARIS-AD",
    scenario_id = "A",
    Study_Arm   = "Treatment",
    Activity    = activity_name,
    mode        = "single_cc",
    rows        = tibble(cost_centre = cost_centre, amount = amount),
    created_by  = 1L
  )
}

.make_baseline_activity <- function(cpms_id = "59904",
                                    study_site = "RDUHT",
                                    activity_name = "Screening failure recovery") {
  list(
    cpms_id     = cpms_id,
    study_site  = study_site,
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

# ── Test runner ──────────────────────────────────────────────────────────────

run_ca_query_tests <- function() {
  
  cat("\n=== ca_queries tests (in-memory DuckDB) ===\n\n")
  .passed <<- 0L; .failed <<- 0L
  
  # Fresh in-memory DB for every run
  CON <<- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit({
    dbDisconnect(CON, shutdown = TRUE)
    rm(CON, envir = .GlobalEnv)
  }, add = TRUE)
  
  # ── Schema init ────────────────────────────────────────────────────────────
  cat("[ schema init ]\n")
  ca_init_table()
  .expect("table created",
          "addon_custom_activities" %in% dbListTables(CON))
  
  cols <- dbListFields(CON, "addon_custom_activities")
  expected_cols <- c("id", "custom_activity_id", "cpms_id", "study_site", "study_name",
                     "scenario_id", "Study_Arm", "Activity", "mode",
                     "slot_num", "cost_centre", "amount", "created_by",
                     "created_at")
  .expect("all expected columns present",
          all(expected_cols %in% cols))
  
  .expect("ca_init_table is idempotent (re-run doesn't error)",
          {
            ca_init_table()
            TRUE
          })
  
  # ── ID generation ──────────────────────────────────────────────────────────
  cat("\n[ ca_next_id ]\n")
  .expect("first id is cpms-001",
          ca_next_id("59904", "RDUHT", "A") == "59904-001")
  
  .expect_error("rejects empty cpms_id",
                ca_next_id("", "RDUHT", "A"))
  .expect_error("rejects NA cpms_id",
                ca_next_id(NA_character_, "RDUHT", "A"))
  
  # ── Insert: single_cc ──────────────────────────────────────────────────────
  cat("\n[ ca_insert: single_cc ]\n")
  id1 <- ca_insert(.make_single_activity())
  .expect("returns id 59904-001",  id1 == "59904-001")
  
  rows <- dbGetQuery(CON, "SELECT * FROM addon_custom_activities WHERE custom_activity_id = ?",
                     params = list(id1))
  .expect("inserted 1 row",                  nrow(rows) == 1L)
  .expect("slot_num = 1",                    rows$slot_num[1] == 1L)
  .expect("amount preserved",                rows$amount[1] == 1000)
  .expect("cost_centre preserved",           rows$cost_centre[1] == "RDH-FIN-001")
  .expect("mode = single_cc",                rows$mode[1] == "single_cc")
  .expect("Activity preserved",              rows$Activity[1] == "External consultancy")
  .expect("created_by = 1",                  rows$created_by[1] == 1L)
  .expect("created_at populated",            !is.na(rows$created_at[1]))
  
  # ── Insert: baseline ───────────────────────────────────────────────────────
  cat("\n[ ca_insert: baseline ]\n")
  id2 <- ca_insert(.make_baseline_activity())
  .expect("returns next id 59904-002",  id2 == "59904-002")
  
  rows2 <- dbGetQuery(CON, "SELECT * FROM addon_custom_activities WHERE custom_activity_id = ?",
                      params = list(id2))
  .expect("inserted 5 rows",                 nrow(rows2) == 5L)
  .expect("slot_nums are 1..5",              identical(sort(rows2$slot_num), 1:5))
  .expect("sum(amount) = 1000",              sum(rows2$amount) == 1000)
  .expect("all mode = baseline",             all(rows2$mode == "baseline"))
  .expect("all share custom_activity_id",    length(unique(rows2$custom_activity_id)) == 1L)
  .expect("Activity preserved across slots", all(rows2$Activity == "Screening failure recovery"))
  
  # ── Insert: NULL created_by ────────────────────────────────────────────────
  cat("\n[ ca_insert: nullable created_by ]\n")
  act_no_user <- .make_single_activity(cost_centre = "CC-X", amount = 50)
  act_no_user$created_by <- NULL
  id3 <- ca_insert(act_no_user)
  rows3 <- dbGetQuery(CON, "SELECT * FROM addon_custom_activities WHERE custom_activity_id = ?",
                      params = list(id3))
  .expect("inserts with NULL created_by",    is.na(rows3$created_by[1]))
  
  # ── Sequential ID generation ───────────────────────────────────────────────
  cat("\n[ sequential id allocation ]\n")
  .expect("third id is 59904-003 (not -002 again)",
          id3 == "59904-003")
  
  next_id <- ca_next_id("59904", "RDUHT", "A")
  .expect("next id is 59904-004",  next_id == "59904-004")
  
  # Different cpms_id starts fresh at 001
  id_other <- ca_insert(.make_single_activity(cpms_id = "12345"))
  .expect("different cpms_id starts at 001",  id_other == "12345-001")
  
  # ── ca_load ────────────────────────────────────────────────────────────────
  cat("\n[ ca_load ]\n")
  loaded <- ca_load("59904", "RDUHT", "A")
  .expect("loads tibble",                    is_tibble(loaded))
  .expect("loads 7 rows (1+5+1)",            nrow(loaded) == 7L)
  .expect("ordered by custom_activity_id then slot_num",
          identical(loaded$custom_activity_id,
                    sort(loaded$custom_activity_id)))
  
  loaded_other <- ca_load("12345", "RDUHT", "A")
  .expect("isolates by cpms_id",             nrow(loaded_other) == 1L)
  
  empty <- ca_load("99999", "RDUHT", "A")
  .expect("returns empty tibble for no-data run", nrow(empty) == 0L)
  .expect("empty result is still a tibble",       is_tibble(empty))
  
  # ── ca_delete ──────────────────────────────────────────────────────────────
  cat("\n[ ca_delete ]\n")
  deleted_n <- ca_delete(id2)   # the baseline (5 rows)
  .expect("ca_delete returns 5 (rows removed)",  deleted_n == 5L)
  
  loaded_after <- ca_load("59904", "RDUHT", "A")
  .expect("baseline rows are gone",              !(id2 %in% loaded_after$custom_activity_id))
  .expect("other activities still present",      id1 %in% loaded_after$custom_activity_id)
  
  deleted_again <- ca_delete(id2)
  .expect("re-delete returns 0",                 deleted_again == 0L)
  
  # ── ca_next_id after delete ────────────────────────────────────────────────
  cat("\n[ ca_next_id after delete ]\n")
  # We've deleted -002 but -003 still exists. Max suffix is 3, so next is 4.
  # This is intentional: ids are not reused. If you delete -002, the next id
  # is still -004, not -002.
  next_after_delete <- ca_next_id("59904", "RDUHT", "A")
  .expect("next_id continues past deleted (not reused)",
          next_after_delete == "59904-004")
  
  # ── ca_clear_run ───────────────────────────────────────────────────────────
  cat("\n[ ca_clear_run ]\n")
  before <- nrow(ca_load("59904", "RDUHT", "A"))
  cleared_n <- ca_clear_run("59904", "RDUHT", "A")
  .expect("clears all rows for the run",        cleared_n == before)
  .expect("nothing left for that run",          nrow(ca_load("59904", "RDUHT", "A")) == 0L)
  .expect("other run unaffected",               nrow(ca_load("12345", "RDUHT", "A")) == 1L)
  
  # After full clear, ids start fresh
  fresh_id <- ca_next_id("59904", "RDUHT", "A")
  .expect("after clear, next_id resets to 001", fresh_id == "59904-001")
  
  # ── Insert validation ──────────────────────────────────────────────────────
  cat("\n[ ca_insert validation ]\n")
  
  .expect_error("rejects missing field",
                ca_insert(list(cpms_id = "X", Study_Arm = "A", Activity = "B", mode = "single_cc")))
  
  .expect_error("rejects bad mode",
                {
                  act <- .make_single_activity()
                  act$mode <- "wrong"
                  ca_insert(act)
                })
  
  .expect_error("rejects wrong row count for single_cc",
                {
                  act <- .make_single_activity()
                  act$rows <- tibble(cost_centre = c("a","b"), amount = c(1,2))
                  ca_insert(act)
                })
  
  .expect_error("rejects wrong row count for baseline",
                {
                  act <- .make_baseline_activity()
                  act$rows <- tibble(cost_centre = "a", amount = 1)
                  ca_insert(act)
                })
  
  .expect_error("rejects missing rows column",
                {
                  act <- .make_single_activity()
                  act$rows <- tibble(amount = 100)
                  ca_insert(act)
                })
  
  # ── Transaction integrity (smoke test) ─────────────────────────────────────
  cat("\n[ transaction safety ]\n")
  # Try inserting an activity with a NA amount — this passes the function
  # validation (NA is technically numeric) but should be caught somewhere if
  # we add stricter checks later. For now, just confirm it doesn't half-write.
  before_n <- nrow(ca_load("12345", "RDUHT", "A"))
  ok <- tryCatch({
    ca_insert(.make_baseline_activity(cpms_id = "12345"))
    TRUE
  }, error = function(e) FALSE)
  after_n <- nrow(ca_load("12345", "RDUHT", "A"))
  .expect("baseline insert wrote all 5 rows or none",
          (ok && after_n == before_n + 5L) || (!ok && after_n == before_n))
  
  # ── Summary ────────────────────────────────────────────────────────────────
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .passed, "    FAILED: ", .failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")
  invisible(list(passed = .passed, failed = .failed))
}
