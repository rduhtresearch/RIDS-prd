# ==============================================================================
# tests/test_ca_ref_activities.R
#
# Test the ref activities table init + queries. In-memory DuckDB, no real DB.
#
# Usage:
#   source("R/addons/custom_activities/ca_ref_activities.R")
#   source("tests/test_ca_ref_activities.R")
#   run_ca_ref_tests()
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

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

run_ca_ref_tests <- function() {
  
  cat("\n=== ca_ref_activities tests ===\n\n")
  .passed <<- 0L; .failed <<- 0L
  
  CON <<- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit({
    dbDisconnect(CON, shutdown = TRUE)
    rm(CON, envir = .GlobalEnv)
  }, add = TRUE)
  
  # ── First-time init ────────────────────────────────────────────────────────
  cat("[ first-time init ]\n")
  ca_init_ref_activities()
  .expect("table exists",
          "ref_custom_activities" %in% dbListTables(CON))
  
  cols <- dbListFields(CON, "ref_custom_activities")
  .expect("expected columns present",
          all(c("id", "name", "archived_at", "created_at") %in% cols))
  
  n <- dbGetQuery(CON, "SELECT COUNT(*) AS n FROM ref_custom_activities")$n
  .expect("seeded with 5 rows", n == 5L)
  
  # ── Idempotency ────────────────────────────────────────────────────────────
  cat("\n[ idempotency ]\n")
  ca_init_ref_activities()
  n2 <- dbGetQuery(CON, "SELECT COUNT(*) AS n FROM ref_custom_activities")$n
  .expect("re-running init doesn't duplicate", n2 == 5L)
  
  # ── Dropdown loader ────────────────────────────────────────────────────────
  cat("\n[ ca_load_ref_activities ]\n")
  names <- ca_load_ref_activities()
  .expect("returns character vector", is.character(names))
  .expect("returns 5 names",          length(names) == 5L)
  .expect("alphabetical order",       identical(names, sort(names)))
  .expect("Patient Expenses present", "Patient Expenses" %in% names)
  .expect("Screen Failure present",   "Screen Failure" %in% names)
  
  # ── Archive behaviour ──────────────────────────────────────────────────────
  cat("\n[ archive filtering ]\n")
  dbExecute(CON,
            "UPDATE ref_custom_activities SET archived_at = CURRENT_TIMESTAMP WHERE name = ?",
            params = list("Screen Failure"))
  
  names_after <- ca_load_ref_activities()
  .expect("archived entry excluded from dropdown", !("Screen Failure" %in% names_after))
  .expect("4 names remaining",                      length(names_after) == 4L)
  
  # ── Top-up new entries ─────────────────────────────────────────────────────
  cat("\n[ seed top-up ]\n")
  # Manually insert a "removed from seed" entry to simulate user-added record
  dbExecute(CON, "INSERT INTO ref_custom_activities (name) VALUES ('Custom admin-added entry')")
  
  ca_init_ref_activities()  # re-run init
  n3 <- dbGetQuery(CON, "SELECT COUNT(*) AS n FROM ref_custom_activities")$n
  .expect("custom entries survive re-init", n3 == 6L)
  
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .passed, "    FAILED: ", .failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")
  invisible(list(passed = .passed, failed = .failed))
}