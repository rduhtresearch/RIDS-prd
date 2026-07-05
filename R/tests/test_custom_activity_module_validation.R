# ==============================================================================
# tests/test_custom_activity_module_validation.R
#
# Focused validation tests for the custom activity modal input rules.
#
# Usage:
#   source("R/modules/custom_activity_module.R")
#   source("R/tests/test_custom_activity_module_validation.R")
#   run_custom_activity_module_validation_tests()
# ==============================================================================

suppressPackageStartupMessages({
  library(tibble)
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

.base_modal_input <- function(mode = .CA_MODE_LEFT_VALUE) {
  x <- list(
    modal_arm      = "Treatment",
    modal_activity = "External consultancy",
    modal_mode     = mode,
    single_cc      = "RDH-FIN-001",
    single_amt     = 100,
    base_cc_1      = "CC1",
    base_cc_2      = "CC2",
    base_cc_3      = "CC3",
    base_cc_4      = "CC4",
    base_cc_5      = "CC5",
    base_amt_1     = 200,
    base_amt_2     = 200,
    base_amt_3     = 200,
    base_amt_4     = 200,
    base_amt_5     = 200
  )

  x
}

run_custom_activity_module_validation_tests <- function() {
  cat("\n=== custom_activity_module validation tests ===\n\n")
  .passed <<- 0L
  .failed <<- 0L

  cat("[ scalar parsing ]\n")
  .expect("parses numeric zero", identical(.ca_as_scalar_num(0), 0))
  .expect("blank string becomes NA", is.na(.ca_as_scalar_num("   ")))
  .expect("NA numeric stays NA", is.na(.ca_as_scalar_num(NA_real_)))

  cat("\n[ single_cc validation ]\n")
  single_zero <- .base_modal_input(.CA_MODE_LEFT_VALUE)
  single_zero$single_amt <- 0
  errs_single_zero <- .ca_validate_modal_inputs(single_zero)
  .expect("single mode accepts zero amount", length(errs_single_zero) == 0L)
  .expect("single mode has no single_amt error at zero", is.null(errs_single_zero$single_amt))

  single_missing <- .base_modal_input(.CA_MODE_LEFT_VALUE)
  single_missing$single_amt <- ""
  errs_single_missing <- .ca_validate_modal_inputs(single_missing)
  .expect("single mode rejects blank amount", identical(errs_single_missing$single_amt, "Required"))

  single_negative <- .base_modal_input(.CA_MODE_LEFT_VALUE)
  single_negative$single_amt <- -0.01
  errs_single_negative <- .ca_validate_modal_inputs(single_negative)
  .expect("single mode rejects negative amount", identical(errs_single_negative$single_amt, "Must be >= 0"))

  cat("\n[ baseline validation ]\n")
  baseline_zero <- .base_modal_input(.CA_MODE_RIGHT_VALUE)
  baseline_zero$base_amt_3 <- 0
  errs_baseline_zero <- .ca_validate_modal_inputs(baseline_zero)
  .expect("baseline mode accepts zero amount in a slot", length(errs_baseline_zero) == 0L)
  .expect("baseline mode has no base_amt_3 error at zero", is.null(errs_baseline_zero$base_amt_3))

  baseline_missing <- .base_modal_input(.CA_MODE_RIGHT_VALUE)
  baseline_missing$base_amt_4 <- NA_real_
  errs_baseline_missing <- .ca_validate_modal_inputs(baseline_missing)
  .expect("baseline mode rejects NA amount", identical(errs_baseline_missing$base_amt_4, "Required"))

  baseline_negative <- .base_modal_input(.CA_MODE_RIGHT_VALUE)
  baseline_negative$base_amt_5 <- -1
  errs_baseline_negative <- .ca_validate_modal_inputs(baseline_negative)
  .expect("baseline mode rejects negative amount", identical(errs_baseline_negative$base_amt_5, "Must be >= 0"))

  cat("\n[ submit enabled proxy ]\n")
  .expect("no validation errors means submit can enable in single mode",
          length(.ca_validate_modal_inputs(single_zero)) == 0L)
  .expect("no validation errors means submit can enable in baseline mode",
          length(.ca_validate_modal_inputs(baseline_zero)) == 0L)

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .passed, "    FAILED: ", .failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")
}
