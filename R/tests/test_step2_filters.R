# ==============================================================================
# tests/test_step2_filters.R
#
# Focused tests for Step 2 view-only study arm / visit / activity filters.
#
# Usage:
#   source("R/modules/step2_mod.R")
#   source("R/tests/test_step2_filters.R")
#   run_step2_filter_tests()
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

run_step2_filter_tests <- function() {
  cat("\n=== step2 filter tests ===\n\n")
  .passed <<- 0L
  .failed <<- 0L

  rows <- tibble(
    CPMS_ID = "CP1",
    study_site = "RDUHT",
    scenario_id = "A",
    Study = "Study A",
    Visit_Number = c("VISIT - 002", "VISIT - 001", "VISIT - 001", "VISIT - 003"),
    Study_Arm = c("Arm B", "Arm A", "Arm A", "Arm B"),
    Visit_Label = c("Follow-up", "Screening", "Screening", "Closeout"),
    Activity_Name = c("Blood test", "Informed consent", "Demographics", NA_character_),
    ICT_Cost = c(20, 100, 50, 10),
    Contract_Cost = c(20, 100, 50, 10),
    activity_occurrence_id = c("B1", "A1", "A2", "B2"),
    staff_group = c(1L, 1L, 2L, 1L)
  )

  cat("[ choices ]\n")
  arm_choices <- step2_study_arm_choices(rows)
  visit_choices <- step2_visit_choices(rows)

  .expect("study arm choices include all arms sorted alphabetically",
          identical(
            unname(arm_choices),
            c(STEP2_FILTER_ALL_ARMS, "Arm A", "Arm B")
          ))
  .expect("visit choices include visit number and label",
          identical(
            names(visit_choices),
            c(
              "All visits",
              "VISIT - 001 - Screening",
              "VISIT - 002 - Follow-up",
              "VISIT - 003 - Closeout"
            )
          ))

  cat("\n[ filtering ]\n")
  arm_rows <- step2_filter_rows(
    rows,
    study_arm_filter = "Arm A",
    visit_filter = STEP2_FILTER_ALL_VISITS,
    activity_search = ""
  )
  .expect("study arm filter returns matching rows only",
          identical(arm_rows$Study_Arm, c("Arm A", "Arm A")))
  .expect("filtered rows retain original source indices",
          identical(arm_rows$.step2_source_index, c(2L, 3L)))

  visit_rows <- step2_filter_rows(
    rows,
    study_arm_filter = "Arm A",
    visit_filter = "VISIT - 001",
    activity_search = ""
  )
  .expect("study arm and visit filters stack",
          identical(visit_rows$Activity_Name, c("Informed consent", "Demographics")))

  search_rows <- step2_filter_rows(
    rows,
    study_arm_filter = STEP2_FILTER_ALL_ARMS,
    visit_filter = STEP2_FILTER_ALL_VISITS,
    activity_search = "CONSENT"
  )
  .expect("activity search is case-insensitive",
          identical(search_rows$.step2_source_index, 2L))

  missing_activity_rows <- step2_filter_rows(
    rows,
    study_arm_filter = "Arm B",
    visit_filter = "VISIT - 003",
    activity_search = "consent"
  )
  .expect("activity search handles missing activity names without errors",
          nrow(missing_activity_rows) == 0L)

  cat("\n[ contract cost initialisation ]\n")
  rounded_input <- rows
  rounded_input$Contract_Cost <- c(NA_real_, NA_real_, 75, NA_real_)
  rounded_rows <- step2_initialize_contract_costs(
    rounded_input,
    use_unrounded_cost = FALSE
  )
  .expect("missing contract costs initialise from rounded ICT cost",
          identical(rounded_rows$Contract_Cost, c(20, 100, 75, 10)))

  unrounded_input <- rows
  unrounded_input$Contract_Cost <- NA_real_
  unrounded_rows <- step2_initialize_contract_costs(
    unrounded_input,
    use_unrounded_cost = TRUE
  )
  .expect("unrounded mode initialises every missing contract cost from ICT cost",
          identical(unrounded_rows$Contract_Cost, c(20, 100, 50, 10)))

  cat("\n[ override state ]\n")
  override_rows <- tibble(
    CPMS_ID = "CP1",
    study_site = "RDUHT",
    scenario_id = "A",
    Study = "Study A",
    Visit_Number = c("VISIT - 002", "VISIT - 001", "VISIT - 001", "VISIT - 003"),
    Study_Arm = c("Arm B", "Arm A", "Arm A", "Arm B"),
    Visit_Label = c("Follow-up", "Screening", "Screening", "Closeout"),
    Activity_Name = c("Blood test", "Informed consent", "Demographics", "Follow-up labs"),
    ICT_Cost = c(20.49, 100.49, 50.25, 10.75),
    Contract_Cost = c(20.49, 100.49, 50.25, 10.75),
    activity_occurrence_id = c("B1", "A1", "A2", "B2"),
    staff_group = c(1L, 1L, 2L, 1L)
  )

  prepared_rows <- step2_prepare_working_data(
    override_rows,
    use_unrounded_cost = FALSE
  )
  overridden_rows <- step2_apply_contract_override(prepared_rows, 2L, 123.45)

  .expect("manual override updates only the selected row",
          identical(overridden_rows$Contract_Cost, c(20.49, 123.45, 50.25, 10.75)))
  .expect("manual override marks only the selected row as overridden",
          identical(overridden_rows$.step2_has_override, c(FALSE, TRUE, FALSE, FALSE)))

  arm_override_rows <- step2_filter_rows(
    step2_strip_state_columns(overridden_rows),
    study_arm_filter = "Arm A",
    visit_filter = STEP2_FILTER_ALL_VISITS,
    activity_search = ""
  )
  .expect("manual override survives study arm filters",
          identical(arm_override_rows$Contract_Cost, c(123.45, 50.25)))

  visit_override_rows <- step2_filter_rows(
    step2_strip_state_columns(overridden_rows),
    study_arm_filter = "Arm A",
    visit_filter = "VISIT - 001",
    activity_search = ""
  )
  .expect("manual override survives visit filters",
          identical(visit_override_rows$Contract_Cost, c(123.45, 50.25)))

  search_override_rows <- step2_filter_rows(
    step2_strip_state_columns(overridden_rows),
    study_arm_filter = STEP2_FILTER_ALL_ARMS,
    visit_filter = STEP2_FILTER_ALL_VISITS,
    activity_search = "CONSENT"
  )
  .expect("manual override survives activity search filters",
          identical(search_override_rows$Contract_Cost, 123.45))

  reset_rows <- step2_reset_contract_cost_mode(
    overridden_rows,
    use_unrounded_cost = TRUE
  )
  .expect("mode toggle clears manual override flags",
          identical(reset_rows$.step2_has_override, c(FALSE, FALSE, FALSE, FALSE)))
  .expect("mode toggle recalculates every row from the selected mode",
          identical(reset_rows$Contract_Cost, override_rows$ICT_Cost))

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .passed, "    FAILED: ", .failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")
  invisible(list(passed = .passed, failed = .failed))
}
