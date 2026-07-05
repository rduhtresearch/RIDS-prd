.step4_passed <- 0L
.step4_failed <- 0L

.step4_expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .step4_passed <<- .step4_passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .step4_failed <<- .step4_failed + 1L
  }
}

run_step4_persistence_tests <- function() {
  cat("\n=== step4 persistence tests ===\n\n")
  .step4_passed <<- 0L
  .step4_failed <<- 0L

  original_templates <- list(
    "Arm A" = data.frame(
      `Template Name` = "Arm A",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  edited_templates <- list(
    "Arm A" = data.frame(
      `Template Name` = "Arm A (Edited)",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )

  selected_edited <- step4_templates_for_export(
    edited_templates = edited_templates,
    original_templates = original_templates
  )
  .step4_expect(
    "edited templates are preferred for final persistence",
    identical(selected_edited, edited_templates)
  )

  selected_original <- step4_templates_for_export(
    edited_templates = NULL,
    original_templates = original_templates
  )
  .step4_expect(
    "original templates are used when there are no edits",
    identical(selected_original, original_templates)
  )

  selected_empty_edits <- step4_templates_for_export(
    edited_templates = list(),
    original_templates = original_templates
  )
  .step4_expect(
    "empty edited templates fall back to the original templates",
    identical(selected_empty_edits, original_templates)
  )

  .step4_expect(
    "display mode shows validation failure when failure is active",
    identical(
      step4_display_mode(
        current_step = "step4",
        templates = NULL,
        validation_failed = TRUE,
        validation_failure_latched = FALSE
      ),
      "validation_failed"
    )
  )

  .step4_expect(
    "display mode keeps validation failure latched even without templates",
    identical(
      step4_display_mode(
        current_step = "step4",
        templates = NULL,
        validation_failed = FALSE,
        validation_failure_latched = TRUE
      ),
      "validation_failed"
    )
  )

  .step4_expect(
    "display mode shows pending while step4 has no templates and no failure",
    identical(
      step4_display_mode(
        current_step = "step4",
        templates = NULL,
        validation_failed = FALSE,
        validation_failure_latched = FALSE
      ),
      "pending"
    )
  )

  .step4_expect(
    "display mode shows ready when templates exist and no failure is present",
    identical(
      step4_display_mode(
        current_step = "step4",
        templates = original_templates,
        validation_failed = FALSE,
        validation_failure_latched = FALSE
      ),
      "ready"
    )
  )

  .step4_expect(
    "preview arm keeps a valid current selection",
    identical(
      step4_effective_preview_arm("Arm A", original_templates),
      "Arm A"
    )
  )

  two_arm_templates <- c(
    original_templates,
    list(
      "Arm B" = data.frame(
        `Template Name` = "Arm B",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  )

  .step4_expect(
    "preview arm falls back to the first template when selection is blank",
    identical(
      step4_effective_preview_arm("", two_arm_templates),
      "Arm A"
    )
  )

  .step4_expect(
    "preview arm falls back to the first template when selection is invalid",
    identical(
      step4_effective_preview_arm("Missing Arm", two_arm_templates),
      "Arm A"
    )
  )

  .step4_expect(
    "preview arm returns NULL when no templates exist",
    is.null(step4_effective_preview_arm("Arm A", NULL))
  )

  .step4_expect(
    "available preview arms returns all template names in order",
    identical(
      step4_available_preview_arms(two_arm_templates),
      c("Arm A", "Arm B")
    )
  )

  .step4_expect(
    "available preview arms returns character(0) when templates are missing",
    identical(
      step4_available_preview_arms(NULL),
      character(0)
    )
  )

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .step4_passed, "    FAILED: ", .step4_failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  invisible(list(passed = .step4_passed, failed = .step4_failed))
}
