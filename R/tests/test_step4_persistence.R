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

  empty_template <- original_templates[[1]][0, , drop = FALSE]
  filtered_templates <- step4_filter_export_templates(list(
    "Missing" = NULL,
    "Empty" = empty_template,
    "Arm A" = original_templates[[1]]
  ))
  .step4_expect(
    "export filtering drops missing and empty templates without reordering",
    identical(filtered_templates, original_templates)
  )

  department_templates <- list(
    "Arm A" = data.frame(
      Department = c("Research", "Pharmacy"),
      Value = c(1L, 2L),
      stringsAsFactors = FALSE
    ),
    "Arm B" = data.frame(
      Value = 3L,
      stringsAsFactors = FALSE
    )
  )
  blanked_templates <- step4_blank_export_departments(department_templates)
  .step4_expect(
    "export preparation blanks internal departments",
    identical(blanked_templates[["Arm A"]]$Department, c(NA, NA))
  )
  .step4_expect(
    "department blanking leaves templates without that column unchanged",
    identical(blanked_templates[["Arm B"]], department_templates[["Arm B"]])
  )
  .step4_expect(
    "department blanking does not mutate its input",
    identical(department_templates[["Arm A"]]$Department, c("Research", "Pharmacy"))
  )

  amendment_templates <- list(
    "Arm A" = data.frame(
      `Template Name` = "Arm A",
      `Analysis Code` = "EDGE-0001",
      Department = "Research",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  amended_templates <- step4_apply_amendment_export_rules(
    amendment_templates,
    version_type = "substantial_amendment",
    effective_from_date = as.Date("2026-07-10"),
    version_number = 3L
  )
  amendment_name <- "Arm A [SUBSTANTIAL AMENDMENT - 10 Jul 2026]"
  .step4_expect(
    "export rules qualify amendment template names",
    identical(names(amended_templates), amendment_name) &&
      identical(amended_templates[[1]][["Template Name"]], amendment_name)
  )
  .step4_expect(
    "export rules qualify amendment analysis codes",
    identical(amended_templates[[1]][["Analysis Code"]], "V3-EDGE-0001")
  )

  zip_target <- tempfile("step4_export_test_", fileext = ".zip")
  zip_extract_dir <- tempfile("step4_export_contents_")
  dir.create(zip_extract_dir, recursive = TRUE)
  on.exit(unlink(c(zip_target, zip_extract_dir), recursive = TRUE), add = TRUE)

  zip_templates <- list(
    "Arm A" = data.frame(
      `Template Name` = "Arm A",
      `Analysis Code` = "EDGE-0001",
      Department = "Research",
      Value = 1L,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    "Empty Arm" = data.frame(
      `Template Name` = character(),
      `Analysis Code` = character(),
      Department = character(),
      Value = integer(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  written_zip <- step4_write_export_zip(
    zip_templates,
    zip_path = zip_target,
    version_type = "baseline",
    effective_from_date = NULL,
    version_number = NULL
  )
  zip_members <- utils::unzip(zip_target, list = TRUE)$Name
  utils::unzip(zip_target, exdir = zip_extract_dir)
  csv_lines <- readLines(file.path(zip_extract_dir, "Arm_A.csv"), warn = FALSE)

  .step4_expect(
    "ZIP writing returns the requested path and creates a non-empty archive",
    identical(written_zip, zip_target) &&
      file.exists(zip_target) &&
      file.info(zip_target)$size > 0
  )
  .step4_expect(
    "ZIP writing filters empty templates and preserves export filenames",
    identical(zip_members, "Arm_A.csv")
  )
  .step4_expect(
    "ZIP CSV content preserves columns and blanks Department",
    identical(
      csv_lines,
      c(
        "\"Template Name\",\"Analysis Code\",\"Department\",\"Value\"",
        "\"Arm A\",\"EDGE-0001\",,1"
      )
    )
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
