# ==============================================================================
# tests/test_edge_builder_module.R
#
# Focused regression tests for template builder move-target and row-move logic.
#
# Usage:
#   source("R/modules/edge_builder_mod.R")
#   source("R/tests/test_edge_builder_module.R")
#   run_edge_builder_module_tests()
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
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

.edge_template <- function(template_name, departments, descriptions) {
  tibble(
    `Template Name` = rep(template_name, length(descriptions)),
    Department = departments,
    `Cost Item Description` = descriptions,
    `Default Cost` = seq_along(descriptions) * 10,
    `Analysis Code` = paste0("CODE-", seq_along(descriptions))
  )
}

run_edge_builder_module_tests <- function() {
  cat("\n=== edge_builder module tests ===\n\n")
  .passed <<- 0L
  .failed <<- 0L

  new_sentinel <- "__new__"

  cat("[ movable template detection ]\n")
  setup_tpl <- .edge_template(
    "Setup & Closedown",
    c("  DIRECT  ", "", NA_character_),
    c("Set-up visit", "Blank row", "Trailing row")
  )
  main_tpl <- .edge_template(
    "Main Arm",
    c(NA_character_, "   "),
    c("VISIT - 001", "VISIT - 002")
  )
  tpls <- list(
    `Main Arm` = main_tpl,
    `Setup & Closedown` = setup_tpl
  )
  movable <- edge_builder_compute_movable(tpls)

  .expect("Setup & Closedown is movable with departmental rows after trimming",
          identical(movable, "Setup & Closedown"))
  .expect("Main Arm stays read-only when Department is blank/NA only",
          !("Main Arm" %in% movable))

  cat("\n[ move target choices ]\n")
  only_new_choice <- edge_builder_move_target_choices(
    active = "Setup & Closedown",
    movable = movable,
    new_sentinel = new_sentinel
  )
  .expect("single movable source still offers named tab creation",
          identical(unname(only_new_choice), new_sentinel))
  .expect("single movable source labels the choice as + New template...",
          identical(names(only_new_choice), "+ New template..."))

  multi_choices <- edge_builder_move_target_choices(
    active = "Setup & Closedown",
    movable = c("Setup & Closedown", "Pharmacy"),
    new_sentinel = new_sentinel
  )
  .expect("existing movable targets remain available",
          identical(unname(multi_choices), c("Pharmacy", new_sentinel)))
  .expect("existing target keeps its own label alongside new-template option",
          identical(names(multi_choices), c("Pharmacy", "+ New template...")))

  readonly_choices <- edge_builder_move_target_choices(
    active = "Main Arm",
    movable = movable,
    new_sentinel = new_sentinel
  )
  .expect("read-only source does not offer any move targets",
          length(readonly_choices) == 0L)

  cat("\n[ new name validation ]\n")
  dup_check <- edge_builder_validate_new_name("Main Arm", names(tpls))
  blank_check <- edge_builder_validate_new_name("   ", names(tpls))
  valid_check <- edge_builder_validate_new_name("Safety Follow-up", names(tpls))

  .expect("duplicate new template names are rejected",
          identical(dup_check$msg, "Name already used") && !dup_check$valid)
  .expect("blank new template names are rejected",
          identical(blank_check$msg, "Required") && !blank_check$valid)
  .expect("unique new template names are accepted",
          isTRUE(valid_check$valid) && identical(valid_check$name, "Safety Follow-up"))

  cat("\n[ department filter and A/Z sorting ]\n")
  filter_tpl <- .edge_template(
    "Setup & Closedown",
    c("  Radiology  ", "", NA_character_, "Pathology", "Radiology"),
    c("Zulu scan", "alpha admin", "Echo review", "bravo bloods", "Alpha follow-up")
  )
  department_choices <- edge_builder_department_choices(filter_tpl)
  .expect("department choices include all, sorted departments, and blank department",
          identical(
            unname(department_choices),
            c(EDGE_BUILDER_DEPT_ALL, "Pathology", "Radiology", EDGE_BUILDER_DEPT_NONE)
          ) &&
            identical(
              names(department_choices),
              c("All departments", "Pathology", "Radiology", "(No department)")
            ))

  radiology_rows <- edge_builder_filter_sort_rows(
    filter_tpl,
    department_filter = "Radiology",
    sort_order = EDGE_BUILDER_SORT_NONE
  )
  .expect("department filter matches trimmed department values",
          identical(
            radiology_rows$`Cost Item Description`,
            c("Zulu scan", "Alpha follow-up")
          ))
  .expect("filtered rows retain original source indices",
          identical(radiology_rows$.edge_builder_source_index, c(1L, 5L)))

  blank_department_rows <- edge_builder_filter_sort_rows(
    filter_tpl,
    department_filter = EDGE_BUILDER_DEPT_NONE,
    sort_order = EDGE_BUILDER_SORT_NONE
  )
  .expect("blank department filter includes empty and NA departments",
          identical(blank_department_rows$.edge_builder_source_index, c(2L, 3L)))

  az_rows <- edge_builder_filter_sort_rows(
    filter_tpl,
    department_filter = EDGE_BUILDER_DEPT_ALL,
    sort_order = EDGE_BUILDER_SORT_ASC
  )
  .expect("description A-Z sorting is case-insensitive",
          identical(
            az_rows$`Cost Item Description`,
            c("alpha admin", "Alpha follow-up", "bravo bloods", "Echo review", "Zulu scan")
          ))

  za_rows <- edge_builder_filter_sort_rows(
    filter_tpl,
    department_filter = EDGE_BUILDER_DEPT_ALL,
    sort_order = EDGE_BUILDER_SORT_DESC
  )
  .expect("description Z-A sorting is supported",
          identical(
            za_rows$`Cost Item Description`,
            c("Zulu scan", "Echo review", "bravo bloods", "Alpha follow-up", "alpha admin")
          ))

  filtered_sorted_rows <- edge_builder_filter_sort_rows(
    filter_tpl,
    department_filter = "Radiology",
    sort_order = EDGE_BUILDER_SORT_ASC
  )
  .expect("filter plus sort keeps visible rows mapped to source rows",
          identical(
            filtered_sorted_rows$.edge_builder_source_index,
            c(5L, 1L)
          ))

  filtered_move_templates <- list(
    `Setup & Closedown` = filter_tpl,
    `Safety Follow-up` = filter_tpl[0, , drop = FALSE]
  )
  filtered_move <- edge_builder_move_rows(
    templates = filtered_move_templates,
    source = "Setup & Closedown",
    target = "Safety Follow-up",
    indices = filtered_sorted_rows$.edge_builder_source_index[[1]]
  )
  .expect("moving from a filtered/sorted view moves the intended source row",
          identical(
            filtered_move$`Safety Follow-up`$`Cost Item Description`,
            "Alpha follow-up"
          ))

  cat("\n[ row moves into a new tab ]\n")
  templates_for_move <- list(
    `Main Arm` = main_tpl,
    `Setup & Closedown` = setup_tpl,
    `Safety Follow-up` = setup_tpl[0, , drop = FALSE]
  )
  moved_templates <- edge_builder_move_rows(
    templates = templates_for_move,
    source = "Setup & Closedown",
    target = "Safety Follow-up",
    indices = 1L
  )

  .expect("source row count decreases after move",
          nrow(moved_templates$`Setup & Closedown`) == nrow(setup_tpl) - 1L)
  .expect("new tab receives the moved row",
          nrow(moved_templates$`Safety Follow-up`) == 1L)
  .expect("moved row is retagged with the new template name",
          identical(moved_templates$`Safety Follow-up`$`Template Name`, "Safety Follow-up"))
  .expect("moved row preserves its original description",
          identical(moved_templates$`Safety Follow-up`$`Cost Item Description`, "Set-up visit"))

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .passed, "    FAILED: ", .failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")
  invisible(list(passed = .passed, failed = .failed))
}
