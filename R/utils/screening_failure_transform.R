suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

.SCREENING_FAILURE_SUFFIX   <- " - SCREENING FAILURE"
.ITEMISED_EDGE_ARMS         <- c("SSP")
.SCREENING_FAILURE_EXCLUDES <- c(
  "Unscheduled Activities",
  "Setup & Closedown",
  "Pharmacy",
  "Custom Activities"
)

is_screening_failure_sheet <- function(sheet_name) {
  str_ends(coalesce(as.character(sheet_name), ""), fixed(.SCREENING_FAILURE_SUFFIX))
}

screening_failure_candidate_sheets <- function(ict) {
  sheet_names <- if (is.list(ict) && !is.data.frame(ict)) {
    names(ict)
  } else {
    as.character(ict)
  }

  sheet_names <- trimws(coalesce(as.character(sheet_names), ""))
  sheet_names <- sheet_names[nzchar(sheet_names)]

  sheet_names[
    !sheet_names %in% .SCREENING_FAILURE_EXCLUDES &
      !is_screening_failure_sheet(sheet_names)
  ]
}

resolve_screening_failure_arm <- function(ict, selected_arm = NULL) {
  candidates <- screening_failure_candidate_sheets(ict)

  if (length(candidates) == 0) {
    return(NA_character_)
  }

  if (length(selected_arm) == 0 || is.null(selected_arm)) {
    selected_arm <- ""
  } else {
    selected_arm <- trimws(as.character(selected_arm)[[1]])
    if (is.na(selected_arm)) {
      selected_arm <- ""
    }
  }

  if (nzchar(selected_arm) && selected_arm %in% candidates) {
    return(selected_arm)
  }

  candidates[[1]]
}

is_itemised_export_row <- function(sheet_name, Study_Arm) {
  Study_Arm %in% .ITEMISED_EDGE_ARMS | is_screening_failure_sheet(sheet_name)
}

is_itemised_adjustment_row <- function(Study_Arm) {
  # Screening Failure rows are deliberately not included here: they should be
  # scaled against the duplicated visit total, then rendered itemised later.
  Study_Arm %in% .ITEMISED_EDGE_ARMS
}

resolve_edge_template_arm <- function(sheet_name, Study_Arm) {
  if_else(is_itemised_export_row(sheet_name, Study_Arm), sheet_name, Study_Arm)
}

prepare_screening_failure_posting_input <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }

  required <- c("sheet_name", "Study_Arm")
  if (!all(required %in% names(df))) {
    return(df)
  }

  df |>
    mutate(
      Study_Arm = if_else(
        # Give duplicated screening rows their own main-arm identity after the
        # Step 2 cost join, so visit-level adjustment reconciles the screening
        # tab independently while template build can still render it itemised.
        is_screening_failure_sheet(sheet_name) & !is_itemised_adjustment_row(Study_Arm),
        sheet_name,
        Study_Arm
      )
    )
}

duplicate_screening_failure_sheets <- function(ict,
                                               include_screening_failure = FALSE,
                                               screening_failure_arm = NULL) {
  if (!isTRUE(include_screening_failure) || is.null(ict)) {
    return(ict)
  }

  if (!is.list(ict) || is.data.frame(ict)) {
    return(ict)
  }

  out <- ict
  target_arm <- resolve_screening_failure_arm(ict, screening_failure_arm)
  if (is.na(target_arm) || !target_arm %in% names(ict)) {
    return(out)
  }

  df <- ict[[target_arm]]
  if (is.null(df) || nrow(df) == 0 || !"Visit" %in% names(df)) {
    return(out)
  }

  visit_choices <- df |>
    dplyr::distinct(Visit) |>
    dplyr::mutate(
      visit_number = suppressWarnings(as.numeric(stringr::str_extract(Visit, "\\d+"))),
      visit_sort_missing = is.na(visit_number)
    ) |>
    dplyr::arrange(visit_sort_missing, visit_number, Visit)

  if (nrow(visit_choices) == 0) {
    return(out)
  }

  first_visit <- visit_choices$Visit[[1]]
  screening_sheet <- paste0(target_arm, .SCREENING_FAILURE_SUFFIX)
  # This is the only place Screening Failure source rows are introduced. The
  # duplicated sheet then flows through posting, adjustment, keying, and EDGE
  # template build like any other exportable sheet.
  screening_rows <- df |>
    dplyr::filter(Visit == first_visit)

  if (nrow(screening_rows) == 0) {
    return(out)
  }

  if ("SheetName" %in% names(screening_rows)) {
    screening_rows$SheetName <- screening_sheet
  }

  out[[screening_sheet]] <- screening_rows

  out
}
