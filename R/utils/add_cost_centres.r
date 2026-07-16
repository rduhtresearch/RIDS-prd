suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

cc_normalize_text <- function(x) {
  stringr::str_to_lower(stringr::str_squish(as.character(x)))
}

SPECIALITY_CC_LOOKUP <- tribble(
  ~speciality,                    ~speciality_cost_centre,
  "Cardiology",                   "52000",
  "Paediatric",                   "53100",
  "Cancer",                       "58109",
  "Orthopedics & Rheumatology",   "59400",
  "Gastro",                       "61105",
  "Respiratory",                  "62100",
  "Renal",                        "63106",
  "Urology",                      "64100",
  "Stroke",                       "66206",
  "Geriatric",                    "66300",
  "Dendron",                      "67103",
  "Dermatology",                  "67302",
  "ED",                           "69101"
) %>%
  mutate(speciality_clean = cc_normalize_text(speciality))

COST_CENTRE_MATRIX_REQUIRED_COLUMNS <- c("Department", "Activity Type", "Staff Role")
COST_CENTRE_MATRIX_SPECIALITY_TOKEN <- cc_normalize_text("Speciality")
COST_CENTRE_MATRIX_EXCLUDED_NOTES <- cc_normalize_text(c("Training Fee", "Inflight Training Fee"))
COST_CENTRE_MATRIX_SUPPORTED_EXTENSIONS <- c("csv", "xlsx")
COST_CENTRE_MATRIX_COLUMN_ALIASES <- c(
  "Activity.Type" = "Activity Type",
  "Staff.Role" = "Staff Role",
  "DIRECT_COST" = "DIRECT",
  "INDIRECT_25 [O/Hs]" = "INDIRECT_25_TRUST",
  "INDIRECT_25.[O/Hs]" = "INDIRECT_25_TRUST",
  "INDIRECT_25 [PI CB]" = "INDIRECT_25_PI",
  "INDIRECT_25.[PI.CB]" = "INDIRECT_25_PI",
  "INDIRECT_50" = "INDIRECT_50_DELIVERY",
  "RD_CAPACITY" = "CAPACITY_RD",
  "TRD40" = "DIRECT_40_PI",
  "TRD60" = "DIRECT_60_TEAM",
  "MFF_CRF" = "MFF_SPLIT_NEW_CC"
)

cc_get_setting <- function(key, default = "") {
  value <- tryCatch({
    rids_repos()$settings$find_value(key)
  }, error = function(e) {
    character(0)
  })

  if (length(value) == 0 || is.na(value[[1]]) || !nzchar(trimws(value[[1]]))) {
    return(default)
  }

  as.character(value[[1]])
}


cc_resolve_speciality_cost_centre <- function(study_speciality) {
  target_speciality <- cc_normalize_text(study_speciality %||% "")

  if (!nzchar(target_speciality)) {
    stop("No speciality was supplied for cost centre resolution.")
  }

  matches <- SPECIALITY_CC_LOOKUP %>%
    filter(.data$speciality_clean == .env$target_speciality) %>%
    pull(.data$speciality_cost_centre)

  if (length(matches) == 0) {
    stop("No speciality cost centre mapping found for '", target_speciality, "'.")
  }

  matches[[1]]
}

cc_allowed_posting_line_types <- function() {
  rids_repos()$rules$posting_line_type_ids()
}

cc_apply_column_aliases <- function(df) {
  alias_hits <- intersect(names(COST_CENTRE_MATRIX_COLUMN_ALIASES), names(df))
  if (length(alias_hits) == 0) {
    return(df)
  }

  rename_targets <- stats::setNames(
    alias_hits,
    unname(COST_CENTRE_MATRIX_COLUMN_ALIASES[alias_hits])
  )

  dplyr::rename(df, !!!rename_targets)
}

cc_matrix_file_extension <- function(file_path, file_name = NULL) {
  display_name <- file_name %||% basename(file_path)
  extension <- tolower(tools::file_ext(display_name))

  if (!nzchar(extension)) {
    extension <- tolower(tools::file_ext(file_path))
  }

  extension
}

cc_read_cost_centre_matrix <- function(file_path, file_name = NULL) {
  if (is.na(file_path) || !nzchar(file_path) || !file.exists(file_path)) {
    stop("Cost centre matrix file not found.")
  }

  extension <- cc_matrix_file_extension(file_path, file_name)
  if (!extension %in% COST_CENTRE_MATRIX_SUPPORTED_EXTENSIONS) {
    stop("Cost centre matrix must be a CSV or XLSX file.")
  }

  matrix_df <- if (identical(extension, "xlsx")) {
    openxlsx::read.xlsx(
      file_path,
      sheet = 1,
      check.names = FALSE,
      skipEmptyRows = FALSE,
      skipEmptyCols = FALSE
    )
  } else {
    read.csv(
      file_path,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      colClasses = "character"
    )
  }

  matrix_df <- tibble::as_tibble(matrix_df)
  matrix_df[] <- lapply(matrix_df, as.character)
  matrix_df
}

cc_prepare_matrix_rules <- function(df, allowed_posting_line_types = NULL) {
  allowed_posting_line_types <- allowed_posting_line_types %||% cc_allowed_posting_line_types()
  df <- cc_apply_column_aliases(df)

  missing_required <- setdiff(COST_CENTRE_MATRIX_REQUIRED_COLUMNS, names(df))
  if (length(missing_required) > 0) {
    stop(
      "Cost centre matrix missing required columns: ",
      paste(missing_required, collapse = ", ")
    )
  }

  split_cols <- intersect(names(df), allowed_posting_line_types)
  if (length(split_cols) == 0) {
    stop("Cost centre matrix must contain at least one recognised split column.")
  }

  prepared <- df %>%
    mutate(across(all_of(split_cols), as.character)) %>%
    mutate(
      Department = cc_normalize_text(.data$Department),
      `Activity Type` = cc_normalize_text(.data$`Activity Type`),
      `Staff Role` = cc_normalize_text(.data$`Staff Role`),
      Notes = if ("Notes" %in% names(.)) cc_normalize_text(.data$Notes) else NA_character_
    ) %>%
    pivot_longer(
      cols = all_of(split_cols),
      names_to = "posting_line_type_id",
      values_to = "matrix_cost_code"
    ) %>%
    mutate(
      posting_line_type_id = cc_normalize_text(.data$posting_line_type_id),
      matrix_cost_code = cc_normalize_text(.data$matrix_cost_code)
    ) %>%
    filter(!is.na(.data$matrix_cost_code), .data$matrix_cost_code != "") %>%
    distinct(
      .data$Department,
      .data$`Activity Type`,
      .data$`Staff Role`,
      .data$posting_line_type_id,
      .data$matrix_cost_code,
      .keep_all = TRUE
    )

  duplicate_rows <- prepared %>%
    count(
      .data$Department,
      .data$`Activity Type`,
      .data$`Staff Role`,
      .data$posting_line_type_id,
      name = "duplicate_count"
    ) %>%
    filter(.data$duplicate_count > 1)

  if (nrow(duplicate_rows) > 0) {
    stop("Duplicate matrix keys detected after pivoting.")
  }

  list(
    rules = prepared,
    split_columns = split_cols
  )
}

cc_inspect_cost_centre_matrix <- function(file_path,
                                          allowed_posting_line_types = NULL,
                                          file_name = NULL) {
  matrix_df <- cc_read_cost_centre_matrix(file_path, file_name = file_name)
  prepared <- cc_prepare_matrix_rules(
    matrix_df,
    allowed_posting_line_types = allowed_posting_line_types
  )
  canonical_df <- cc_apply_column_aliases(matrix_df)

  populated_counts <- vapply(prepared$split_columns, function(column_name) {
    values <- trimws(canonical_df[[column_name]])
    as.integer(sum(!is.na(values) & nzchar(values)))
  }, integer(1))

  list(
    data = matrix_df,
    rules = prepared$rules,
    split_columns = prepared$split_columns,
    populated_counts = populated_counts,
    row_count = nrow(matrix_df),
    column_count = ncol(matrix_df),
    file_extension = cc_matrix_file_extension(file_path, file_name)
  )
}

validate_cost_centre_matrix_file <- function(file_path,
                                             allowed_posting_line_types = NULL,
                                             file_name = NULL) {
  tryCatch({
    inspection <- cc_inspect_cost_centre_matrix(
      file_path,
      allowed_posting_line_types = allowed_posting_line_types,
      file_name = file_name
    )

    list(
      valid = TRUE,
      message = sprintf(
        "Valid matrix: %s split columns detected.",
        length(inspection$split_columns)
      ),
      split_columns = inspection$split_columns,
      populated_counts = inspection$populated_counts,
      row_count = inspection$row_count,
      column_count = inspection$column_count,
      file_extension = inspection$file_extension
    )
  }, error = function(e) {
    list(
      valid = FALSE,
      message = conditionMessage(e),
      split_columns = character(),
      populated_counts = integer(),
      row_count = 0L,
      column_count = 0L,
      file_extension = ""
    )
  })
}

cc_get_active_matrix_config <- function() {
  file_path <- cc_get_setting("cost_centre_matrix_file", "")

  if (!nzchar(file_path)) {
    stop("No cost centre matrix has been configured.")
  }

  list(file_path = file_path)
}

cc_load_active_matrix_rules <- function() {
  config <- cc_get_active_matrix_config()
  validation <- validate_cost_centre_matrix_file(config$file_path)

  if (!isTRUE(validation$valid)) {
    stop(validation$message)
  }

  matrix_df <- cc_read_cost_centre_matrix(config$file_path)
  cc_prepare_matrix_rules(matrix_df)$rules
}

cc_build_unmatched_report <- function(resolved) {
  report_columns <- intersect(
    c(
      "cpms_id",
      "study_site",
      "scenario_id",
      "row_id",
      "sheet_name",
      "Study_Arm",
      "Activity",
      "Visit",
      "Department_join",
      "activity_type_join",
      "Staff_Role_join",
      "posting_line_type_join"
    ),
    names(resolved)
  )

  if (length(report_columns) == 0) {
    return(tibble::tibble())
  }

  resolved %>%
    filter(is.na(.data$cost_code)) %>%
    select(all_of(report_columns)) %>%
    rename(
      Department = any_of("Department_join"),
      activity_type = any_of("activity_type_join"),
      Staff_Role = any_of("Staff_Role_join"),
      posting_line_type_id = any_of("posting_line_type_join")
    )
}

#' Attach cost_code (cost centre) to each posting line using the configured matrix.
#'
#' @param posting_output Posting lines dataframe, post `adjust_posting_lines()`
#'   and post `rename(Staff_Role = Staff.Role)`.
#' @param study_speciality The speciality name selected in step 1.
#' @return Same dataframe with `cost_code` populated and a summary attribute.
add_cost_centres <- function(posting_output, study_speciality) {
  required <- c("Department", "Staff_Role", "posting_line_type_id", "activity_type")
  join_helper_columns <- c(
    "Department_join",
    "activity_type_join",
    "Staff_Role_join",
    "posting_line_type_join"
  )
  missing <- setdiff(required, names(posting_output))
  if (length(missing) > 0) {
    stop("add_cost_centres(): missing columns: ", paste(missing, collapse = ", "))
  }

  rules <- cc_load_active_matrix_rules()
  speciality_cost_centre <- cc_resolve_speciality_cost_centre(study_speciality)

  resolved <- posting_output %>%
    mutate(
      Department_join = cc_normalize_text(.data$Department),
      activity_type_join = cc_normalize_text(.data$activity_type),
      Staff_Role_join = cc_normalize_text(.data$Staff_Role),
      posting_line_type_join = cc_normalize_text(.data$posting_line_type_id)
    ) %>%
    left_join(
      rules %>%
        filter(is.na(.data$Notes) | !.data$Notes %in% COST_CENTRE_MATRIX_EXCLUDED_NOTES) %>%
        select(
          "Department",
          "Activity Type",
          "Staff Role",
          "posting_line_type_id",
          "matrix_cost_code"
        ),
      by = c(
        "Department_join" = "Department",
        "activity_type_join" = "Activity Type",
        "Staff_Role_join" = "Staff Role",
        "posting_line_type_join" = "posting_line_type_id"
      )
    ) %>%
    mutate(
      cost_code = case_when(
        .data$matrix_cost_code == COST_CENTRE_MATRIX_SPECIALITY_TOKEN ~ speciality_cost_centre,
        !is.na(.data$matrix_cost_code) ~ .data$matrix_cost_code,
        TRUE ~ NA_character_
      )
    ) %>%
    select(
      -all_of(c(
        "matrix_cost_code"
      ))
    )

  unmatched_report <- cc_build_unmatched_report(resolved)

  output <- resolved %>%
    select(-any_of(join_helper_columns))

  attr(output, "cost_centre_assignment_summary") <- list(
    total_rows = nrow(resolved),
    matched_rows = sum(!is.na(resolved$cost_code)),
    unmatched_rows = sum(is.na(resolved$cost_code))
  )
  attr(output, "cost_centre_unmatched_report") <- unmatched_report

  output
}
