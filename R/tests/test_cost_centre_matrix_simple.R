suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(tibble)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(y)
  }
  x
}

.ccm_passed <- 0L
.ccm_failed <- 0L

.ccm_expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .ccm_passed <<- .ccm_passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .ccm_failed <<- .ccm_failed + 1L
  }
}

run_cost_centre_matrix_simple_tests <- function() {
  cat("\n=== cost centre matrix simple tests ===\n\n")
  .ccm_passed <<- 0L
  .ccm_failed <<- 0L

  source("R/utils/add_cost_centres.r")

  db_path <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
  old_con <- if (exists("CON", inherits = TRUE)) get("CON", inherits = TRUE) else NULL
  assign("CON", con, envir = .GlobalEnv)

  on.exit({
    if (is.null(old_con)) {
      rm("CON", envir = .GlobalEnv)
    } else {
      assign("CON", old_con, envir = .GlobalEnv)
    }
    dbDisconnect(con, shutdown = TRUE)
    if (file.exists(db_path)) unlink(db_path)
  }, add = TRUE)

  dbExecute(con, "CREATE TABLE app_settings (key TEXT PRIMARY KEY, value TEXT NOT NULL)")
  dbExecute(con, "CREATE TABLE posting_line_types (posting_line_type_id TEXT PRIMARY KEY, label TEXT)")
  dbExecute(con, "
    INSERT INTO posting_line_types (posting_line_type_id, label) VALUES
    ('DIRECT', 'Direct'),
    ('INDIRECT_25_PI', 'Indirect PI'),
    ('INDIRECT_25_TRUST', 'Indirect Trust'),
    ('INDIRECT_50_DELIVERY', 'Indirect Delivery'),
    ('CAPACITY_RD', 'Capacity'),
    ('DIRECT_40_PI', 'TRD40'),
    ('DIRECT_60_TEAM', 'TRD60'),
    ('MFF_SPLIT_NEW_CC', 'MFF split')
  ")

  matrix_path <- tempfile(fileext = ".csv")
  write.csv(
    tibble(
      Department = c("Study Team", "Study Team", "Study Team"),
      `Activity Type` = c("Baseline", "Baseline", "Training"),
      `Staff Role` = c("Admin/Data Entry", "Medical Staff", "Medical Staff"),
      Tab = c("A", "A", "B"),
      Notes = c("", "", "Training Fee"),
      DIRECT_COST = c("50007", "Speciality", "99999"),
      `INDIRECT_25 [PI CB]` = c("", "Speciality", ""),
      TRD40 = c("", "70040", ""),
      MFF_CRF = c("81000", "", "")
    ),
    file = matrix_path,
    row.names = FALSE
  )

  valid <- validate_cost_centre_matrix_file(matrix_path)
  .ccm_expect("valid matrix CSV passes validation", isTRUE(valid$valid))
  .ccm_expect("validation detects split columns", length(valid$split_columns) == 4L)
  .ccm_expect("non-split columns are ignored", !"Tab" %in% valid$split_columns)

  dbExecute(con, "INSERT INTO app_settings (key, value) VALUES ('cost_centre_matrix_file', ?)", params = list(matrix_path))

  rows <- tibble(
    Department = c("study team", "STUDY TEAM", "Study Team", "Study Team", "Study Team"),
    activity_type = c("baseline", "BASELINE", "Baseline", "Training", "Baseline"),
    Staff_Role = c("admin/data entry", "MEDICAL STAFF", "Medical Staff", "Medical Staff", "Admin/Data Entry"),
    posting_line_type_id = c("direct", "indirect_25_pi", "CAPACITY_RD", "DIRECT", "MFF_SPLIT_NEW_CC"),
    cost_code = NA_character_
  )

  resolved <- add_cost_centres(rows, "Cancer")
  summary <- attr(resolved, "cost_centre_assignment_summary")
  unmatched_report <- attr(resolved, "cost_centre_unmatched_report")

  .ccm_expect("direct alias maps correctly", identical(resolved$cost_code[[1]], "50007"))
  .ccm_expect("speciality token resolves from step 1 speciality", identical(resolved$cost_code[[2]], "58109"))
  .ccm_expect("case-insensitive department join works", identical(resolved$cost_code[[1]], "50007"))
  .ccm_expect("case-insensitive activity type join works", identical(resolved$cost_code[[1]], "50007"))
  .ccm_expect("case-insensitive staff role join works", identical(resolved$cost_code[[1]], "50007"))
  .ccm_expect("case-insensitive split type join works", identical(resolved$cost_code[[1]], "50007"))
  .ccm_expect("MFF_CRF alias maps to MFF split posting line type", identical(resolved$cost_code[[5]], "81000"))
  .ccm_expect("unmatched non-speciality rows remain NA", is.na(resolved$cost_code[[3]]))
  .ccm_expect("training fee rows are excluded from matching", is.na(resolved$cost_code[[4]]))
  .ccm_expect("summary tracks unmatched rows", identical(summary$unmatched_rows, 2L))
  .ccm_expect("unmatched report includes unmatched rows", nrow(unmatched_report) == 2L)
  .ccm_expect(
    "normalized join helper columns are not returned for persistence",
    !any(c(
      "Department_join",
      "activity_type_join",
      "Staff_Role_join",
      "posting_line_type_join"
    ) %in% names(resolved))
  )
  .ccm_expect(
    "unmatched report uses normalized join fields",
    identical(
      names(unmatched_report),
      c("Department", "activity_type", "Staff_Role", "posting_line_type_id")
    )
  )

  missing_speciality_error <- tryCatch({
    add_cost_centres(rows[2, , drop = FALSE], "Unknown Speciality")
    NULL
  }, error = function(e) conditionMessage(e))
  .ccm_expect("unknown speciality errors clearly", grepl("No speciality cost centre mapping found", missing_speciality_error, fixed = TRUE))

  dbExecute(con, "DELETE FROM app_settings WHERE key = 'cost_centre_matrix_file'")
  no_matrix_error <- tryCatch({
    add_cost_centres(rows, "Cancer")
    NULL
  }, error = function(e) conditionMessage(e))
  .ccm_expect("missing configured matrix errors clearly", grepl("No cost centre matrix has been configured", no_matrix_error, fixed = TRUE))

  dup_path <- tempfile(fileext = ".csv")
  write.csv(
    tibble(
      Department = c("Study Team", "Study Team"),
      `Activity Type` = c("Baseline", "Baseline"),
      `Staff Role` = c("Admin/Data Entry", "Admin/Data Entry"),
      DIRECT = c("50007", "50008")
    ),
    file = dup_path,
    row.names = FALSE
  )
  dup_validation <- validate_cost_centre_matrix_file(dup_path)
  .ccm_expect("duplicate keys fail validation", !isTRUE(dup_validation$valid))

  identical_dup_path <- tempfile(fileext = ".csv")
  write.csv(
    tibble(
      Department = c("Study Team", "Study Team"),
      `Activity Type` = c("Baseline", "Baseline"),
      `Staff Role` = c("Admin/Data Entry", "Admin/Data Entry"),
      DIRECT = c("50007", "50007")
    ),
    file = identical_dup_path,
    row.names = FALSE
  )
  identical_dup_validation <- validate_cost_centre_matrix_file(identical_dup_path)
  .ccm_expect("identical duplicate rows are allowed", isTRUE(identical_dup_validation$valid))

  missing_cols_path <- tempfile(fileext = ".csv")
  write.csv(
    tibble(
      Department = "Study Team",
      DIRECT = "50007"
    ),
    file = missing_cols_path,
    row.names = FALSE
  )
  missing_cols_validation <- validate_cost_centre_matrix_file(missing_cols_path)
  .ccm_expect("missing required columns fail validation", !isTRUE(missing_cols_validation$valid))

  list(passed = .ccm_passed, failed = .ccm_failed)
}
