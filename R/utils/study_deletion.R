suppressPackageStartupMessages({
  library(DBI)
})

validate_study_run_identity <- function(cpms_id, study_site, scenario_id) {
  values <- list(
    cpms_id = cpms_id,
    study_site = study_site,
    scenario_id = scenario_id
  )

  for (nm in names(values)) {
    value <- values[[nm]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
      stop("validate_study_run_identity(): `", nm, "` must be a non-empty single string.")
    }
    values[[nm]] <- trimws(value)
  }

  values
}

delete_study_run <- function(cpms_id, study_site, scenario_id, con = CON, delete_files = TRUE) {
  run_ref <- validate_study_run_identity(cpms_id, study_site, scenario_id)

  meta <- dbGetQuery(
    con,
    paste(
      "SELECT id, saved_file_path, edge_zip_path",
      "FROM meta_data",
      "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
    ),
    params = unname(run_ref)
  )

  upload_ids <- unique(as.character(meta$id[!is.na(meta$id)]))
  file_paths <- unique(c(meta$saved_file_path, meta$edge_zip_path))
  file_paths <- file_paths[!is.na(file_paths) & nzchar(trimws(file_paths))]

  counts <- list(
    addon_custom_activities = 0L,
    posting_lines = 0L,
    ict_costing_tbl = 0L,
    app_logs = 0L,
    meta_data = 0L
  )

  dbWithTransaction(con, {
    if (dbExistsTable(con, "addon_custom_activities")) {
      counts$addon_custom_activities <- as.integer(dbExecute(
        con,
        paste(
          "DELETE FROM addon_custom_activities",
          "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
        ),
        params = unname(run_ref)
      ))
    }

    if (dbExistsTable(con, "posting_lines")) {
      counts$posting_lines <- as.integer(dbExecute(
        con,
        paste(
          "DELETE FROM posting_lines",
          "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
        ),
        params = unname(run_ref)
      ))
    }

    if (dbExistsTable(con, "ict_costing_tbl")) {
      counts$ict_costing_tbl <- as.integer(dbExecute(
        con,
        paste(
          "DELETE FROM ict_costing_tbl",
          "WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?"
        ),
        params = unname(run_ref)
      ))
    }

    if (dbExistsTable(con, "app_logs") && length(upload_ids) > 0) {
      counts$app_logs <- sum(vapply(upload_ids, function(upload_id) {
        as.integer(dbExecute(
          con,
          "DELETE FROM app_logs WHERE upload_id = ?",
          params = list(upload_id)
        ))
      }, integer(1)))
    }

    counts$meta_data <- as.integer(dbExecute(
      con,
      paste(
        "DELETE FROM meta_data",
        "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
      ),
      params = unname(run_ref)
    ))
  })

  file_results <- list(
    deleted = character(0),
    missing = character(0),
    failed = character(0)
  )

  if (isTRUE(delete_files) && length(file_paths) > 0) {
    for (path in file_paths) {
      normalized_path <- trimws(as.character(path))

      if (!file.exists(normalized_path)) {
        file_results$missing <- c(file_results$missing, normalized_path)
        next
      }

      deleted_ok <- tryCatch({
        unlink(normalized_path, force = TRUE)
        !file.exists(normalized_path)
      }, error = function(e) {
        FALSE
      })

      if (isTRUE(deleted_ok)) {
        file_results$deleted <- c(file_results$deleted, normalized_path)
      } else {
        file_results$failed <- c(file_results$failed, normalized_path)
      }
    }
  }

  list(
    run_ref = run_ref,
    upload_ids = upload_ids,
    counts = counts,
    files = file_results,
    total_rows_deleted = sum(unlist(counts), na.rm = TRUE)
  )
}
