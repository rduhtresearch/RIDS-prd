suppressPackageStartupMessages({
  library(DBI)
})

source("R/persistence/repositories/study_repository.R", local = FALSE)

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
  studies <- study_repository(con)

  meta <- studies$find_run_files(run_ref$cpms_id, run_ref$study_site, run_ref$scenario_id)

  upload_ids <- unique(as.character(meta$id[!is.na(meta$id)]))
  file_paths <- unique(c(meta$saved_file_path, meta$edge_zip_path))
  file_paths <- file_paths[!is.na(file_paths) & nzchar(trimws(file_paths))]

  counts <- studies$delete_run(
    run_ref$cpms_id, run_ref$study_site, run_ref$scenario_id,
    upload_ids = upload_ids
  )

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
