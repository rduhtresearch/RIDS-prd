# posting_lines repository. All SQL for the posting_lines table lives here.

posting_line_repository <- function(con) {
  has_version_id <- function() {
    "version_id" %in% tolower(DBI::dbListFields(con, "posting_lines"))
  }

  require_version_id <- function(version_id, operation) {
    if (has_version_id() &&
        (is.null(version_id) || length(version_id) != 1L || is.na(version_id))) {
      stop(operation, " requires a template version ID.")
    }
    version_id
  }

  has_arm_identity <- function() {
    "arm_identity" %in% tolower(DBI::dbListFields(con, "posting_lines"))
  }

  prepare_posting_lines <- function(df, version_id = NULL) {
    fields <- tolower(DBI::dbListFields(con, "posting_lines"))
    table_has_arm_identity <- has_arm_identity()
    if (table_has_arm_identity && !"Arm_Identity" %in% names(df)) {
      df$Arm_Identity <- df$Study_Arm
    }
    if (!table_has_arm_identity && "Arm_Identity" %in% names(df)) {
      df$Arm_Identity <- NULL
    }
    if (!"activity_occurrence_id" %in% fields && "activity_occurrence_id" %in% names(df)) {
      df$activity_occurrence_id <- NULL
    }
    if (has_version_id()) {
      if (!is.null(version_id)) df$version_id <- as.integer(version_id)
    } else if ("version_id" %in% names(df)) {
      df$version_id <- NULL
    }
    df
  }

  list(
    count_for_run = function(cpms_id, study_site, scenario_id, version_id = NULL) {
      version_id <- require_version_id(version_id, "count_for_run()")
      version_clause <- if (has_version_id() && !is.null(version_id)) " AND version_id = ?" else ""
      params <- list(cpms_id, study_site, scenario_id)
      if (nzchar(version_clause)) params <- c(params, list(as.integer(version_id)))
      rids_dbGetQuery(
        con,
        paste0(paste(
          "SELECT COUNT(*) AS n FROM posting_lines",
          "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
        ), version_clause),
        params = params
      )$n[1]
    },

    find_by_run = function(cpms_id, study_site, scenario_id, version_id = NULL) {
      version_id <- require_version_id(version_id, "find_by_run()")
      version_clause <- if (has_version_id() && !is.null(version_id)) " AND version_id = ?" else ""
      params <- list(cpms_id, study_site, scenario_id)
      if (nzchar(version_clause)) params <- c(params, list(as.integer(version_id)))
      rids_canonicalize_names(rids_dbGetQuery(
        con,
        paste0(paste(
          "SELECT * FROM posting_lines",
          "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
        ), version_clause),
        params = params
      ), "posting_lines")
    },

    # Atomic replace of one run's posting lines (step 4 persist).
    replace_for_run = function(df, cpms_id, study_site, scenario_id, version_id = NULL) {
      version_id <- require_version_id(version_id, "replace_for_run()")
      DBI::dbWithTransaction(con, {
        df <- prepare_posting_lines(df, version_id)
        version_clause <- if (has_version_id() && !is.null(version_id)) " AND version_id = ?" else ""
        params <- list(cpms_id, study_site, scenario_id)
        if (nzchar(version_clause)) params <- c(params, list(as.integer(version_id)))
        rids_dbExecute(
          con,
          paste0(paste(
            "DELETE FROM posting_lines",
            "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
          ), version_clause),
          params = params
        )
        DBI::dbAppendTable(con, "posting_lines", rids_prepare_append(con, df))
      })
      invisible(TRUE)
    }
  )
}
