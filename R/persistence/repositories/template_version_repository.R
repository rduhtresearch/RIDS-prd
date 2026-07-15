template_version_repository <- function(con) {
  validate_type <- function(version_type) {
    allowed <- c("baseline", "substantial_amendment", "distribution_amendment")
    if (!identical(length(version_type), 1L) || !version_type %in% allowed) {
      stop("Unsupported template version type: ", version_type)
    }
  }

  normalize_activity_date <- function(activity_date) {
    if (length(activity_date) != 1L || is.null(activity_date) || is.na(activity_date)) {
      stop("Activity date must be a non-missing single date.")
    }
    parsed <- tryCatch(as.Date(as.character(activity_date)), error = function(e) as.Date(NA))
    if (is.na(parsed)) stop("Activity date must be a valid date.")
    parsed
  }

  assert_expected_study <- function(version, expected_study_id) {
    if (!is.null(expected_study_id) &&
        !identical(as.integer(version$study_id[[1]]), as.integer(expected_study_id))) {
      stop("Template version does not belong to the selected study.")
    }
  }

  resolve_by_status <- function(cpms_id, study_site, scenario_id, activity_date,
                                statuses) {
    activity_date <- normalize_activity_date(activity_date)
    status_sql <- paste(sprintf("'%s'", statuses), collapse = ", ")
    result <- rids_dbGetQuery(
      con,
      paste0(
        "SELECT tv.*
         FROM template_versions tv
         JOIN meta_data m ON m.id = tv.study_id
         WHERE m.cpms_id = ? AND m.study_site = ? AND m.scenario_id = ?
           AND tv.status IN (", status_sql, ")
           AND (tv.version_type = 'baseline' OR tv.effective_from_date <= ?)
         ORDER BY
           CASE WHEN tv.version_type = 'baseline' THEN 0 ELSE 1 END DESC,
           tv.effective_from_date DESC,
           tv.version_number DESC
         LIMIT 1"
      ),
      params = list(cpms_id, study_site, scenario_id, activity_date)
    )
    if (nrow(result) == 0) NULL else result[1, ]
  }

  list(
    create = function(study_id, version_type, effective_from_date = NULL,
                      uploaded_by = NULL, notes = NULL, original_filename,
                      saved_file_path) {
      validate_type(version_type)
      if (identical(version_type, "baseline") && !is.null(effective_from_date)) {
        stop("Baseline versions cannot have an effective-from date.")
      }
      if (!identical(version_type, "baseline") &&
          (is.null(effective_from_date) || is.na(effective_from_date))) {
        stop("Amendment versions require an effective-from date.")
      }
      DBI::dbWithTransaction(con, {
        nullable_text <- function(value) {
          if (is.null(value) || length(value) == 0L) NA_character_ else as.character(value[[1]])
        }

        if (identical(version_type, "baseline")) {
          existing_baseline <- rids_dbGetQuery(
            con,
            "SELECT 1 FROM template_versions
             WHERE study_id = ? AND version_type = 'baseline' LIMIT 1",
            params = list(as.integer(study_id))
          )
          if (nrow(existing_baseline) > 0) {
            stop("This study already has a baseline template version.")
          }
        } else {
          processing <- rids_dbGetQuery(
            con,
            "SELECT 1 FROM template_versions
             WHERE study_id = ? AND status = 'processing' LIMIT 1",
            params = list(as.integer(study_id))
          )
          if (nrow(processing) > 0) {
            stop("This study already has an amendment being processed. Complete or discard it first.")
          }
        }

        next_number <- rids_dbGetQuery(
          con,
          "SELECT COALESCE(MAX(version_number), 0) + 1 AS next_number
           FROM template_versions WHERE study_id = ?",
          params = list(as.integer(study_id))
        )$next_number[[1]]

        rids_dbExecute(
          con,
          "INSERT INTO template_versions
             (study_id, version_number, version_type, effective_from_date,
              status, uploaded_by, notes, original_filename, saved_file_path)
           VALUES (?, ?, ?, ?, 'processing', ?, ?, ?, ?)",
          params = list(
            as.integer(study_id), as.integer(next_number), version_type,
            if (is.null(effective_from_date)) as.Date(NA) else as.Date(effective_from_date),
            nullable_text(uploaded_by), nullable_text(notes),
            nullable_text(original_filename), nullable_text(saved_file_path)
          )
        )

        rids_dbGetQuery(
          con,
          "SELECT version_id FROM template_versions
           WHERE study_id = ? AND version_number = ?",
          params = list(as.integer(study_id), as.integer(next_number))
        )$version_id[[1]]
      })
    },

    list_for_study = function(cpms_id, study_site, scenario_id) {
      rids_dbGetQuery(
        con,
        "SELECT tv.*
         FROM template_versions tv
         JOIN meta_data m ON m.id = tv.study_id
         WHERE m.cpms_id = ? AND m.study_site = ? AND m.scenario_id = ?
         ORDER BY tv.version_number DESC",
        params = list(cpms_id, study_site, scenario_id)
      )
    },

    find = function(version_id) {
      rids_dbGetQuery(
        con,
        "SELECT tv.*, m.cpms_id, m.study_site, m.scenario_id, m.study_name,
                m.edge_id, m.speciality_id, m.mff_split_enabled, m.mff_split_pct
         FROM template_versions tv
         JOIN meta_data m ON m.id = tv.study_id
         WHERE tv.version_id = ?",
        params = list(as.integer(version_id))
      )
    },

    baseline_for_study = function(study_id) {
      rids_dbGetQuery(
        con,
        "SELECT * FROM template_versions
         WHERE study_id = ? AND version_type = 'baseline'
         ORDER BY version_number LIMIT 1",
        params = list(as.integer(study_id))
      )
    },

    resolve_for_activity_date = function(cpms_id, study_site, scenario_id, activity_date) {
      resolve_by_status(
        cpms_id, study_site, scenario_id, activity_date,
        statuses = c("active", "archived")
      )
    },

    resolve_available_for_activity_date = function(cpms_id, study_site, scenario_id,
                                                   activity_date) {
      resolve_by_status(
        cpms_id, study_site, scenario_id, activity_date,
        statuses = "active"
      )
    },

    set_edge_zip_path = function(version_id, zip_path, expected_study_id = NULL) {
      version <- rids_dbGetQuery(
        con,
        "SELECT study_id FROM template_versions WHERE version_id = ?",
        params = list(as.integer(version_id))
      )
      if (nrow(version) == 0) stop("Template version not found.")
      assert_expected_study(version, expected_study_id)
      updated <- rids_dbExecute(
        con,
        "UPDATE template_versions SET edge_zip_path = ? WHERE version_id = ?",
        params = list(zip_path, as.integer(version_id))
      )
      if (updated != 1L) stop("Failed to update the template version ZIP path.")
      invisible(TRUE)
    },

    activate = function(version_id, expected_study_id = NULL) {
      DBI::dbWithTransaction(con, {
        version <- rids_dbGetQuery(
          con,
          "SELECT study_id, status, edge_zip_path
           FROM template_versions WHERE version_id = ?",
          params = list(as.integer(version_id))
        )
        if (nrow(version) == 0) stop("Template version not found.")
        assert_expected_study(version, expected_study_id)
        if (identical(version$status[[1]], "archived")) {
          stop("An archived template version cannot be activated.")
        }
        if (is.na(version$edge_zip_path[[1]]) || !nzchar(version$edge_zip_path[[1]])) {
          stop("A template version cannot be activated before its EDGE ZIP is saved.")
        }
        rids_dbExecute(
          con,
          "UPDATE template_versions SET status = 'active' WHERE version_id = ?",
          params = list(as.integer(version_id))
        )
      })
      invisible(TRUE)
    },

    archive = function(version_id, expected_study_id = NULL, as_of_date = Sys.Date()) {
      as_of_date <- normalize_activity_date(as_of_date)
      DBI::dbWithTransaction(con, {
        version <- rids_dbGetQuery(
          con,
          "SELECT version_type, study_id, status
           FROM template_versions WHERE version_id = ?",
          params = list(as.integer(version_id))
        )
        if (nrow(version) == 0) stop("Template version not found.")
        assert_expected_study(version, expected_study_id)
        if (!identical(version$status[[1]], "active")) {
          stop("Only an active template version can be archived.")
        }

        superseding <- if (identical(version$version_type[[1]], "baseline")) {
          rids_dbGetQuery(
            con,
            "SELECT COUNT(*) AS n FROM template_versions
             WHERE study_id = ? AND version_id <> ? AND status = 'active'
               AND version_type <> 'baseline' AND effective_from_date <= ?",
            params = list(version$study_id[[1]], as.integer(version_id), as_of_date)
          )$n[[1]]
        } else {
          target <- rids_dbGetQuery(
            con,
            "SELECT effective_from_date, version_number
             FROM template_versions WHERE version_id = ?",
            params = list(as.integer(version_id))
          )
          rids_dbGetQuery(
            con,
            "SELECT COUNT(*) AS n FROM template_versions
             WHERE study_id = ? AND version_id <> ? AND status = 'active'
               AND version_type <> 'baseline' AND effective_from_date <= ?
               AND (effective_from_date > ? OR
                    (effective_from_date = ? AND version_number > ?))",
            params = list(
              version$study_id[[1]], as.integer(version_id), as_of_date,
              as.Date(target$effective_from_date[[1]]),
              as.Date(target$effective_from_date[[1]]),
              as.integer(target$version_number[[1]])
            )
          )$n[[1]]
        }
        if (superseding == 0) {
          stop("This version cannot be archived until a newer amendment is active.")
        }

        rids_dbExecute(
          con,
          "UPDATE template_versions SET status = 'archived' WHERE version_id = ?",
          params = list(as.integer(version_id))
        )
      })
      invisible(TRUE)
    },

    discard = function(version_id, expected_study_id = NULL) {
      version_id <- as.integer(version_id)
      counts <- list(addon_custom_activities = 0L, posting_lines = 0L,
                     ict_costing_tbl = 0L, template_versions = 0L)
      DBI::dbWithTransaction(con, {
        version <- rids_dbGetQuery(
          con,
          "SELECT study_id, status FROM template_versions WHERE version_id = ?",
          params = list(version_id)
        )
        if (nrow(version) == 0) stop("Template version not found.")
        assert_expected_study(version, expected_study_id)
        if (!identical(version$status[[1]], "processing")) {
          stop("Only an incomplete template version can be discarded.")
        }

        for (table in c("addon_custom_activities", "posting_lines", "ict_costing_tbl")) {
          if (DBI::dbExistsTable(con, table)) {
            counts[[table]] <- as.integer(rids_dbExecute(
              con, paste("DELETE FROM", table, "WHERE version_id = ?"),
              params = list(version_id)
            ))
          }
        }
        counts$template_versions <- as.integer(rids_dbExecute(
          con, "DELETE FROM template_versions WHERE version_id = ?",
          params = list(version_id)
        ))
      })
      counts
    }
  )
}
