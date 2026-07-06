# meta_data (study run) repository. All SQL for the meta_data table lives
# here, plus the cross-table cascade delete for a study run (one domain
# operation, executed in a single transaction).

study_repository <- function(con) {
  list(
    exists_run = function(cpms_id, study_site, scenario_id) {
      nrow(DBI::dbGetQuery(
        con,
        paste(
          "SELECT 1",
          "FROM meta_data",
          "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?",
          "LIMIT 1"
        ),
        params = list(cpms_id, study_site, scenario_id)
      )) > 0
    },

    insert_meta = function(cpms_id, study_site, scenario_id, edge_id, study_name,
                           notes, uploaded_by, original_filename, saved_file_path,
                           speciality_id, mff_split_enabled, mff_split_pct) {
      DBI::dbExecute(
        con,
        "INSERT INTO meta_data
   (cpms_id, study_site, scenario_id, edge_id, study_name, notes, uploaded_by,
    original_filename, saved_file_path, speciality_id, mff_split_enabled, mff_split_pct)
   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          cpms_id, study_site, scenario_id, edge_id, study_name, notes,
          uploaded_by, original_filename, saved_file_path, speciality_id,
          mff_split_enabled, mff_split_pct
        )
      )
      invisible(TRUE)
    },

    last_upload_id = function() {
      DBI::dbGetQuery(con, "SELECT currval('upload_id_seq') AS upload_id")$upload_id[[1]]
    },

    # Full study list for the library view (NUL-scrubbed, speciality joined).
    list_studies = function() {
      DBI::dbGetQuery(
        con,
        "SELECT
           REPLACE(m.cpms_id, chr(0), '') AS cpms_id,
           REPLACE(m.study_site, chr(0), '') AS study_site,
           REPLACE(m.study_name, chr(0), '') AS study_name,
           REPLACE(m.scenario_id, chr(0), '') AS scenario_id,
           REPLACE(m.edge_id, chr(0), '') AS edge_id,
           REPLACE(m.uploaded_by, chr(0), '') AS uploaded_by,
           m.speciality_id,
           REPLACE(COALESCE(s.name, ''), chr(0), '') AS speciality_name,
           m.upload_timestamp
         FROM meta_data m
         LEFT JOIN specialities s ON m.speciality_id = s.id
         ORDER BY upload_timestamp DESC"
      )
    },

    # Full metadata for one study run (NUL-scrubbed, speciality joined).
    find_meta = function(cpms_id, study_site, scenario_id) {
      DBI::dbGetQuery(
        con,
        "SELECT
           REPLACE(m.cpms_id, chr(0), '') AS cpms_id,
           REPLACE(m.study_site, chr(0), '') AS study_site,
           REPLACE(m.study_name, chr(0), '') AS study_name,
           REPLACE(m.scenario_id, chr(0), '') AS scenario_id,
           REPLACE(m.edge_id, chr(0), '') AS edge_id,
           REPLACE(m.uploaded_by, chr(0), '') AS uploaded_by,
           m.upload_timestamp,
           REPLACE(m.original_filename, chr(0), '') AS original_filename,
           REPLACE(m.notes, chr(0), '') AS notes,
           REPLACE(m.saved_file_path, chr(0), '') AS saved_file_path,
           REPLACE(m.edge_zip_path, chr(0), '') AS edge_zip_path,
           m.speciality_id, s.name AS speciality_name
         FROM meta_data m
         LEFT JOIN specialities s ON m.speciality_id = s.id
         WHERE m.cpms_id = ? AND m.study_site = ? AND m.scenario_id = ?",
        params = list(cpms_id, study_site, scenario_id)
      )
    },

    mff_config = function(cpms_id, study_site, scenario_id) {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT mff_split_enabled, mff_split_pct",
          "FROM meta_data",
          "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?",
          "LIMIT 1"
        ),
        params = list(cpms_id, study_site, scenario_id)
      )
    },

    set_edge_zip_path = function(zip_path, cpms_id, study_site, scenario_id) {
      DBI::dbExecute(
        con,
        paste(
          "UPDATE meta_data SET edge_zip_path = ?",
          "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
        ),
        params = list(zip_path, cpms_id, study_site, scenario_id)
      )
      invisible(TRUE)
    },

    find_run_files = function(cpms_id, study_site, scenario_id) {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT id, saved_file_path, edge_zip_path",
          "FROM meta_data",
          "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
        ),
        params = list(cpms_id, study_site, scenario_id)
      )
    },

    # Cascade delete of one study run across all child tables, in a single
    # transaction. Returns per-table deletion counts.
    delete_run = function(cpms_id, study_site, scenario_id, upload_ids = character()) {
      run_params <- list(cpms_id, study_site, scenario_id)
      counts <- list(
        addon_custom_activities = 0L,
        posting_lines = 0L,
        ict_costing_tbl = 0L,
        app_logs = 0L,
        meta_data = 0L
      )

      DBI::dbWithTransaction(con, {
        if (DBI::dbExistsTable(con, "addon_custom_activities")) {
          counts$addon_custom_activities <- as.integer(DBI::dbExecute(
            con,
            paste(
              "DELETE FROM addon_custom_activities",
              "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
            ),
            params = run_params
          ))
        }

        if (DBI::dbExistsTable(con, "posting_lines")) {
          counts$posting_lines <- as.integer(DBI::dbExecute(
            con,
            paste(
              "DELETE FROM posting_lines",
              "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
            ),
            params = run_params
          ))
        }

        if (DBI::dbExistsTable(con, "ict_costing_tbl")) {
          counts$ict_costing_tbl <- as.integer(DBI::dbExecute(
            con,
            paste(
              "DELETE FROM ict_costing_tbl",
              "WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?"
            ),
            params = run_params
          ))
        }

        if (DBI::dbExistsTable(con, "app_logs") && length(upload_ids) > 0) {
          counts$app_logs <- sum(vapply(upload_ids, function(upload_id) {
            as.integer(DBI::dbExecute(
              con,
              "DELETE FROM app_logs WHERE upload_id = ?",
              params = list(upload_id)
            ))
          }, integer(1)))
        }

        counts$meta_data <- as.integer(DBI::dbExecute(
          con,
          paste(
            "DELETE FROM meta_data",
            "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
          ),
          params = run_params
        ))
      })

      counts
    }
  )
}
