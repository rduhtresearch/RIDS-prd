# ict_costing_tbl repository. All SQL for the ICT costing table lives here.

ict_costing_repository <- function(con) {
  has_arm_identity <- function() {
    "arm_identity" %in% tolower(DBI::dbListFields(con, "ict_costing_tbl"))
  }

  has_version_id <- function() {
    "version_id" %in% tolower(DBI::dbListFields(con, "ict_costing_tbl"))
  }

  require_version_id <- function(version_id, operation) {
    if (has_version_id() &&
        (is.null(version_id) || length(version_id) != 1L || is.na(version_id))) {
      stop(operation, " requires a template version ID.")
    }
    version_id
  }

  arm_identity_select <- function() {
    if (has_arm_identity()) "Arm_Identity" else "Study_Arm AS Arm_Identity"
  }

  prepare_ict_cost_table <- function(df, version_id = NULL) {
    table_has_arm_identity <- has_arm_identity()
    if (table_has_arm_identity && !"Arm_Identity" %in% names(df)) {
      df$Arm_Identity <- df$Study_Arm
    }
    if (!table_has_arm_identity && "Arm_Identity" %in% names(df)) {
      df$Arm_Identity <- NULL
    }
    if (has_version_id()) {
      if (!is.null(version_id)) df$version_id <- as.integer(version_id)
    } else if ("version_id" %in% names(df)) {
      df$version_id <- NULL
    }
    df
  }

  list(
    find_by_run = function(cpms_id, study_site, scenario_id, version_id = NULL) {
      version_id <- require_version_id(version_id, "find_by_run()")
      version_clause <- if (has_version_id() && !is.null(version_id)) " AND version_id = ?" else ""
      params <- list(cpms_id, study_site, scenario_id)
      if (nzchar(version_clause)) params <- c(params, list(as.integer(version_id)))
      rids_canonicalize_names(rids_dbGetQuery(
        con,
        paste0(
          "SELECT CPMS_ID, study_site, scenario_id, ",
          if (has_version_id()) "version_id, " else "",
          "Study, Visit_Number, Study_Arm, ",
          arm_identity_select(),
          ",
         Visit_Label, Activity_Name, ICT_Cost, Contract_Cost,
         activity_occurrence_id, staff_group
         FROM ict_costing_tbl
         WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?",
          version_clause
        ),
        params = params
      ), "ict_costing_tbl")
    },

    # Atomic replace of one run's rows with an edited data frame (step 2 save).
    replace_run = function(df, cpms_id, study_site, scenario_id, version_id = NULL) {
      version_id <- require_version_id(version_id, "replace_run()")
      DBI::dbWithTransaction(con, {
        df <- prepare_ict_cost_table(df, version_id)
        version_clause <- if (has_version_id() && !is.null(version_id)) " AND version_id = ?" else ""
        params <- list(cpms_id, study_site, scenario_id)
        if (nzchar(version_clause)) params <- c(params, list(as.integer(version_id)))
        rids_dbExecute(
          con,
          paste0(paste(
            "DELETE FROM ict_costing_tbl",
            "WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?"
          ), version_clause),
          params = params
        )
        DBI::dbAppendTable(con, "ict_costing_tbl", rids_prepare_append(con, df))
      })
      invisible(TRUE)
    },

    # Atomic replace via staging table for a fresh workbook ingest (pipeline
    # stage B): replaces every (CPMS_ID, study_site, scenario_id) run present
    # in the incoming table.
    replace_from_staging = function(ict_cost_table, version_id = NULL) {
      version_id <- require_version_id(version_id, "replace_from_staging()")
      DBI::dbWithTransaction(con, {
        table_has_arm_identity <- has_arm_identity()
        table_has_version_id <- has_version_id()
        ict_cost_table <- prepare_ict_cost_table(ict_cost_table, version_id)

        DBI::dbWriteTable(con, "stg_ict_costing_tbl", rids_prepare_append(con, ict_cost_table), overwrite = TRUE)

        if (table_has_version_id && !is.null(version_id)) {
          rids_dbExecute(
            con,
            "DELETE FROM ict_costing_tbl WHERE version_id = ?",
            params = list(as.integer(version_id))
          )
        } else {
          rids_dbExecute(con, "
            DELETE FROM ict_costing_tbl
            WHERE (CPMS_ID, study_site, scenario_id) IN (
              SELECT DISTINCT CPMS_ID, study_site, scenario_id
              FROM stg_ict_costing_tbl
            )
          ")
        }

        if (table_has_arm_identity && table_has_version_id) {
          rids_dbExecute(con, "
            INSERT INTO ict_costing_tbl (
              CPMS_ID, study_site, scenario_id, version_id, Study, Visit_Number,
              Study_Arm, Arm_Identity, Visit_Label, Activity_Name, ICT_Cost,
              Contract_Cost, activity_occurrence_id, staff_group
            )
            SELECT
              CPMS_ID, study_site, scenario_id, version_id, Study, Visit_Number,
              Study_Arm, Arm_Identity, Visit_Label, Activity_Name, ICT_Cost,
              Contract_Cost, activity_occurrence_id, staff_group
            FROM stg_ict_costing_tbl
          ")
        } else if (table_has_arm_identity) {
          rids_dbExecute(con, "
            INSERT INTO ict_costing_tbl (
              CPMS_ID, study_site, scenario_id, Study, Visit_Number, Study_Arm, Arm_Identity,
              Visit_Label, Activity_Name, ICT_Cost, Contract_Cost, activity_occurrence_id, staff_group
            )
            SELECT
              CPMS_ID, study_site, scenario_id, Study, Visit_Number, Study_Arm, Arm_Identity,
              Visit_Label, Activity_Name, ICT_Cost, Contract_Cost, activity_occurrence_id, staff_group
            FROM stg_ict_costing_tbl
          ")
        } else {
          rids_dbExecute(con, "
            INSERT INTO ict_costing_tbl (
              CPMS_ID, study_site, scenario_id, Study, Visit_Number, Study_Arm, Visit_Label,
              Activity_Name, ICT_Cost, Contract_Cost, activity_occurrence_id, staff_group
            )
            SELECT
              CPMS_ID, study_site, scenario_id, Study, Visit_Number, Study_Arm, Visit_Label,
              Activity_Name, ICT_Cost, Contract_Cost, activity_occurrence_id, staff_group
            FROM stg_ict_costing_tbl
          ")
        }

        rids_dbExecute(con, "DROP TABLE stg_ict_costing_tbl")
      })
      invisible(TRUE)
    },

    visit_lookup = function(cpms_id, study_site, scenario_id, version_id = NULL) {
      version_id <- require_version_id(version_id, "visit_lookup()")
      version_clause <- if (has_version_id() && !is.null(version_id)) " AND version_id = ?" else ""
      params <- list(cpms_id, study_site, scenario_id)
      if (nzchar(version_clause)) params <- c(params, list(as.integer(version_id)))
      rids_canonicalize_names(rids_dbGetQuery(
        con,
        paste0("SELECT DISTINCT Study, Study_Arm, Visit_Label, Visit_Number
         FROM ict_costing_tbl
         WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?
           AND Visit_Label IS NOT NULL", version_clause),
        params = params
      ), "ict_costing_tbl")
    },

    exists_table = function() {
      DBI::dbExistsTable(con, "ict_costing_tbl")
    },

    contract_costs_for_run = function(cpms_id, study_site, scenario_id, version_id = NULL) {
      version_id <- require_version_id(version_id, "contract_costs_for_run()")
      version_clause <- if (has_version_id() && !is.null(version_id)) " AND version_id = ?" else ""
      params <- list(cpms_id, study_site, scenario_id)
      if (nzchar(version_clause)) params <- c(params, list(as.integer(version_id)))

      rids_canonicalize_names(rids_dbGetQuery(con, paste0(
        "SELECT CPMS_ID, study_site, scenario_id, ",
        if (has_version_id()) "version_id, " else "",
        "Visit_Number, Study_Arm, ", arm_identity_select(),
        ", Activity_Name, Contract_Cost, activity_occurrence_id, staff_group
         FROM ict_costing_tbl
         WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?",
        version_clause
      ), params = params), "ict_costing_tbl")
    },

    all_contract_costs = function() {
      rids_canonicalize_names(rids_dbGetQuery(con, paste0(
        "SELECT CPMS_ID, study_site, scenario_id, ",
        if (has_version_id()) "version_id, " else "",
        "Visit_Number, Study_Arm, ",
        arm_identity_select(),
        ", Activity_Name, Contract_Cost,
           activity_occurrence_id, staff_group
         FROM ict_costing_tbl"
      )), "ict_costing_tbl")
    }
  )
}
