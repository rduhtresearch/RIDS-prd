# ict_costing_tbl repository. All SQL for the ICT costing table lives here.

ict_costing_repository <- function(con) {
  has_arm_identity <- function() {
    "arm_identity" %in% tolower(DBI::dbListFields(con, "ict_costing_tbl"))
  }

  arm_identity_select <- function() {
    if (has_arm_identity()) "Arm_Identity" else "Study_Arm AS Arm_Identity"
  }

  prepare_ict_cost_table <- function(df) {
    table_has_arm_identity <- has_arm_identity()
    if (table_has_arm_identity && !"Arm_Identity" %in% names(df)) {
      df$Arm_Identity <- df$Study_Arm
    }
    if (!table_has_arm_identity && "Arm_Identity" %in% names(df)) {
      df$Arm_Identity <- NULL
    }
    df
  }

  list(
    find_by_run = function(cpms_id, study_site, scenario_id) {
      rids_canonicalize_names(rids_dbGetQuery(
        con,
        paste0(
          "SELECT CPMS_ID, study_site, scenario_id, Study, Visit_Number, Study_Arm, ",
          arm_identity_select(),
          ",
         Visit_Label, Activity_Name, ICT_Cost, Contract_Cost,
         activity_occurrence_id, staff_group
         FROM ict_costing_tbl
         WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?"
        ),
        params = list(cpms_id, study_site, scenario_id)
      ), "ict_costing_tbl")
    },

    # Atomic replace of one run's rows with an edited data frame (step 2 save).
    replace_run = function(df, cpms_id, study_site, scenario_id) {
      DBI::dbWithTransaction(con, {
        df <- prepare_ict_cost_table(df)
        rids_dbExecute(
          con,
          paste(
            "DELETE FROM ict_costing_tbl",
            "WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?"
          ),
          params = list(cpms_id, study_site, scenario_id)
        )
        DBI::dbAppendTable(con, "ict_costing_tbl", rids_prepare_append(con, df))
      })
      invisible(TRUE)
    },

    # Atomic replace via staging table for a fresh workbook ingest (pipeline
    # stage B): replaces every (CPMS_ID, study_site, scenario_id) run present
    # in the incoming table.
    replace_from_staging = function(ict_cost_table) {
      DBI::dbWithTransaction(con, {
        table_has_arm_identity <- has_arm_identity()
        ict_cost_table <- prepare_ict_cost_table(ict_cost_table)

        DBI::dbWriteTable(con, "stg_ict_costing_tbl", rids_prepare_append(con, ict_cost_table), overwrite = TRUE)

        rids_dbExecute(con, "
          DELETE FROM ict_costing_tbl
          WHERE (CPMS_ID, study_site, scenario_id) IN (
            SELECT DISTINCT CPMS_ID, study_site, scenario_id
            FROM stg_ict_costing_tbl
          )
        ")

        if (table_has_arm_identity) {
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

    visit_lookup = function(cpms_id, study_site, scenario_id) {
      rids_canonicalize_names(rids_dbGetQuery(
        con,
        "SELECT DISTINCT Study, Study_Arm, Visit_Label, Visit_Number
         FROM ict_costing_tbl
         WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?
           AND Visit_Label IS NOT NULL",
        params = list(cpms_id, study_site, scenario_id)
      ), "ict_costing_tbl")
    },

    exists_table = function() {
      DBI::dbExistsTable(con, "ict_costing_tbl")
    },

    all_contract_costs = function() {
      rids_canonicalize_names(rids_dbGetQuery(con, paste0(
        "SELECT CPMS_ID, study_site, scenario_id, Visit_Number, Study_Arm, ",
        arm_identity_select(),
        ", Activity_Name, Contract_Cost,
           activity_occurrence_id, staff_group
         FROM ict_costing_tbl"
      )), "ict_costing_tbl")
    }
  )
}
