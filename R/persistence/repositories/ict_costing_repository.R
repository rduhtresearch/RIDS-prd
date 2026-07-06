# ict_costing_tbl repository. All SQL for the ICT costing table lives here.

ict_costing_repository <- function(con) {
  list(
    find_by_run = function(cpms_id, study_site, scenario_id) {
      DBI::dbGetQuery(
        con,
        "SELECT CPMS_ID, study_site, scenario_id, Study, Visit_Number, Study_Arm,
         Visit_Label, Activity_Name, ICT_Cost, Contract_Cost,
         activity_occurrence_id, staff_group
         FROM ict_costing_tbl
         WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?",
        params = list(cpms_id, study_site, scenario_id)
      )
    },

    # Atomic replace of one run's rows with an edited data frame (step 2 save).
    replace_run = function(df, cpms_id, study_site, scenario_id) {
      DBI::dbWithTransaction(con, {
        DBI::dbExecute(
          con,
          paste(
            "DELETE FROM ict_costing_tbl",
            "WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?"
          ),
          params = list(cpms_id, study_site, scenario_id)
        )
        DBI::dbAppendTable(con, "ict_costing_tbl", df)
      })
      invisible(TRUE)
    },

    # Atomic replace via staging table for a fresh workbook ingest (pipeline
    # stage B): replaces every (CPMS_ID, study_site, scenario_id) run present
    # in the incoming table.
    replace_from_staging = function(ict_cost_table) {
      DBI::dbWithTransaction(con, {
        DBI::dbWriteTable(con, "stg_ict_costing_tbl", ict_cost_table, overwrite = TRUE)

        DBI::dbExecute(con, "
          DELETE FROM ict_costing_tbl
          WHERE (CPMS_ID, study_site, scenario_id) IN (
            SELECT DISTINCT CPMS_ID, study_site, scenario_id
            FROM stg_ict_costing_tbl
          )
        ")

        DBI::dbExecute(con, "
          INSERT INTO ict_costing_tbl (
            CPMS_ID, study_site, scenario_id, Study, Visit_Number, Study_Arm, Visit_Label,
            Activity_Name, ICT_Cost, Contract_Cost, activity_occurrence_id, staff_group
          )
          SELECT
            CPMS_ID, study_site, scenario_id, Study, Visit_Number, Study_Arm, Visit_Label,
            Activity_Name, ICT_Cost, Contract_Cost, activity_occurrence_id, staff_group
          FROM stg_ict_costing_tbl
        ")

        DBI::dbExecute(con, "DROP TABLE stg_ict_costing_tbl")
      })
      invisible(TRUE)
    },

    visit_lookup = function(cpms_id, study_site, scenario_id) {
      DBI::dbGetQuery(
        con,
        "SELECT DISTINCT Study, Study_Arm, Visit_Label, Visit_Number
         FROM ict_costing_tbl
         WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?
           AND Visit_Label IS NOT NULL",
        params = list(cpms_id, study_site, scenario_id)
      )
    },

    exists_table = function() {
      DBI::dbExistsTable(con, "ict_costing_tbl")
    },

    all_contract_costs = function() {
      DBI::dbGetQuery(con, "
        SELECT CPMS_ID, study_site, scenario_id, Visit_Number, Study_Arm, Activity_Name, Contract_Cost,
               activity_occurrence_id, staff_group
        FROM ict_costing_tbl
      ")
    }
  )
}
