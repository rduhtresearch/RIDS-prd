# posting_lines repository. All SQL for the posting_lines table lives here.

posting_line_repository <- function(con) {
  list(
    count_for_run = function(cpms_id, study_site, scenario_id) {
      rids_dbGetQuery(
        con,
        paste(
          "SELECT COUNT(*) AS n FROM posting_lines",
          "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
        ),
        params = list(cpms_id, study_site, scenario_id)
      )$n[1]
    },

    find_by_run = function(cpms_id, study_site, scenario_id) {
      rids_canonicalize_names(rids_dbGetQuery(
        con,
        paste(
          "SELECT * FROM posting_lines",
          "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
        ),
        params = list(cpms_id, study_site, scenario_id)
      ), "posting_lines")
    },

    # Atomic replace of one run's posting lines (step 4 persist).
    replace_for_run = function(df, cpms_id, study_site, scenario_id) {
      DBI::dbWithTransaction(con, {
        rids_dbExecute(
          con,
          paste(
            "DELETE FROM posting_lines",
            "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
          ),
          params = list(cpms_id, study_site, scenario_id)
        )
        DBI::dbAppendTable(con, "posting_lines", rids_prepare_append(con, df))
      })
      invisible(TRUE)
    }
  )
}
