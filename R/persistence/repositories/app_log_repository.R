# app_logs repository. All SQL for the app_logs table lives here.
#
# Note: nothing in the app currently inserts into app_logs (log_event() in
# R/utils/logging.R is a stub); this repository covers the read/prune paths
# actually in use.

app_log_repository <- function(con) {
  list(
    # filters: named list; NULL/empty entries are skipped. Mirrors the WHERE
    # construction previously inline in query_app_logs().
    query = function(where = character(), params = list(), limit = 1000L) {
      sql <- paste(
        "SELECT log_id, timestamp, level, area, message, user_id, username, session_id,",
        "cpms_id, upload_id, details_json, app_version",
        "FROM app_logs"
      )

      if (length(where) > 0) {
        sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
      }

      sql <- paste(sql, "ORDER BY timestamp DESC, log_id DESC")

      if (!is.null(limit) && is.finite(limit) && limit > 0) {
        sql <- paste(sql, "LIMIT", as.integer(limit))
      }

      DBI::dbGetQuery(con, sql, params = params)
    },

    distinct_values = function(column) {
      allowed <- c("level", "area", "username")
      if (!column %in% allowed) {
        return(character())
      }
      sql <- paste0(
        "SELECT DISTINCT ", column, " FROM app_logs ",
        "WHERE ", column, " IS NOT NULL AND trim(", column, ") <> '' ",
        "ORDER BY ", column
      )
      vals <- DBI::dbGetQuery(con, sql)[[1]]
      vals[!is.na(vals) & nzchar(vals)]
    },

    prune_before = function(cutoff) {
      as.integer(DBI::dbExecute(
        con,
        "DELETE FROM app_logs WHERE timestamp < ?",
        params = list(cutoff)
      ))
    }
  )
}
