# auth_audit_log repository. All SQL for the auth_audit_log table lives here.

auth_audit_repository <- function(con) {
  list(
    record = function(event_type, user_id, actor_user_id, username, success,
                      message, session_id) {
      rids_dbExecute(
        con,
        paste(
          "INSERT INTO auth_audit_log",
          "(event_type, user_id, actor_user_id, username, success, message, session_id)",
          "VALUES (?, ?, ?, ?, ?, ?, ?)"
        ),
        params = list(
          event_type, user_id, actor_user_id, username,
          isTRUE(success), message, session_id
        )
      )
      invisible(TRUE)
    }
  )
}
