# auth_sessions repository. All SQL for the auth_sessions table lives here.

session_repository <- function(con) {
  list(
    insert = function(user_id, token_hash, expires_at, user_agent) {
      rids_dbExecute(
        con,
        paste(
          "INSERT INTO auth_sessions (user_id, token_hash, expires_at, user_agent)",
          "VALUES (?, ?, ?, ?)"
        ),
        params = list(user_id, token_hash, expires_at, user_agent)
      )
      rids_dbGetQuery(con, "SELECT currval('auth_session_id_seq') AS session_id")$session_id[[1]]
    },

    # Most recent session (with its user row) matching a token hash.
    find_by_token_hash = function(token_hash) {
      rids_dbGetQuery(
        con,
        paste(
          "SELECT s.session_id, s.user_id, s.expires_at, s.revoked_at, s.created_at, s.user_agent,",
          "u.name, u.username, u.email, u.role, u.active, u.force_password_change",
          "FROM auth_sessions s",
          "JOIN users u ON u.user_id = s.user_id",
          "WHERE s.token_hash = ?",
          "ORDER BY s.created_at DESC LIMIT 1"
        ),
        params = list(token_hash)
      )
    },

    find_brief = function(session_id) {
      rids_dbGetQuery(
        con,
        paste(
          "SELECT s.session_id, s.user_id, u.username",
          "FROM auth_sessions s",
          "LEFT JOIN users u ON u.user_id = s.user_id",
          "WHERE s.session_id = ? LIMIT 1"
        ),
        params = list(session_id)
      )
    },

    revoke = function(session_id) {
      rids_dbExecute(
        con,
        "UPDATE auth_sessions SET revoked_at = CURRENT_TIMESTAMP WHERE session_id = ? AND revoked_at IS NULL",
        params = list(session_id)
      )
      invisible(TRUE)
    },

    revoke_all_for_user = function(user_id) {
      rids_dbExecute(
        con,
        "UPDATE auth_sessions SET revoked_at = CURRENT_TIMESTAMP WHERE user_id = ? AND revoked_at IS NULL",
        params = list(user_id)
      )
      invisible(TRUE)
    }
  )
}
