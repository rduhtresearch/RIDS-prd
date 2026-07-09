# users repository. All SQL for the users table lives here.

user_repository <- function(con) {
  user_columns <- paste(
    "user_id, name, username, email, password_hash, role, active,",
    "force_password_change, created_at, updated_at, last_login_at"
  )

  find_by_id <- function(user_id) {
    row <- rids_dbGetQuery(
      con,
      paste("SELECT", user_columns, "FROM users WHERE user_id = ? LIMIT 1"),
      params = list(user_id)
    )
    if (nrow(row) == 0) NULL else row
  }

  list(
    find_by_username = function(username) {
      row <- rids_dbGetQuery(
        con,
        paste("SELECT", user_columns, "FROM users WHERE lower(username) = lower(?) LIMIT 1"),
        params = list(username)
      )
      if (nrow(row) == 0) NULL else row
    },

    find_by_id = find_by_id,

    count = function() {
      rids_dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n[[1]]
    },

    username_taken_by_other = function(username, user_id) {
      nrow(rids_dbGetQuery(
        con,
        "SELECT user_id FROM users WHERE lower(username) = lower(?) AND user_id <> ? LIMIT 1",
        params = list(username, user_id)
      )) > 0
    },

    insert = function(name, username, email, password_hash, role, active,
                      force_password_change = TRUE) {
      rids_dbExecute(
        con,
        paste(
          "INSERT INTO users",
          "(name, username, email, password_hash, role, active, force_password_change)",
          "VALUES (?, ?, ?, ?, ?, ?, ?)"
        ),
        params = list(name, username, email, password_hash, role,
                      isTRUE(active), isTRUE(force_password_change))
      )
      rids_dbGetQuery(con, "SELECT currval('user_id_seq') AS user_id")$user_id[[1]]
    },

    update_account = function(user_id, name, username, email, role, active) {
      rids_dbExecute(
        con,
        paste(
          "UPDATE users SET",
          "name = ?, username = ?, email = ?, role = ?, active = ?, updated_at = CURRENT_TIMESTAMP",
          "WHERE user_id = ?"
        ),
        params = list(name, username, email, role, isTRUE(active), user_id)
      )
      invisible(TRUE)
    },

    set_active = function(user_id, active) {
      rids_dbExecute(
        con,
        "UPDATE users SET active = ?, updated_at = CURRENT_TIMESTAMP WHERE user_id = ?",
        params = list(isTRUE(active), user_id)
      )
      invisible(TRUE)
    },

    set_password_hash = function(user_id, password_hash, force_password_change) {
      rids_dbExecute(
        con,
        paste(
          "UPDATE users SET password_hash = ?, force_password_change = ?,",
          "updated_at = CURRENT_TIMESTAMP WHERE user_id = ?"
        ),
        params = list(password_hash, isTRUE(force_password_change), user_id)
      )
      invisible(TRUE)
    },

    touch_last_login = function(user_id) {
      rids_dbExecute(
        con,
        "UPDATE users SET last_login_at = CURRENT_TIMESTAMP, updated_at = CURRENT_TIMESTAMP WHERE user_id = ?",
        params = list(user_id)
      )
      invisible(TRUE)
    },

    list_all = function() {
      rids_dbGetQuery(
        con,
        paste(
          "SELECT user_id, name, username, email, role, active, force_password_change,",
          "created_at, updated_at, last_login_at",
          "FROM users ORDER BY created_at DESC, username ASC"
        )
      )
    }
  )
}
