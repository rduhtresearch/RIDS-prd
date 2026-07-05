# users repository. All SQL for the users table lives here.

user_repository <- function(con) {
  user_columns <- paste(
    "user_id, name, username, email, password_hash, role, active,",
    "force_password_change, created_at, updated_at, last_login_at"
  )

  find_by_id <- function(user_id) {
    row <- DBI::dbGetQuery(
      con,
      paste("SELECT", user_columns, "FROM users WHERE user_id = ? LIMIT 1"),
      params = list(user_id)
    )
    if (nrow(row) == 0) NULL else row
  }

  list(
    find_by_username = function(username) {
      row <- DBI::dbGetQuery(
        con,
        paste("SELECT", user_columns, "FROM users WHERE lower(username) = lower(?) LIMIT 1"),
        params = list(username)
      )
      if (nrow(row) == 0) NULL else row
    },

    find_by_id = find_by_id,

    count = function() {
      DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n[[1]]
    },

    username_taken_by_other = function(username, user_id) {
      nrow(DBI::dbGetQuery(
        con,
        "SELECT user_id FROM users WHERE lower(username) = lower(?) AND user_id <> ? LIMIT 1",
        params = list(username, user_id)
      )) > 0
    },

    insert = function(name, username, email, password_hash, role, active,
                      force_password_change = TRUE) {
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO users",
          "(name, username, email, password_hash, role, active, force_password_change)",
          "VALUES (?, ?, ?, ?, ?, ?, ?)"
        ),
        params = list(name, username, email, password_hash, role,
                      isTRUE(active), isTRUE(force_password_change))
      )
      DBI::dbGetQuery(con, "SELECT currval('user_id_seq') AS user_id")$user_id[[1]]
    },

    update_account = function(user_id, name, username, email, role, active) {
      DBI::dbExecute(
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
      DBI::dbExecute(
        con,
        "UPDATE users SET active = ?, updated_at = CURRENT_TIMESTAMP WHERE user_id = ?",
        params = list(isTRUE(active), user_id)
      )
      invisible(TRUE)
    },

    set_password_hash = function(user_id, password_hash, force_password_change) {
      DBI::dbExecute(
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
      DBI::dbExecute(
        con,
        "UPDATE users SET last_login_at = CURRENT_TIMESTAMP, updated_at = CURRENT_TIMESTAMP WHERE user_id = ?",
        params = list(user_id)
      )
      invisible(TRUE)
    },

    list_all = function() {
      DBI::dbGetQuery(
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
