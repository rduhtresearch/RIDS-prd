suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(htmltools)
  library(shiny)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(y)
  }
  x
}

.auth_passed <- 0L
.auth_failed <- 0L

.auth_expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .auth_passed <<- .auth_passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .auth_failed <<- .auth_failed + 1L
  }
}

run_auth_password_reset_tests <- function() {
  cat("\n=== auth password reset tests ===\n\n")
  .auth_passed <<- 0L
  .auth_failed <<- 0L

  source("R/utils/auth.r")
  source("R/modules/login_mod.R")

  db_path <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

  old_con <- if (exists("CON", inherits = TRUE)) get("CON", inherits = TRUE) else NULL
  had_app_log_exception <- exists("app_log_exception", inherits = TRUE)
  old_app_log_exception <- if (had_app_log_exception) get("app_log_exception", inherits = TRUE) else NULL
  had_log_event <- exists("log_event", inherits = TRUE)
  old_log_event <- if (had_log_event) get("log_event", inherits = TRUE) else NULL

  assign("CON", con, envir = .GlobalEnv)
  assign("app_log_exception", function(...) NULL, envir = .GlobalEnv)
  assign("log_event", function(...) NULL, envir = .GlobalEnv)

  on.exit({
    if (is.null(old_con)) {
      rm("CON", envir = .GlobalEnv)
    } else {
      assign("CON", old_con, envir = .GlobalEnv)
    }

    if (had_app_log_exception) {
      assign("app_log_exception", old_app_log_exception, envir = .GlobalEnv)
    } else if (exists("app_log_exception", envir = .GlobalEnv, inherits = FALSE)) {
      rm("app_log_exception", envir = .GlobalEnv)
    }

    if (had_log_event) {
      assign("log_event", old_log_event, envir = .GlobalEnv)
    } else if (exists("log_event", envir = .GlobalEnv, inherits = FALSE)) {
      rm("log_event", envir = .GlobalEnv)
    }

    dbDisconnect(con, shutdown = TRUE)
    if (file.exists(db_path)) unlink(db_path, force = TRUE)
  }, add = TRUE)

  dbExecute(con, "CREATE SEQUENCE user_id_seq;")
  dbExecute(con, "CREATE SEQUENCE auth_session_id_seq;")
  dbExecute(con, "CREATE SEQUENCE auth_audit_id_seq;")

  dbExecute(con, "
    CREATE TABLE users (
      user_id INTEGER PRIMARY KEY DEFAULT nextval('user_id_seq'),
      name TEXT,
      username TEXT UNIQUE NOT NULL,
      email TEXT,
      password_hash TEXT,
      role TEXT NOT NULL DEFAULT 'user',
      active BOOLEAN NOT NULL DEFAULT TRUE,
      force_password_change BOOLEAN NOT NULL DEFAULT FALSE,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      last_login_at TIMESTAMP
    )
  ")

  dbExecute(con, "
    CREATE TABLE auth_sessions (
      session_id INTEGER PRIMARY KEY DEFAULT nextval('auth_session_id_seq'),
      user_id INTEGER NOT NULL,
      token_hash TEXT NOT NULL,
      expires_at TIMESTAMP NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      revoked_at TIMESTAMP,
      user_agent TEXT,
      FOREIGN KEY (user_id) REFERENCES users(user_id)
    )
  ")

  dbExecute(con, "
    CREATE TABLE auth_audit_log (
      audit_id INTEGER PRIMARY KEY DEFAULT nextval('auth_audit_id_seq'),
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      event_type TEXT NOT NULL,
      user_id INTEGER,
      actor_user_id INTEGER,
      username TEXT,
      success BOOLEAN NOT NULL DEFAULT TRUE,
      message TEXT,
      session_id INTEGER
    )
  ")

  active_result <- create_user_account(
    name = "Active User",
    username = "active.user",
    temporary_password = "OldPass123",
    active = TRUE
  )
  inactive_result <- create_user_account(
    name = "Inactive User",
    username = "inactive.user",
    temporary_password = "OtherPass123",
    active = FALSE
  )

  active_user_id <- active_result$user$user_id[[1]]
  session_result <- create_auth_session(
    user_id = active_user_id,
    user_agent = "auth-reset-test",
    duration_hours = 1
  )

  reset_result <- reset_user_password_by_username("active.user", "NewPass123")
  active_user <- get_user_by_id(active_user_id)
  active_session <- dbGetQuery(
    con,
    "SELECT revoked_at FROM auth_sessions WHERE session_id = ?",
    params = list(session_result$session_id)
  )
  reset_event <- dbGetQuery(
    con,
    paste(
      "SELECT COUNT(*) AS n FROM auth_audit_log",
      "WHERE event_type = 'self_service_password_reset_insecure' AND username = 'active.user'"
    )
  )
  old_login <- authenticate_user("active.user", "OldPass123")
  new_login <- authenticate_user("active.user", "NewPass123")

  .auth_expect("active user can reset by username", isTRUE(reset_result$success))
  .auth_expect("old password no longer authenticates", !isTRUE(old_login$success))
  .auth_expect("new password authenticates", isTRUE(new_login$success))
  .auth_expect("force password change cleared", !isTRUE(active_user$force_password_change[[1]]))
  .auth_expect("existing sessions are revoked", !is.na(active_session$revoked_at[[1]]))
  .auth_expect("audit event recorded", reset_event$n[[1]] == 1)

  inactive_reset <- reset_user_password_by_username("inactive.user", "ResetPass123")
  .auth_expect("inactive user cannot reset", !isTRUE(inactive_reset$success))
  .auth_expect(
    "inactive reset returns clear message",
    identical(inactive_reset$message, "User is inactive.")
  )

  missing_reset <- reset_user_password_by_username("missing.user", "ResetPass123")
  .auth_expect("unknown username fails cleanly", !isTRUE(missing_reset$success))
  .auth_expect(
    "unknown username returns clear message",
    identical(missing_reset$message, "User not found.")
  )

  login_markup <- as.character(renderTags(loginUI("login"))$html)
  .auth_expect(
    "login screen includes forgot password action",
    grepl("Forgot password\\?", login_markup)
  )
  .auth_expect(
    "login screen includes reset password action",
    grepl("Reset password", login_markup)
  )
  .auth_expect(
    "login screen includes back to sign in action",
    grepl("Back to sign in", login_markup)
  )
  .auth_expect(
    "login screen includes reset username input",
    grepl("reset_username", login_markup, fixed = TRUE)
  )

  list(passed = .auth_passed, failed = .auth_failed)
}
