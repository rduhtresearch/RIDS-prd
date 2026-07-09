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
  cat("\n=== auth password reset tests (MFA-gated) ===\n\n")
  .auth_passed <<- 0L
  .auth_failed <<- 0L

  source("R/utils/auth.r")
  source("R/utils/user_credentials.R")
  source("R/auth/auth_provider.R")
  source("R/modules/login_mod.R")

  db_path <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

  old_con <- if (exists("CON", inherits = TRUE)) get("CON", inherits = TRUE) else NULL
  had_app_log_exception <- exists("app_log_exception", inherits = TRUE)
  old_app_log_exception <- if (had_app_log_exception) get("app_log_exception", inherits = TRUE) else NULL
  had_log_event <- exists("log_event", inherits = TRUE)
  old_log_event <- if (had_log_event) get("log_event", inherits = TRUE) else NULL
  had_secret <- exists("CREDENTIAL_SECRET", inherits = TRUE)
  old_secret <- if (had_secret) get("CREDENTIAL_SECRET", inherits = TRUE) else NULL

  assign("CON", con, envir = .GlobalEnv)
  assign("app_log_exception", function(...) NULL, envir = .GlobalEnv)
  assign("log_event", function(...) NULL, envir = .GlobalEnv)
  assign("CREDENTIAL_SECRET", "auth-reset-test-secret-value", envir = .GlobalEnv)

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

    if (had_secret) {
      assign("CREDENTIAL_SECRET", old_secret, envir = .GlobalEnv)
    } else if (exists("CREDENTIAL_SECRET", envir = .GlobalEnv, inherits = FALSE)) {
      rm("CREDENTIAL_SECRET", envir = .GlobalEnv)
    }

    dbDisconnect(con, shutdown = TRUE)
    if (file.exists(db_path)) unlink(db_path, force = TRUE)
  }, add = TRUE)

  run_migrations(con)

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

  # Enroll TOTP for the active user
  enrollment <- start_mfa_enrollment(active_user_id, "active.user")
  confirm_result <- confirm_mfa_enrollment(
    active_user_id,
    totp_code_for_step(enrollment$secret, totp_current_step())
  )
  .auth_expect("MFA enrollment confirms with a valid code", isTRUE(confirm_result$success))

  reset_code <- totp_code_for_step(enrollment$secret, totp_current_step() + 1)
  reset_result <- reset_user_password_with_mfa("active.user", reset_code, "NewPass123")
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
      "WHERE event_type = 'password_reset_completed' AND username = 'active.user'"
    )
  )
  insecure_event <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM auth_audit_log WHERE event_type = 'self_service_password_reset_insecure'"
  )
  old_login <- authenticate_user("active.user", "OldPass123")
  new_login <- authenticate_user("active.user", "NewPass123")

  .auth_expect("enrolled user can reset with a valid MFA code", isTRUE(reset_result$success))
  .auth_expect("old password no longer authenticates", !isTRUE(old_login$success))
  .auth_expect("new password authenticates", isTRUE(new_login$success))
  .auth_expect("force password change cleared", !isTRUE(active_user$force_password_change[[1]]))
  .auth_expect("existing sessions are revoked", !is.na(active_session$revoked_at[[1]]))
  .auth_expect("completed reset audit event recorded", reset_event$n[[1]] == 1)
  .auth_expect("insecure reset event type never emitted", insecure_event$n[[1]] == 0)

  wrong_code <- reset_user_password_with_mfa("active.user", "000000", "AnotherPass1")
  .auth_expect("wrong MFA code fails", !isTRUE(wrong_code$success))

  inactive_reset <- reset_user_password_with_mfa("inactive.user", "123456", "ResetPass123")
  missing_reset <- reset_user_password_with_mfa("missing.user", "123456", "ResetPass123")
  .auth_expect("inactive user cannot reset", !isTRUE(inactive_reset$success))
  .auth_expect("unknown username fails cleanly", !isTRUE(missing_reset$success))
  .auth_expect(
    "no account enumeration (identical generic messages)",
    identical(inactive_reset$message, missing_reset$message) &&
      identical(wrong_code$message, missing_reset$message)
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
  .auth_expect(
    "reset view requires an authentication code",
    grepl("reset_code", login_markup, fixed = TRUE)
  )
  .auth_expect(
    "login screen includes the MFA challenge view",
    grepl("mfa_code", login_markup, fixed = TRUE)
  )
  .auth_expect(
    "login screen includes the MFA enrollment view",
    grepl("enroll_code", login_markup, fixed = TRUE)
  )
  .auth_expect(
    "old username-only reset function removed",
    !exists("reset_user_password_by_username", mode = "function")
  )

  list(passed = .auth_passed, failed = .auth_failed)
}
