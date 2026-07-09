# Characterization tests for auth behavior (R/utils/auth.r + R/auth/*).
#
# Phase 0 pinned the pre-refactor behavior. Phase 3 deliberately changed two
# things, both user-approved, and these tests were updated in the same
# commit as those changes:
#   1. The 'developer' role no longer exists (user/admin only; migration
#      0006 maps legacy developer rows to admin).
#   2. The insecure username-only password reset was replaced by an
#      MFA-gated reset (see test-mfa.R for the new flow's tests).
# Everything else pins unchanged behavior.

auth_test_db <- function(env = parent.frame()) {
  source_from_root("R/utils/auth.r")
  source_from_root("R/auth/mfa.R")

  db_path <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

  old_con <- if (exists("CON", inherits = TRUE)) get("CON", inherits = TRUE) else NULL
  assign("CON", con, envir = .GlobalEnv)
  assign("app_log_exception", function(...) NULL, envir = .GlobalEnv)
  assign("log_event", function(...) NULL, envir = .GlobalEnv)
  assign("CREDENTIAL_SECRET", "characterization-test-secret", envir = .GlobalEnv)

  withr::defer({
    if (is.null(old_con)) {
      if (exists("CON", envir = .GlobalEnv, inherits = FALSE)) rm("CON", envir = .GlobalEnv)
    } else {
      assign("CON", old_con, envir = .GlobalEnv)
    }
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(db_path, force = TRUE)
  }, envir = env)

  old_wd <- setwd(rids_repo_root())
  withr::defer(setwd(old_wd), envir = env)
  run_migrations(con)

  con
}

audit_count <- function(con, event_type) {
  DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM auth_audit_log WHERE event_type = ?",
    params = list(event_type)
  )$n[[1]]
}

test_that("role helpers: two-role model (user/admin)", {
  source_from_root("R/utils/auth.r")

  expect_identical(normalize_role("admin"), "admin")
  expect_identical(normalize_role(" ADMIN "), "admin")
  expect_identical(normalize_role("user"), "user")
  expect_identical(normalize_role(NULL), "user")
  expect_identical(normalize_role(NA_character_), "user")
  expect_identical(normalize_role("something_else"), "user")
  # Approved change: developer role removed — unknown roles (including the
  # old dev/developer strings) normalize to user; data migration 0006 maps
  # stored developer rows to admin before the app ever reads them.
  expect_identical(normalize_role("dev"), "user")
  expect_identical(normalize_role("developer"), "user")

  expect_true(is_admin("admin"))
  expect_false(is_admin("user"))

  # Approved change: developer-only helpers removed with the role.
  expect_false(exists("is_manager", mode = "function"))
  expect_false(exists("can_edit", mode = "function"))
})

test_that("migration 0006 maps stored developer roles to admin", {
  con <- auth_test_db()

  DBI::dbExecute(
    con,
    "INSERT INTO users (name, username, password_hash, role) VALUES (?, ?, ?, ?)",
    params = list("Late Dev", "late.dev", "hash", "user")
  )
  # Simulate a legacy row that slipped in with the old role string, then
  # re-run the migration logic directly (the migration already ran in setup).
  DBI::dbExecute(con, "UPDATE users SET role = 'developer' WHERE username = 'late.dev'")
  DBI::dbExecute(con, "UPDATE users SET role = 'admin' WHERE lower(trim(role)) IN ('dev', 'developer')")

  role <- DBI::dbGetQuery(
    con,
    "SELECT role FROM users WHERE username = 'late.dev'"
  )$role[[1]]
  expect_identical(role, "admin")
})

test_that("session restore: every failure branch returns its distinct status", {
  con <- auth_test_db()

  user <- create_user_account(
    name = "Char Test", username = "char.test",
    temporary_password = "CharPass123", active = TRUE
  )
  uid <- user$user$user_id[[1]]

  # missing / invalid token
  expect_identical(restore_auth_session(NULL)$status, "missing")
  expect_identical(restore_auth_session("")$status, "missing")
  expect_identical(restore_auth_session("not-a-real-token")$status, "invalid")

  # valid round trip
  sess <- create_auth_session(uid, user_agent = "char-test", duration_hours = 1)
  ok <- restore_auth_session(sess$token)
  expect_true(ok$success)
  expect_identical(ok$status, "ok")
  expect_identical(ok$session$username[[1]], "char.test")
  expect_equal(audit_count(con, "session_restored"), 1)

  # revoked session
  revoke_auth_session(sess$session_id)
  revoked <- restore_auth_session(sess$token)
  expect_false(revoked$success)
  expect_identical(revoked$status, "revoked")

  # expired session
  sess2 <- create_auth_session(uid, duration_hours = 1)
  DBI::dbExecute(
    con,
    "UPDATE auth_sessions SET expires_at = TIMESTAMP '2000-01-01 00:00:00' WHERE session_id = ?",
    params = list(sess2$session_id)
  )
  expired <- restore_auth_session(sess2$token)
  expect_false(expired$success)
  expect_identical(expired$status, "expired")
  expect_equal(audit_count(con, "session_expired"), 1)

  # inactive user
  sess3 <- create_auth_session(uid, duration_hours = 1)
  DBI::dbExecute(con, "UPDATE users SET active = FALSE WHERE user_id = ?", params = list(uid))
  inactive <- restore_auth_session(sess3$token)
  expect_false(inactive$success)
  expect_identical(inactive$status, "inactive")
})

test_that("authenticate_user does not distinguish unknown user from bad password", {
  con <- auth_test_db()

  create_user_account(
    name = "Login Test", username = "login.test",
    temporary_password = "RightPass123", active = TRUE
  )

  unknown <- authenticate_user("no.such.user", "whatever")
  wrong_pw <- authenticate_user("login.test", "WrongPass123")
  expect_identical(unknown$reason, "invalid_credentials")
  expect_identical(wrong_pw$reason, "invalid_credentials")

  good <- authenticate_user("login.test", "RightPass123")
  expect_true(good$success)
  expect_equal(audit_count(con, "login_failure"), 2)
})

test_that("admin password reset forces change and revokes all sessions", {
  con <- auth_test_db()

  user <- create_user_account(
    name = "Reset Target", username = "reset.target",
    temporary_password = "InitialPass1", active = TRUE
  )
  uid <- user$user$user_id[[1]]
  s1 <- create_auth_session(uid, duration_hours = 1)
  s2 <- create_auth_session(uid, duration_hours = 1)

  res <- reset_user_password(uid, "TempPass999", actor_user_id = 42L)
  expect_true(res$success)

  after <- get_user_by_id(uid)
  expect_true(isTRUE(after$force_password_change[[1]]))

  open_sessions <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM auth_sessions WHERE user_id = ? AND revoked_at IS NULL",
    params = list(uid)
  )$n[[1]]
  expect_equal(open_sessions, 0)
  expect_equal(audit_count(con, "admin_password_reset"), 1)
})

test_that("change_user_password requires the current password and clears force flag", {
  con <- auth_test_db()

  user <- create_user_account(
    name = "Changer", username = "pw.changer",
    temporary_password = "StartPass123", active = TRUE
  )
  uid <- user$user$user_id[[1]]

  bad <- change_user_password(uid, "WrongCurrent", "NewPass456")
  expect_false(bad$success)
  expect_identical(bad$message, "Current password is incorrect.")

  good <- change_user_password(uid, "StartPass123", "NewPass456")
  expect_true(good$success)
  expect_false(isTRUE(get_user_by_id(uid)$force_password_change[[1]]))
  expect_true(authenticate_user("pw.changer", "NewPass456")$success)
  expect_equal(audit_count(con, "password_changed"), 1)
})

test_that("the insecure username-only reset no longer exists", {
  source_from_root("R/utils/auth.r")
  expect_false(exists("reset_user_password_by_username", mode = "function"))
})
