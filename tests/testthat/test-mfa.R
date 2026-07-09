# MFA enrollment/verification flow and the MFA-gated password reset that
# replaced the insecure username-only reset (approved behavior change).

mfa_test_db <- function(env = parent.frame()) {
  source_from_root("R/utils/auth.r")
  source_from_root("R/utils/user_credentials.R")
  source_from_root("R/auth/mfa.R")

  db_path <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

  old_con <- if (exists("CON", inherits = TRUE)) get("CON", inherits = TRUE) else NULL
  assign("CON", con, envir = .GlobalEnv)
  assign("app_log_exception", function(...) NULL, envir = .GlobalEnv)
  assign("log_event", function(...) NULL, envir = .GlobalEnv)
  assign("CREDENTIAL_SECRET", "mfa-test-secret-mfa-test-secret", envir = .GlobalEnv)

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

make_user <- function(username = "mfa.user", password = "MfaPass123") {
  create_user_account(
    name = "MFA User", username = username,
    temporary_password = password, active = TRUE
  )$user$user_id[[1]]
}

current_code_for_user <- function(uid, offset = 0L) {
  factor <- rids_repos()$mfa$find_factor(uid)
  secret <- mfa_decrypt_secret(factor$secret_ciphertext[[1]], factor$secret_nonce[[1]])
  totp_code_for_step(secret, totp_current_step() + offset)
}

test_that("enrollment: start, confirm with valid code, recovery codes issued", {
  con <- mfa_test_db()
  uid <- make_user()

  expect_false(user_mfa_enrolled(uid))

  enrollment <- start_mfa_enrollment(uid, "mfa.user")
  expect_match(enrollment$secret, "^[A-Z2-7]+$")
  expect_match(enrollment$provisioning_uri, "^otpauth://totp/")
  expect_false(user_mfa_enrolled(uid))  # not verified yet

  bad <- confirm_mfa_enrollment(uid, "000000")
  expect_false(bad$success)

  good <- confirm_mfa_enrollment(uid, totp_code_for_step(enrollment$secret, totp_current_step()))
  expect_true(good$success)
  expect_length(good$recovery_codes, 8)
  expect_true(user_mfa_enrolled(uid))

  audit <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM auth_audit_log WHERE event_type = 'mfa_enrolled'"
  )$n[[1]]
  expect_equal(audit, 1)
})

test_that("verification: valid code accepted once (replay rejected), recovery code single-use", {
  con <- mfa_test_db()
  uid <- make_user()

  enrollment <- start_mfa_enrollment(uid, "mfa.user")
  confirmed <- confirm_mfa_enrollment(
    uid, totp_code_for_step(enrollment$secret, totp_current_step())
  )

  # confirm consumed the current step; use the next step's code
  next_code <- totp_code_for_step(enrollment$secret, totp_current_step() + 1)
  first <- verify_mfa_code(uid, next_code)
  expect_true(first$success)
  expect_identical(first$method, "totp")

  replay <- verify_mfa_code(uid, next_code)
  expect_false(replay$success)
  expect_identical(replay$reason, "code_replayed")

  wrong <- verify_mfa_code(uid, "123456")
  expect_false(wrong$success)

  recovery <- confirmed$recovery_codes[[1]]
  rec1 <- verify_mfa_code(uid, recovery)
  expect_true(rec1$success)
  expect_identical(rec1$method, "recovery_code")

  rec2 <- verify_mfa_code(uid, recovery)
  expect_false(rec2$success)
})

test_that("admin MFA reset clears enrollment so the user re-enrolls", {
  con <- mfa_test_db()
  uid <- make_user()

  enrollment <- start_mfa_enrollment(uid, "mfa.user")
  confirm_mfa_enrollment(uid, totp_code_for_step(enrollment$secret, totp_current_step()))
  expect_true(user_mfa_enrolled(uid))

  res <- admin_reset_user_mfa(uid, actor_user_id = 99L)
  expect_true(res$success)
  expect_false(user_mfa_enrolled(uid))
  expect_null(rids_repos()$mfa$find_factor(uid))

  audit <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM auth_audit_log WHERE event_type = 'admin_mfa_reset'"
  )$n[[1]]
  expect_equal(audit, 1)
})

test_that("MFA-gated reset: succeeds with valid code, same semantics as before otherwise", {
  con <- mfa_test_db()
  uid <- make_user(username = "reset.mfa", password = "OldPass123")

  enrollment <- start_mfa_enrollment(uid, "reset.mfa")
  confirm_mfa_enrollment(uid, totp_code_for_step(enrollment$secret, totp_current_step()))

  sess <- create_auth_session(uid, duration_hours = 1)

  code <- totp_code_for_step(enrollment$secret, totp_current_step() + 1)
  res <- reset_user_password_with_mfa("reset.mfa", code, "NewPass456")
  expect_true(res$success)

  # same downstream semantics as the old reset: new password works, old
  # doesn't, sessions revoked, force flag cleared
  expect_true(authenticate_user("reset.mfa", "NewPass456")$success)
  expect_false(isTRUE(authenticate_user("reset.mfa", "OldPass123")$success))
  open_sessions <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM auth_sessions WHERE user_id = ? AND revoked_at IS NULL",
    params = list(uid)
  )$n[[1]]
  expect_equal(open_sessions, 0)
  expect_false(isTRUE(get_user_by_id(uid)$force_password_change[[1]]))

  completed <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM auth_audit_log WHERE event_type = 'password_reset_completed'"
  )$n[[1]]
  expect_equal(completed, 1)

  # the old insecure event type is never emitted
  insecure <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM auth_audit_log WHERE event_type = 'self_service_password_reset_insecure'"
  )$n[[1]]
  expect_equal(insecure, 0)
})

test_that("MFA-gated reset: no account enumeration", {
  con <- mfa_test_db()
  uid <- make_user(username = "enum.check", password = "SomePass123")
  enrollment <- start_mfa_enrollment(uid, "enum.check")
  confirm_mfa_enrollment(uid, totp_code_for_step(enrollment$secret, totp_current_step()))

  unknown_user <- reset_user_password_with_mfa("no.such.user", "123456", "NewPass456")
  wrong_code <- reset_user_password_with_mfa("enum.check", "000000", "NewPass456")

  expect_false(unknown_user$success)
  expect_false(wrong_code$success)
  # identical generic message either way
  expect_identical(unknown_user$message, wrong_code$message)

  # inactive users get the same generic response
  DBI::dbExecute(con, "UPDATE users SET active = FALSE WHERE user_id = ?", params = list(uid))
  inactive <- reset_user_password_with_mfa("enum.check", "000000", "NewPass456")
  expect_identical(inactive$message, unknown_user$message)

  # password unchanged throughout
  DBI::dbExecute(con, "UPDATE users SET active = TRUE WHERE user_id = ?", params = list(uid))
  expect_true(authenticate_user("enum.check", "SomePass123")$success)
})

test_that("unenrolled users cannot use the self-service reset", {
  con <- mfa_test_db()
  make_user(username = "no.mfa", password = "SomePass123")

  res <- reset_user_password_with_mfa("no.mfa", "123456", "NewPass456")
  expect_false(res$success)
  expect_true(authenticate_user("no.mfa", "SomePass123")$success)
})
