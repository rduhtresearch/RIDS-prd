# MFA orchestration: TOTP enrollment, verification, recovery codes, and the
# MFA-gated self-service password reset that replaces the old username-only
# reset. TOTP secrets are stored encrypted with the app credential key (same
# scheme as user_api_credentials); recovery codes are stored hashed.

source("R/auth/totp.R", local = FALSE)
source("R/persistence/repositories/mfa_repository.R", local = FALSE)

mfa_encrypt_secret <- function(secret) {
  nonce <- sodium::random(24)
  ciphertext <- sodium::data_encrypt(
    charToRaw(enc2utf8(secret)),
    key = credential_key(),
    nonce = nonce
  )
  list(
    secret_ciphertext = sodium::bin2hex(ciphertext),
    secret_nonce = sodium::bin2hex(nonce)
  )
}

mfa_decrypt_secret <- function(secret_ciphertext, secret_nonce) {
  raw_value <- sodium::data_decrypt(
    sodium::hex2bin(as.character(secret_ciphertext)),
    key = credential_key(),
    nonce = sodium::hex2bin(as.character(secret_nonce))
  )
  enc2utf8(rawToChar(raw_value))
}

hash_recovery_code <- function(code) {
  code <- toupper(gsub("[^A-Za-z0-9]", "", as.character(code %||% "")))
  sodium::bin2hex(sodium::hash(charToRaw(code)))
}

generate_recovery_codes <- function(n = 8L) {
  vapply(seq_len(n), function(i) {
    raw_part <- sodium::bin2hex(sodium::random(5))
    toupper(paste0(substr(raw_part, 1, 5), "-", substr(raw_part, 6, 10)))
  }, character(1))
}

#' Whether the user has a verified MFA factor.
user_mfa_enrolled <- function(user_id) {
  factor <- rids_repos()$mfa$find_factor(user_id)
  !is.null(factor) && !is.na(factor$verified_at[[1]])
}

#' Begin (or restart) TOTP enrollment: mint a secret, store it encrypted and
#' unverified, and return what the UI needs to display.
start_mfa_enrollment <- function(user_id, username) {
  secret <- totp_generate_secret()
  encrypted <- mfa_encrypt_secret(secret)

  rids_repos()$mfa$upsert_factor(
    user_id = user_id,
    secret_ciphertext = encrypted$secret_ciphertext,
    secret_nonce = encrypted$secret_nonce
  )

  list(
    secret = secret,
    provisioning_uri = totp_provisioning_uri(secret, username)
  )
}

#' Confirm enrollment with a first valid code. Issues recovery codes on
#' success (returned once, stored only as hashes).
confirm_mfa_enrollment <- function(user_id, code) {
  factor <- rids_repos()$mfa$find_factor(user_id)
  if (is.null(factor)) {
    return(list(success = FALSE, message = "No enrollment in progress."))
  }

  secret <- mfa_decrypt_secret(factor$secret_ciphertext[[1]], factor$secret_nonce[[1]])
  step <- totp_verify_code(secret, code)
  if (is.null(step)) {
    return(list(success = FALSE, message = "That code didn't match. Try again."))
  }

  rids_repos()$mfa$mark_factor_verified(factor$factor_id[[1]])
  rids_repos()$mfa$set_last_used_step(factor$factor_id[[1]], step)

  recovery_codes <- generate_recovery_codes()
  rids_repos()$mfa$replace_recovery_codes(
    user_id,
    vapply(recovery_codes, hash_recovery_code, character(1))
  )

  log_auth_event(
    event_type = "mfa_enrolled",
    user_id = user_id,
    actor_user_id = user_id,
    success = TRUE,
    message = "TOTP factor enrolled and verified"
  )

  list(success = TRUE, recovery_codes = recovery_codes)
}

#' Verify a login/reset MFA challenge: accepts a current TOTP code (with
#' replay protection) or an unused recovery code (consumed on use).
verify_mfa_code <- function(user_id, code) {
  factor <- rids_repos()$mfa$find_factor(user_id)
  if (is.null(factor) || is.na(factor$verified_at[[1]])) {
    return(list(success = FALSE, reason = "not_enrolled"))
  }

  secret <- mfa_decrypt_secret(factor$secret_ciphertext[[1]], factor$secret_nonce[[1]])
  step <- totp_verify_code(secret, code)

  if (!is.null(step)) {
    last_used <- factor$last_used_step[[1]]
    if (!is.na(last_used) && step <= as.numeric(last_used)) {
      return(list(success = FALSE, reason = "code_replayed"))
    }
    rids_repos()$mfa$set_last_used_step(factor$factor_id[[1]], step)
    return(list(success = TRUE, method = "totp"))
  }

  code_id <- rids_repos()$mfa$find_unused_recovery_code(user_id, hash_recovery_code(code))
  if (!is.null(code_id)) {
    rids_repos()$mfa$mark_recovery_code_used(code_id)
    log_auth_event(
      event_type = "mfa_recovery_code_used",
      user_id = user_id,
      actor_user_id = user_id,
      success = TRUE,
      message = "Recovery code accepted for MFA challenge"
    )
    return(list(success = TRUE, method = "recovery_code"))
  }

  list(success = FALSE, reason = "invalid_code")
}

#' Admin: clear a user's MFA enrollment so they re-enroll at next login.
admin_reset_user_mfa <- function(user_id, actor_user_id = NULL) {
  user_row <- get_user_by_id(user_id)
  if (is.null(user_row)) {
    return(list(success = FALSE, message = "User not found."))
  }

  rids_repos()$mfa$delete_factors_for_user(user_id)
  rids_repos()$mfa$delete_recovery_codes_for_user(user_id)

  log_auth_event(
    event_type = "admin_mfa_reset",
    user_id = user_id,
    actor_user_id = actor_user_id,
    username = user_row$username[[1]],
    success = TRUE,
    message = "MFA enrollment cleared by admin"
  )

  list(success = TRUE)
}

#' MFA-gated self-service password reset. Replaces the old username-only
#' reset: the caller must present a valid TOTP or recovery code for the
#' account. Responses are identical for unknown username, inactive user, and
#' wrong code — no account enumeration.
reset_user_password_with_mfa <- function(username, code, new_password) {
  generic_failure <- list(
    success = FALSE,
    message = "Reset failed. Check your username and authentication code."
  )

  user_row <- get_user_by_username(username)
  if (is.null(user_row)) {
    log_auth_event(
      event_type = "password_reset_failed",
      username = username,
      success = FALSE,
      message = "Self-service reset: unknown username"
    )
    return(generic_failure)
  }

  row <- user_row[1, , drop = FALSE]

  if (!isTRUE(row$active[[1]])) {
    log_auth_event(
      event_type = "password_reset_failed",
      user_id = row$user_id[[1]],
      username = row$username[[1]],
      success = FALSE,
      message = "Self-service reset: inactive user"
    )
    return(generic_failure)
  }

  mfa_result <- verify_mfa_code(row$user_id[[1]], code)
  if (!isTRUE(mfa_result$success)) {
    log_auth_event(
      event_type = "password_reset_failed",
      user_id = row$user_id[[1]],
      username = row$username[[1]],
      success = FALSE,
      message = paste("Self-service reset: MFA verification failed:", mfa_result$reason %||% "invalid")
    )
    return(generic_failure)
  }

  password_hash <- hash_password(new_password)
  if (is.null(password_hash)) {
    return(list(success = FALSE, message = "Failed to reset password."))
  }

  rids_repos()$users$set_password_hash(row$user_id[[1]], password_hash, force_password_change = FALSE)
  rids_repos()$sessions$revoke_all_for_user(row$user_id[[1]])

  log_auth_event(
    event_type = "password_reset_completed",
    user_id = row$user_id[[1]],
    actor_user_id = row$user_id[[1]],
    username = row$username[[1]],
    success = TRUE,
    message = paste0("Self-service password reset completed (MFA method: ", mfa_result$method, ")")
  )

  list(success = TRUE, user = get_user_by_id(row$user_id[[1]]))
}
