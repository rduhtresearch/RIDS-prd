# MFA repository: mfa_factors + mfa_recovery_codes. Stores ciphertext/hashes
# only; TOTP secret encryption and code hashing live in R/auth/mfa.R.

mfa_repository <- function(con) {
  list(
    find_factor = function(user_id, method = "totp") {
      rows <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT factor_id, user_id, method, secret_ciphertext, secret_nonce,",
          "verified_at, last_used_step, created_at",
          "FROM mfa_factors WHERE user_id = ? AND method = ? LIMIT 1"
        ),
        params = list(as.integer(user_id), method)
      )
      if (nrow(rows) == 0) NULL else rows[1, , drop = FALSE]
    },

    upsert_factor = function(user_id, secret_ciphertext, secret_nonce, method = "totp") {
      DBI::dbExecute(
        con,
        "DELETE FROM mfa_factors WHERE user_id = ? AND method = ?",
        params = list(as.integer(user_id), method)
      )
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO mfa_factors (user_id, method, secret_ciphertext, secret_nonce)",
          "VALUES (?, ?, ?, ?)"
        ),
        params = list(as.integer(user_id), method, secret_ciphertext, secret_nonce)
      )
      invisible(TRUE)
    },

    mark_factor_verified = function(factor_id) {
      DBI::dbExecute(
        con,
        "UPDATE mfa_factors SET verified_at = CURRENT_TIMESTAMP WHERE factor_id = ?",
        params = list(factor_id)
      )
      invisible(TRUE)
    },

    set_last_used_step = function(factor_id, step) {
      DBI::dbExecute(
        con,
        "UPDATE mfa_factors SET last_used_step = ? WHERE factor_id = ?",
        params = list(as.numeric(step), factor_id)
      )
      invisible(TRUE)
    },

    delete_factors_for_user = function(user_id) {
      DBI::dbExecute(
        con,
        "DELETE FROM mfa_factors WHERE user_id = ?",
        params = list(as.integer(user_id))
      )
      invisible(TRUE)
    },

    replace_recovery_codes = function(user_id, code_hashes) {
      DBI::dbExecute(
        con,
        "DELETE FROM mfa_recovery_codes WHERE user_id = ?",
        params = list(as.integer(user_id))
      )
      for (code_hash in code_hashes) {
        DBI::dbExecute(
          con,
          "INSERT INTO mfa_recovery_codes (user_id, code_hash) VALUES (?, ?)",
          params = list(as.integer(user_id), code_hash)
        )
      }
      invisible(TRUE)
    },

    find_unused_recovery_code = function(user_id, code_hash) {
      rows <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT code_id FROM mfa_recovery_codes",
          "WHERE user_id = ? AND code_hash = ? AND used_at IS NULL LIMIT 1"
        ),
        params = list(as.integer(user_id), code_hash)
      )
      if (nrow(rows) == 0) NULL else rows$code_id[[1]]
    },

    mark_recovery_code_used = function(code_id) {
      DBI::dbExecute(
        con,
        "UPDATE mfa_recovery_codes SET used_at = CURRENT_TIMESTAMP WHERE code_id = ?",
        params = list(code_id)
      )
      invisible(TRUE)
    },

    delete_recovery_codes_for_user = function(user_id) {
      DBI::dbExecute(
        con,
        "DELETE FROM mfa_recovery_codes WHERE user_id = ?",
        params = list(as.integer(user_id))
      )
      invisible(TRUE)
    }
  )
}
