normalize_credential_provider <- function(provider) {
  provider <- tolower(trimws(as.character(provider %||% "")))

  if (!nzchar(provider)) {
    stop("Credential provider is required.")
  }

  provider
}

get_credential_secret <- function() {
  secret <- trimws(as.character(get0("CREDENTIAL_SECRET", ifnotfound = "", inherits = TRUE)))

  if (!nzchar(secret) || nchar(secret) < 16) {
    stop("CREDENTIAL_SECRET is missing or too short.")
  }

  secret
}

credential_key <- function() {
  sodium::sha256(charToRaw(enc2utf8(get_credential_secret())))
}

encrypt_api_secret <- function(secret) {
  secret <- trimws(as.character(secret %||% ""))
  if (!nzchar(secret)) {
    stop("API secret cannot be empty.")
  }

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

decrypt_api_secret <- function(secret_ciphertext, secret_nonce) {
  ciphertext <- sodium::hex2bin(as.character(secret_ciphertext %||% ""))
  nonce <- sodium::hex2bin(as.character(secret_nonce %||% ""))

  raw_value <- sodium::data_decrypt(
    ciphertext,
    key = credential_key(),
    nonce = nonce
  )

  enc2utf8(rawToChar(raw_value))
}

mask_api_secret <- function(secret) {
  secret <- trimws(as.character(secret %||% ""))
  if (!nzchar(secret)) {
    return("")
  }

  if (nchar(secret) <= 4) {
    return("****")
  }

  paste0(strrep("*", max(4L, nchar(secret) - 4L)), substr(secret, nchar(secret) - 3L, nchar(secret)))
}

get_user_api_credential_row <- function(user_id, provider) {
  provider <- normalize_credential_provider(provider)

  rows <- DBI::dbGetQuery(
    CON,
    paste(
      "SELECT credential_id, user_id, provider, secret_ciphertext, secret_nonce,",
      "created_at, updated_at",
      "FROM user_api_credentials",
      "WHERE user_id = ? AND provider = ?",
      "LIMIT 1"
    ),
    params = list(as.integer(user_id), provider)
  )

  if (nrow(rows) == 0) {
    return(NULL)
  }

  rows[1, , drop = FALSE]
}

save_user_api_credential <- function(user_id, provider, secret) {
  provider <- normalize_credential_provider(provider)
  secret <- trimws(as.character(secret %||% ""))

  if (!nzchar(secret)) {
    return(list(success = FALSE, message = "API key is required."))
  }

  encrypted <- tryCatch(
    encrypt_api_secret(secret),
    error = function(e) {
      app_log_exception("integrations", "API key encryption failed", e, list(user_id = user_id, provider = provider))
      NULL
    }
  )

  if (is.null(encrypted)) {
    return(list(success = FALSE, message = "Unable to save the API key."))
  }

  tryCatch({
    existing <- get_user_api_credential_row(user_id, provider)

    if (is.null(existing)) {
      DBI::dbExecute(
        CON,
        paste(
          "INSERT INTO user_api_credentials",
          "(user_id, provider, secret_ciphertext, secret_nonce)",
          "VALUES (?, ?, ?, ?)"
        ),
        params = list(
          as.integer(user_id),
          provider,
          encrypted$secret_ciphertext,
          encrypted$secret_nonce
        )
      )
    } else {
      DBI::dbExecute(
        CON,
        paste(
          "UPDATE user_api_credentials",
          "SET secret_ciphertext = ?, secret_nonce = ?, updated_at = CURRENT_TIMESTAMP",
          "WHERE credential_id = ?"
        ),
        params = list(
          encrypted$secret_ciphertext,
          encrypted$secret_nonce,
          existing$credential_id[[1]]
        )
      )
    }

    log_event(
      level = "INFO",
      area = "integrations",
      message = "User API credential saved",
      user_id = user_id,
      username = tryCatch(get_user_by_id(user_id)$username[[1]], error = function(e) NA_character_),
      details = list(provider = provider)
    )

    list(success = TRUE, message = "API key saved.")
  }, error = function(e) {
    app_log_exception("integrations", "API key save failed", e, list(user_id = user_id, provider = provider))
    list(success = FALSE, message = "Unable to save the API key.")
  })
}

get_user_api_credential <- function(user_id, provider) {
  provider <- normalize_credential_provider(provider)

  row <- tryCatch(
    get_user_api_credential_row(user_id, provider),
    error = function(e) {
      app_log_exception("integrations", "Credential lookup failed", e, list(user_id = user_id, provider = provider))
      NULL
    }
  )

  if (is.null(row)) {
    return(NULL)
  }

  tryCatch(
    decrypt_api_secret(row$secret_ciphertext[[1]], row$secret_nonce[[1]]),
    error = function(e) {
      app_log_exception("integrations", "Credential decrypt failed", e, list(user_id = user_id, provider = provider))
      NULL
    }
  )
}

delete_user_api_credential <- function(user_id, provider) {
  provider <- normalize_credential_provider(provider)

  tryCatch({
    deleted <- DBI::dbExecute(
      CON,
      "DELETE FROM user_api_credentials WHERE user_id = ? AND provider = ?",
      params = list(as.integer(user_id), provider)
    )

    if (deleted > 0) {
      log_event(
        level = "INFO",
        area = "integrations",
        message = "User API credential deleted",
        user_id = user_id,
        username = tryCatch(get_user_by_id(user_id)$username[[1]], error = function(e) NA_character_),
        details = list(provider = provider)
      )
    }

    isTRUE(deleted > 0)
  }, error = function(e) {
    app_log_exception("integrations", "Credential delete failed", e, list(user_id = user_id, provider = provider))
    FALSE
  })
}

get_user_api_credential_status <- function(user_id, provider) {
  provider <- normalize_credential_provider(provider)

  row <- tryCatch(
    get_user_api_credential_row(user_id, provider),
    error = function(e) {
      app_log_exception("integrations", "Credential status lookup failed", e, list(user_id = user_id, provider = provider))
      NULL
    }
  )

  if (is.null(row)) {
    return(list(
      configured = FALSE,
      provider = provider,
      masked_secret = "",
      updated_at = NULL
    ))
  }

  secret <- get_user_api_credential(user_id, provider)

  list(
    configured = !is.null(secret) && nzchar(secret),
    provider = provider,
    masked_secret = mask_api_secret(secret),
    updated_at = row$updated_at[[1]]
  )
}
