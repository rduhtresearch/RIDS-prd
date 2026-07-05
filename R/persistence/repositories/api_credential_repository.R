# user_api_credentials repository. Stores ciphertext + nonce only;
# encryption/decryption stays at the application layer (R/utils/user_credentials.R).

api_credential_repository <- function(con) {
  list(
    find = function(user_id, provider) {
      rows <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT credential_id, user_id, provider, secret_ciphertext, secret_nonce,",
          "created_at, updated_at",
          "FROM user_api_credentials",
          "WHERE user_id = ? AND provider = ?",
          "LIMIT 1"
        ),
        params = list(as.integer(user_id), provider)
      )
      if (nrow(rows) == 0) NULL else rows[1, , drop = FALSE]
    },

    insert = function(user_id, provider, secret_ciphertext, secret_nonce) {
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO user_api_credentials",
          "(user_id, provider, secret_ciphertext, secret_nonce)",
          "VALUES (?, ?, ?, ?)"
        ),
        params = list(as.integer(user_id), provider, secret_ciphertext, secret_nonce)
      )
      invisible(TRUE)
    },

    update_secret = function(credential_id, secret_ciphertext, secret_nonce) {
      DBI::dbExecute(
        con,
        paste(
          "UPDATE user_api_credentials",
          "SET secret_ciphertext = ?, secret_nonce = ?, updated_at = CURRENT_TIMESTAMP",
          "WHERE credential_id = ?"
        ),
        params = list(secret_ciphertext, secret_nonce, credential_id)
      )
      invisible(TRUE)
    },

    delete = function(user_id, provider) {
      as.integer(DBI::dbExecute(
        con,
        "DELETE FROM user_api_credentials WHERE user_id = ? AND provider = ?",
        params = list(as.integer(user_id), provider)
      ))
    }
  )
}
