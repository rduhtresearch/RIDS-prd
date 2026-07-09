# Rebuild user_api_credentials without its foreign key to users.
# Historical: the original table carried FK(user_id) REFERENCES users, which
# blocked user-row rewrites in DuckDB. Previously detected and rebuilt on
# every boot (R/setup.r user_tables()); now a one-time versioned migration.
# No-op on fresh databases (0001 creates the table without the FK).

migrate <- function(con) {
  create_sql <- tryCatch(
    DBI::dbGetQuery(
      con,
      "SELECT sql FROM duckdb_tables() WHERE table_name = 'user_api_credentials' LIMIT 1"
    )$sql[[1]],
    error = function(e) NA_character_
  )

  if (is.na(create_sql) || !grepl("FOREIGN KEY", create_sql, fixed = TRUE)) {
    return(invisible(FALSE))
  }

  DBI::dbExecute(con, "DROP TABLE IF EXISTS user_api_credentials__migrate;")
  DBI::dbExecute(con, "
    CREATE TABLE user_api_credentials__migrate (
      credential_id      INTEGER PRIMARY KEY,
      user_id            INTEGER NOT NULL,
      provider           TEXT NOT NULL,
      secret_ciphertext  TEXT NOT NULL,
      secret_nonce       TEXT NOT NULL,
      created_at         TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at         TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
  ")
  DBI::dbExecute(con, "
    INSERT INTO user_api_credentials__migrate
      (credential_id, user_id, provider, secret_ciphertext, secret_nonce, created_at, updated_at)
    SELECT
      credential_id, user_id, provider, secret_ciphertext, secret_nonce, created_at, updated_at
    FROM user_api_credentials;
  ")
  DBI::dbExecute(con, "DROP TABLE user_api_credentials;")
  DBI::dbExecute(con, "ALTER TABLE user_api_credentials__migrate RENAME TO user_api_credentials;")
  DBI::dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_user_api_credentials_user_provider
      ON user_api_credentials (user_id, provider);
  ")

  invisible(TRUE)
}
