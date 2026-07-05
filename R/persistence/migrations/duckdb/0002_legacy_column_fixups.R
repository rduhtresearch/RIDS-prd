# Legacy column fixups for databases created before these columns existed.
# Previously run as ALTER-if-missing checks on every boot (R/setup.r); now a
# one-time versioned migration. No-op on fresh databases (0001 creates the
# full current shapes).

migrate <- function(con) {
  add_missing <- function(table, defs) {
    existing <- DBI::dbListFields(con, table)
    for (col in names(defs)) {
      if (!col %in% existing) {
        DBI::dbExecute(con, paste("ALTER TABLE", table, "ADD COLUMN", col, defs[[col]], ";"))
      }
    }
  }

  add_missing("ict_costing_tbl", c(
    study_site = "VARCHAR",
    scenario_id = "VARCHAR"
  ))

  add_missing("meta_data", c(
    speciality_id = "INTEGER",
    study_site = "VARCHAR",
    edge_zip_path = "VARCHAR",
    mff_split_enabled = "BOOLEAN DEFAULT FALSE",
    mff_split_pct = "DOUBLE DEFAULT 0"
  ))
  DBI::dbExecute(con, "UPDATE meta_data SET mff_split_enabled = FALSE WHERE mff_split_enabled IS NULL;")
  DBI::dbExecute(con, "UPDATE meta_data SET mff_split_pct = 0 WHERE mff_split_pct IS NULL;")

  add_missing("users", c(
    user_id = "INTEGER",
    name = "TEXT",
    username = "TEXT",
    email = "TEXT",
    password_hash = "TEXT",
    role = "TEXT DEFAULT 'user'",
    active = "BOOLEAN DEFAULT TRUE",
    force_password_change = "BOOLEAN DEFAULT FALSE",
    created_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
    updated_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
    last_login_at = "TIMESTAMP"
  ))

  add_missing("user_api_credentials", c(
    credential_id = "INTEGER",
    user_id = "INTEGER",
    provider = "TEXT",
    secret_ciphertext = "TEXT",
    secret_nonce = "TEXT",
    created_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
    updated_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
  ))

  add_missing("amount_map", c(calc_method = "TEXT DEFAULT 'STANDARD'"))
  DBI::dbExecute(con, "UPDATE amount_map SET calc_method = 'STANDARD' WHERE calc_method IS NULL;")

  add_missing("posting_lines", c(
    study_site = "VARCHAR",
    activity_type = "VARCHAR",
    time_required = "VARCHAR"
  ))

  add_missing("addon_custom_activities", c(study_site = "VARCHAR"))

  invisible(TRUE)
}
