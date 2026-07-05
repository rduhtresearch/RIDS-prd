# ==============================================================================
# SETUP & AUTHENTICATION
# ==============================================================================
source("R/utils/auth.r", local = FALSE)
source("R/addons/custom_activities/ca_schema.R", local = FALSE)
source("R/addons/custom_activities/ca_ref_activities.R", local = FALSE)

# ==============================================================================
# 01_BUILD_RULES_DB_AH.R
# Purpose: Build + seed DuckDB rules DB for Scenarios A–H
# ==============================================================================

## Init database ---------------------------------------------------------------
init_db <- function() {
  tryCatch({
    dbGetQuery(CON, "SELECT 1")
  }, error = function(e) {
    stop("DB ERROR: ", e$message)
  })
}

## ICT Cost data  --------------------------------------------------------------
ict_table <- function() {
  query <- c("
     CREATE TABLE IF NOT EXISTS ict_costing_tbl (
     CPMS_ID                VARCHAR,
     study_site             VARCHAR,
     scenario_id            VARCHAR,
     Study                  VARCHAR,
     Visit_Number           VARCHAR,
     Study_Arm              VARCHAR,
     Visit_Label            VARCHAR,
     Activity_Name          VARCHAR,
     ICT_Cost               DOUBLE,
     Contract_Cost          DOUBLE,
     activity_occurrence_id INTEGER,
     staff_group            INTEGER
     );"
  )
  
  dbExecute(CON, query)

  ict_cols <- dbListFields(CON, "ict_costing_tbl")
  if (!"study_site" %in% ict_cols) {
    dbExecute(CON, "ALTER TABLE ict_costing_tbl ADD COLUMN study_site VARCHAR;")
  }
  if (!"scenario_id" %in% ict_cols) {
    dbExecute(CON, "ALTER TABLE ict_costing_tbl ADD COLUMN scenario_id VARCHAR;")
  }
}

## App data --------------------------------------------------------------------
meta_table <- function() {
  queries <- c(
    "CREATE SEQUENCE IF NOT EXISTS upload_id_seq;",
    "CREATE TABLE IF NOT EXISTS meta_data (
      id                INTEGER PRIMARY KEY DEFAULT nextval('upload_id_seq'),
      cpms_id           VARCHAR,
      study_site        VARCHAR,
      scenario_id       VARCHAR,
      edge_id           VARCHAR,
      study_name        VARCHAR,
      notes             VARCHAR,
      uploaded_by       VARCHAR,
      upload_timestamp  TIMESTAMP DEFAULT current_timestamp,
      original_filename VARCHAR,
      saved_file_path   VARCHAR,
      speciality_id     INTEGER,
      edge_zip_path     VARCHAR,
      mff_split_enabled BOOLEAN DEFAULT FALSE,
      mff_split_pct     DOUBLE DEFAULT 0
    );"
  )
  
  for (query in queries) {
    dbExecute(CON, query)
  }
  
  # Idempotent column add for pre-existing DBs that were built before
  # speciality_id was part of the schema.
  meta_cols <- dbListFields(CON, "meta_data")
  if (!"speciality_id" %in% meta_cols) {
    dbExecute(CON, "ALTER TABLE meta_data ADD COLUMN speciality_id INTEGER;")
  }

  if (!"study_site" %in% meta_cols) {
    dbExecute(CON, "ALTER TABLE meta_data ADD COLUMN study_site VARCHAR;")
  }
  
  if (!"edge_zip_path" %in% meta_cols) {
    dbExecute(CON, "ALTER TABLE meta_data ADD COLUMN edge_zip_path VARCHAR;")
  }
  if (!"mff_split_enabled" %in% meta_cols) {
    dbExecute(CON, "ALTER TABLE meta_data ADD COLUMN mff_split_enabled BOOLEAN DEFAULT FALSE;")
  }
  if (!"mff_split_pct" %in% meta_cols) {
    dbExecute(CON, "ALTER TABLE meta_data ADD COLUMN mff_split_pct DOUBLE DEFAULT 0;")
  }

  dbExecute(CON, "UPDATE meta_data SET mff_split_enabled = FALSE WHERE mff_split_enabled IS NULL;")
  dbExecute(CON, "UPDATE meta_data SET mff_split_pct = 0 WHERE mff_split_pct IS NULL;")

  dbExecute(CON, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_meta_data_unique_study_identity
      ON meta_data (cpms_id, study_site, scenario_id);
  ")

  tryCatch({
    dbExecute(CON, "
      UPDATE meta_data
      SET
        cpms_id = REPLACE(cpms_id, chr(0), ''),
        study_site = REPLACE(study_site, chr(0), ''),
        scenario_id = REPLACE(scenario_id, chr(0), ''),
        edge_id = REPLACE(edge_id, chr(0), ''),
        study_name = REPLACE(study_name, chr(0), ''),
        notes = REPLACE(notes, chr(0), ''),
        uploaded_by = REPLACE(uploaded_by, chr(0), ''),
        original_filename = REPLACE(original_filename, chr(0), ''),
        saved_file_path = REPLACE(saved_file_path, chr(0), ''),
        edge_zip_path = REPLACE(edge_zip_path, chr(0), '')
      WHERE
        strpos(cpms_id, chr(0)) > 0 OR
        strpos(study_site, chr(0)) > 0 OR
        strpos(scenario_id, chr(0)) > 0 OR
        strpos(edge_id, chr(0)) > 0 OR
        strpos(study_name, chr(0)) > 0 OR
        strpos(notes, chr(0)) > 0 OR
        strpos(uploaded_by, chr(0)) > 0 OR
        strpos(original_filename, chr(0)) > 0 OR
        strpos(saved_file_path, chr(0)) > 0 OR
        strpos(edge_zip_path, chr(0)) > 0;
    ")
  }, error = function(e) {
  })
}
## User tables -----------------------------------------------------------------
user_tables <- function() {
  tryCatch({
    existing_tables <- tryCatch(dbListTables(CON), error = function(e) character())

    users_expected_cols <- c(
      "user_id", "name", "username", "email", "password_hash", "role",
      "active", "force_password_change", "created_at", "updated_at", "last_login_at"
    )
    users_column_defs <- c(
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
    )

    if ("tokens" %in% existing_tables) {
      stop(
        "Legacy auth table 'tokens' was found. Startup will not modify auth tables automatically. ",
        "Take a database backup and run a manual auth migration before launching RIDS."
      )
    }

    if ("users" %in% existing_tables) {
      existing_users_cols <- dbListFields(CON, "users")
      extra_users_cols <- setdiff(existing_users_cols, users_expected_cols)
      if (length(extra_users_cols) > 0) {
        stop(
          "The users table has unsupported columns: ",
          paste(extra_users_cols, collapse = ", "),
          ". Startup will not modify auth tables automatically. ",
          "Take a database backup and run a manual auth migration before launching RIDS."
        )
      }
    }

    dbExecute(CON, "CREATE SEQUENCE IF NOT EXISTS user_id_seq;")
    dbExecute(CON, "CREATE SEQUENCE IF NOT EXISTS auth_session_id_seq;")
    dbExecute(CON, "CREATE SEQUENCE IF NOT EXISTS auth_audit_id_seq;")
    dbExecute(CON, "CREATE SEQUENCE IF NOT EXISTS user_api_credential_id_seq;")

    dbExecute(CON, "
      CREATE TABLE IF NOT EXISTS users (
        user_id                INTEGER PRIMARY KEY DEFAULT nextval('user_id_seq'),
        name                   TEXT,
        username               TEXT UNIQUE NOT NULL,
        email                  TEXT,
        password_hash          TEXT,
        role                   TEXT NOT NULL DEFAULT 'user',
        active                 BOOLEAN NOT NULL DEFAULT TRUE,
        force_password_change  BOOLEAN NOT NULL DEFAULT FALSE,
        created_at             TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        updated_at             TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        last_login_at          TIMESTAMP
      );
    ")

    dbExecute(CON, "
      CREATE TABLE IF NOT EXISTS auth_sessions (
        session_id    INTEGER PRIMARY KEY DEFAULT nextval('auth_session_id_seq'),
        user_id       INTEGER NOT NULL,
        token_hash    TEXT NOT NULL,
        expires_at    TIMESTAMP NOT NULL,
        created_at    TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        revoked_at    TIMESTAMP,
        user_agent    TEXT,
        FOREIGN KEY (user_id) REFERENCES users(user_id)
      );
    ")

    dbExecute(CON, "
      CREATE TABLE IF NOT EXISTS auth_audit_log (
        audit_id       INTEGER PRIMARY KEY DEFAULT nextval('auth_audit_id_seq'),
        timestamp      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        event_type     TEXT NOT NULL,
        user_id        INTEGER,
        actor_user_id  INTEGER,
        username       TEXT,
        success        BOOLEAN NOT NULL DEFAULT TRUE,
        message        TEXT,
        session_id     INTEGER
      );
    ")

    dbExecute(CON, "
      CREATE TABLE IF NOT EXISTS user_api_credentials (
        credential_id      INTEGER PRIMARY KEY DEFAULT nextval('user_api_credential_id_seq'),
        user_id            INTEGER NOT NULL,
        provider           TEXT NOT NULL,
        secret_ciphertext  TEXT NOT NULL,
        secret_nonce       TEXT NOT NULL,
        created_at         TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        updated_at         TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      );
    ")

    credential_expected_cols <- c(
      "credential_id", "user_id", "provider", "secret_ciphertext",
      "secret_nonce", "created_at", "updated_at"
    )
    credential_column_defs <- c(
      credential_id = "INTEGER",
      user_id = "INTEGER",
      provider = "TEXT",
      secret_ciphertext = "TEXT",
      secret_nonce = "TEXT",
      created_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
      updated_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    )

    credential_cols <- dbListFields(CON, "user_api_credentials")
    missing_credential_cols <- setdiff(credential_expected_cols, credential_cols)
    for (col in missing_credential_cols) {
      dbExecute(
        CON,
        paste("ALTER TABLE user_api_credentials ADD COLUMN", col, credential_column_defs[[col]], ";")
      )
    }

    recreate_credentials_without_fk <- FALSE
    credentials_create_sql <- tryCatch(
      DBI::dbGetQuery(
        CON,
        "SELECT sql FROM duckdb_tables() WHERE table_name = 'user_api_credentials' LIMIT 1"
      )$sql[[1]],
      error = function(e) NA_character_
    )

    if (!is.na(credentials_create_sql) &&
        grepl("FOREIGN KEY", credentials_create_sql, fixed = TRUE)) {
      recreate_credentials_without_fk <- TRUE
    }

    if (isTRUE(recreate_credentials_without_fk)) {
      dbExecute(CON, "DROP TABLE IF EXISTS user_api_credentials__migrate;")
      DBI::dbWithTransaction(CON, {
        dbExecute(CON, "
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
        dbExecute(CON, "
          INSERT INTO user_api_credentials__migrate
            (credential_id, user_id, provider, secret_ciphertext, secret_nonce, created_at, updated_at)
          SELECT
            credential_id, user_id, provider, secret_ciphertext, secret_nonce, created_at, updated_at
          FROM user_api_credentials;
        ")
        dbExecute(CON, "DROP TABLE user_api_credentials;")
        dbExecute(CON, "ALTER TABLE user_api_credentials__migrate RENAME TO user_api_credentials;")
      })
    }

    dbExecute(
      CON,
      paste(
        "CREATE UNIQUE INDEX IF NOT EXISTS idx_user_api_credentials_user_provider",
        "ON user_api_credentials (user_id, provider);"
      )
    )

    users_cols <- dbListFields(CON, "users")
    missing_users_cols <- setdiff(users_expected_cols, users_cols)
    for (col in missing_users_cols) {
      dbExecute(
        CON,
        paste("ALTER TABLE users ADD COLUMN", col, users_column_defs[[col]], ";")
      )
    }
  }, error = function(e) {
    stop("Failed to initialise user tables: ", e$message)
  })
}

## Populate user tables --------------------------------------------------------
seed_users <- function() {
}

## Rules tables ----------------------------------------------------------------
build_rules_tables <- function() {
  # 4) Helper to run SQL quickly
  exec_sql <- function(sql) dbExecute(CON, sql)
  upsert_rule_row <- function(table, key_col, key_val, sql, params) {
    exists <- dbGetQuery(
      CON,
      paste0("SELECT 1 FROM ", table, " WHERE ", key_col, " = ? LIMIT 1"),
      params = list(key_val)
    )
    if (nrow(exists) == 0) {
      dbExecute(CON, sql, params = params)
    }
  }

  # 5) Create tables (idempotent)
  # 5.1) Ruleset/version container
  exec_sql("
    CREATE TABLE IF NOT EXISTS rulesets (
      ruleset_id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      version TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      notes TEXT
    );
  ")
  
  # 5.2) Provider org list
  exec_sql("
    CREATE TABLE IF NOT EXISTS provider_orgs (
      provider_org TEXT PRIMARY KEY
    );
  ")
  
  # 5.3) Posting line types
  exec_sql("
    CREATE TABLE IF NOT EXISTS posting_line_types (
      posting_line_type_id TEXT PRIMARY KEY,
      label TEXT NOT NULL
    );
  ")
  
  # 5.4) Distribution rules
  exec_sql("
    CREATE TABLE IF NOT EXISTS dist_rules (
      dist_rule_id TEXT PRIMARY KEY,
      ruleset_id TEXT NOT NULL,
      scenario_id TEXT NOT NULL,
      row_category TEXT NOT NULL,
      condition_field TEXT,
      condition_op TEXT,
      condition_value TEXT,
      posting_line_type_id TEXT NOT NULL,
      priority INTEGER NOT NULL DEFAULT 100,
      notes TEXT,
      FOREIGN KEY (ruleset_id) REFERENCES rulesets(ruleset_id),
      FOREIGN KEY (posting_line_type_id) REFERENCES posting_line_types(posting_line_type_id)
    );
  ")
  
  # 5.5) Amount mapping
  exec_sql("
    CREATE TABLE IF NOT EXISTS amount_map (
      posting_line_type_id TEXT PRIMARY KEY,
      base_mult DOUBLE NOT NULL,
      split_mult DOUBLE NOT NULL,
      applies_to_row_category TEXT NOT NULL,
      calc_method TEXT NOT NULL DEFAULT 'STANDARD',
      notes TEXT,
      FOREIGN KEY (posting_line_type_id) REFERENCES posting_line_types(posting_line_type_id)
    );
  ")
  
  # 5.6) Routing rules
  exec_sql("
    CREATE TABLE IF NOT EXISTS routing_rules (
      routing_rule_id TEXT PRIMARY KEY,
      ruleset_id TEXT NOT NULL,
      scenario_id TEXT NOT NULL,
      condition_field TEXT,
      condition_op TEXT,
      condition_value TEXT,
      posting_line_type_id TEXT NOT NULL,
      destination_bucket TEXT NOT NULL,
      priority INTEGER NOT NULL DEFAULT 100,
      notes TEXT,
      FOREIGN KEY (ruleset_id) REFERENCES rulesets(ruleset_id),
      FOREIGN KEY (posting_line_type_id) REFERENCES posting_line_types(posting_line_type_id)
    );
  ")
  
  amount_cols <- dbListFields(CON, "amount_map")
  if (!"calc_method" %in% amount_cols) {
    exec_sql("ALTER TABLE amount_map ADD COLUMN calc_method TEXT DEFAULT 'STANDARD';")
  }
  exec_sql("UPDATE amount_map SET calc_method = 'STANDARD' WHERE calc_method IS NULL;")

  upsert_rule_row(
    "rulesets",
    "ruleset_id",
    "COMM_AH_V1",
    "
      INSERT INTO rulesets (ruleset_id, name, version, notes)
      VALUES (?, ?, ?, ?)
    ",
    list("COMM_AH_V1", "Commercial Rules A–H", "v1", "A–H scenarios; MFF fixed at runtime param for MVP")
  )
  
  for (org in c("RDUHT", "CRF", "DPT", "UoE")) {
    upsert_rule_row(
      "provider_orgs",
      "provider_org",
      org,
      "INSERT INTO provider_orgs (provider_org) VALUES (?)",
      list(org)
    )
  }
  
  posting_line_type_rows <- list(
    list("DIRECT", "Direct Cost"),
    list("DIRECT_40_PI", "Direct Cost 40% (PI)"),
    list("DIRECT_60_TEAM", "Direct Cost 60% (Delivery/Team)"),
    list("CAPACITY_RD", "Capacity (R&D)"),
    list("INDIRECT_50_DELIVERY", "Indirect 50% (Delivery/Support)"),
    list("INDIRECT_25_TRUST", "Indirect 25% (Trust Overhead)"),
    list("INDIRECT_25_PI", "Indirect 25% (PI)"),
    list("MFF_SPLIT_NEW_CC", "MFF Split to New Cost Centre")
  )
  
  for (row in posting_line_type_rows) {
    upsert_rule_row(
      "posting_line_types",
      "posting_line_type_id",
      row[[1]],
      "INSERT INTO posting_line_types (posting_line_type_id, label) VALUES (?, ?)",
      row
    )
  }
  
  amount_map_rows <- list(
    list("DIRECT", 1.0, 1.0, "BOTH", "STANDARD", "AC * mff"),
    list("CAPACITY_RD", 0.2, 1.0, "BOTH", "STANDARD", "AC * 0.2 * mff"),
    list("INDIRECT_50_DELIVERY", 0.7, 0.5, "BASELINE", "STANDARD", "AC * 0.7 * mff * 0.5"),
    list("INDIRECT_25_TRUST", 0.7, 0.25, "BASELINE", "STANDARD", "AC * 0.7 * mff * 0.25"),
    list("INDIRECT_25_PI", 0.7, 0.25, "BASELINE", "STANDARD", "AC * 0.7 * mff * 0.25"),
    list("DIRECT_40_PI", 1.0, 0.4, "BASELINE", "STANDARD", "AC * mff * 0.4 (TRD medic split)"),
    list("DIRECT_60_TEAM", 1.0, 0.6, "BASELINE", "STANDARD", "AC * mff * 0.6 (TRD medic split)"),
    list("MFF_SPLIT_NEW_CC", 0.0, 0.0, "BOTH", "MFF_SPLIT_ONLY", "Calculated from total pre-MFF base x uplift x split pct")
  )
  
  for (row in amount_map_rows) {
    upsert_rule_row(
      "amount_map",
      "posting_line_type_id",
      row[[1]],
      "
        INSERT INTO amount_map
          (posting_line_type_id, base_mult, split_mult, applies_to_row_category, calc_method, notes)
        VALUES (?, ?, ?, ?, ?, ?)
      ",
      row
    )
  }
  
  # 9) Seed dist_rules
  insert_dist_rule <- function(id, scenario, row_category, posting_line, priority,
                               condition_field = NA, condition_op = NA, condition_value = NA, notes = NA) {
    upsert_rule_row(
      "dist_rules",
      "dist_rule_id",
      id,
      "
        INSERT INTO dist_rules
          (dist_rule_id, ruleset_id, scenario_id, row_category,
           condition_field, condition_op, condition_value,
           posting_line_type_id, priority, notes)
        VALUES (?, 'COMM_AH_V1', ?, ?, ?, ?, ?, ?, ?, ?)
      ",
      list(
        id, scenario, row_category,
        condition_field, condition_op, condition_value,
        posting_line, priority, notes
      )
    )
  }
  
  # 9.1) Rule Vectors
  baseline_std <- c("DIRECT", "CAPACITY_RD", "INDIRECT_50_DELIVERY", "INDIRECT_25_TRUST", "INDIRECT_25_PI")
  invest_std <- c("DIRECT", "CAPACITY_RD")
  training_std <- baseline_std
  setup_close_departmental_std <- c("DIRECT")
  baseline_std_mff <- c(baseline_std, "MFF_SPLIT_NEW_CC")
  invest_std_mff <- c(invest_std, "MFF_SPLIT_NEW_CC")
  training_std_mff <- c(training_std, "MFF_SPLIT_NEW_CC")
  setup_close_departmental_std_mff <- c(setup_close_departmental_std, "MFF_SPLIT_NEW_CC")
  
  # 9.2) Scenarios like A
  like_A <- c("A", "C", "E", "G", "H")
  for (sc in like_A) {
    pr <- 10
    for (pl in baseline_std_mff) {
      insert_dist_rule(paste0(sc, "_BASE_", pl), sc, "BASELINE", pl, pr)
      pr <- pr + 10
    }
    pr <- 10
    for (pl in invest_std_mff) {
      insert_dist_rule(paste0(sc, "_INV_", pl), sc, "INVESTIGATION", pl, pr)
      pr <- pr + 10
    }
    pr <- 10
    for (pl in training_std_mff) {
      insert_dist_rule(paste0(sc, "_TRAIN_", pl), sc, "TRAINING_FEE", pl, pr)
      pr <- pr + 10
    }
    pr <- 10
    for (pl in setup_close_departmental_std_mff) {
      insert_dist_rule(
        paste0(sc, "_SETUPCLOSE_DEPT_", pl),
        sc,
        "SETUP_CLOSE_DEPARTMENTAL",
        pl,
        pr,
        notes = "Setup & Closedown departmental costs: direct only"
      )
      pr <- pr + 10
    }
  }
  
  # 9.3) TRD scenarios: B, D, F
  trd_scenarios <- c("B", "D", "F")
  
  for (sc in trd_scenarios) {
    
    pr <- 20
    for (pl in baseline_std_mff) {
      insert_dist_rule(paste0(sc, "_BASE_NONMED_", pl), sc, "BASELINE", pl, pr,
                       condition_field = "is_medic", condition_op = "=", condition_value = "FALSE",
                       notes = "TRD scenario: non-medic baseline uses standard direct")
      pr <- pr + 10
    }
    
    pr_train <- 10
    for (pl in training_std_mff) {
      insert_dist_rule(paste0(sc, "_TRAIN_", pl), sc, "TRAINING_FEE", pl, pr_train)
      pr_train <- pr_train + 10
    }
    
    # Setup & Closedown Departmental costs: direct only
    pr_setup <- 10
    for (pl in setup_close_departmental_std_mff) {
      insert_dist_rule(
        paste0(sc, "_SETUPCLOSE_DEPT_", pl),
        sc,
        "SETUP_CLOSE_DEPARTMENTAL",
        pl,
        pr_setup,
        notes = "Setup & Closedown departmental costs: direct only"
      )
      pr_setup <- pr_setup + 10
    }
    
    insert_dist_rule(paste0(sc, "_BASE_MED_DIRECT40"), sc, "BASELINE", "DIRECT_40_PI", 5,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE",
                     notes = "TRD medic: 40% direct to PI")
    insert_dist_rule(paste0(sc, "_BASE_MED_DIRECT60"), sc, "BASELINE", "DIRECT_60_TEAM", 6,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE",
                     notes = "TRD medic: 60% direct to team")
    insert_dist_rule(paste0(sc, "_BASE_MED_CAP"), sc, "BASELINE", "CAPACITY_RD", 20,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE")
    insert_dist_rule(paste0(sc, "_BASE_MED_I50"), sc, "BASELINE", "INDIRECT_50_DELIVERY", 30,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE")
    insert_dist_rule(paste0(sc, "_BASE_MED_I25T"), sc, "BASELINE", "INDIRECT_25_TRUST", 40,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE")
    insert_dist_rule(paste0(sc, "_BASE_MED_I25P"), sc, "BASELINE", "INDIRECT_25_PI", 50,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE")
    insert_dist_rule(paste0(sc, "_BASE_MED_MFF"), sc, "BASELINE", "MFF_SPLIT_NEW_CC", 60,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE")
    
    insert_dist_rule(paste0(sc, "_INV_DIRECT"), sc, "INVESTIGATION", "DIRECT", 10)
    insert_dist_rule(paste0(sc, "_INV_CAP"), sc, "INVESTIGATION", "CAPACITY_RD", 20)
    insert_dist_rule(paste0(sc, "_INV_MFF"), sc, "INVESTIGATION", "MFF_SPLIT_NEW_CC", 30)
  }
  
  # 10) Seed routing_rules
  insert_routing <- function(id, scenario, posting_line, dest_bucket, priority,
                             condition_field = NA, condition_op = NA, condition_value = NA, notes = NA) {
    upsert_rule_row(
      "routing_rules",
      "routing_rule_id",
      id,
      "
        INSERT INTO routing_rules
          (routing_rule_id, ruleset_id, scenario_id,
           condition_field, condition_op, condition_value,
           posting_line_type_id, destination_bucket, priority, notes)
        VALUES (?, 'COMM_AH_V1', ?, ?, ?, ?, ?, ?, ?, ?)
      ",
      list(
        id, scenario,
        condition_field, condition_op, condition_value,
        posting_line, dest_bucket, priority, notes
      )
    )
  }
  
  # 10.1) Internal Routing
  internal_like <- c("A", "B", "C", "D", "E", "F")
  for (sc in internal_like) {
    insert_routing(paste0(sc, "_R_DIRECT"), sc, "DIRECT", "DEST_PROVIDER", 10)
    insert_routing(paste0(sc, "_R_D40"), sc, "DIRECT_40_PI", "DEST_PI_ORG", 10)
    insert_routing(paste0(sc, "_R_D60"), sc, "DIRECT_60_TEAM", "DEST_SUPPORT", 10)
    insert_routing(paste0(sc, "_R_CAP"), sc, "CAPACITY_RD", "DEST_RD", 10)
    insert_routing(paste0(sc, "_R_I50"), sc, "INDIRECT_50_DELIVERY", "DEST_SUPPORT", 10, notes = "50% indirect to support")
    insert_routing(paste0(sc, "_R_I25T"), sc, "INDIRECT_25_TRUST", "DEST_TRUST_OH", 10)
    insert_routing(paste0(sc, "_R_I25P"), sc, "INDIRECT_25_PI", "DEST_PI_ORG", 10)
    insert_routing(paste0(sc, "_R_MFF"), sc, "MFF_SPLIT_NEW_CC", "DEST_MFF_SPLIT", 10)
  }
  
  # 10.2) External Routing
  external_like <- c("G", "H")
  for (sc in external_like) {
    insert_routing(paste0(sc, "_R_DIRECT"), sc, "DIRECT", "DEST_PROVIDER", 10)
    insert_routing(paste0(sc, "_R_CAP"), sc, "CAPACITY_RD", "DEST_RD", 10)
    insert_routing(paste0(sc, "_R_I50"), sc, "INDIRECT_50_DELIVERY", "DEST_PROVIDER", 10)
    insert_routing(paste0(sc, "_R_I25T"), sc, "INDIRECT_25_TRUST", "DEST_PROVIDER", 10)
    insert_routing(paste0(sc, "_R_I25P"), sc, "INDIRECT_25_PI", "DEST_PROVIDER", 10)
    insert_routing(paste0(sc, "_R_MFF"), sc, "MFF_SPLIT_NEW_CC", "DEST_MFF_SPLIT", 10)
  }
  
}

bootstrap_admin <- function() {
}

# Admin settings tables --------------------------------------------------------
settings_table <- function() {
  dbExecute(CON, "
    CREATE TABLE IF NOT EXISTS app_settings (
      key   TEXT PRIMARY KEY,
      value TEXT NOT NULL
    );
  ")
  
  # Seed defaults if empty
  count <- dbGetQuery(CON, "SELECT COUNT(*) AS n FROM app_settings")$n
  if (count == 0) {
    dbExecute(CON,
              "INSERT INTO app_settings (key, value) VALUES (?, ?)",
              params = list("ict_upload_dir", ICT_UPLOAD_DIR)
    )
    dbExecute(CON,
              "INSERT INTO app_settings (key, value) VALUES (?, ?)",
              params = list("edge_output_dir", EDGE_OUTPUT_DIR)
    )
  }

  existing_keys <- tryCatch(
    dbGetQuery(CON, "SELECT key FROM app_settings")$key,
    error = function(e) character()
  )

  if (!"log_retention_days" %in% existing_keys) {
    dbExecute(
      CON,
      "INSERT INTO app_settings (key, value) VALUES (?, ?)",
      params = list("log_retention_days", "90")
    )
  }

  if (!"cost_centre_matrix_file" %in% existing_keys) {
    dbExecute(
      CON,
      "INSERT INTO app_settings (key, value) VALUES (?, ?)",
      params = list("cost_centre_matrix_file", "")
    )
  }

}

app_logs_table <- function() {
  storage_mode <- "duckdb"

  if (exists("APP_CONFIG", inherits = TRUE) &&
      !is.null(APP_CONFIG$storage_mode) &&
      nzchar(APP_CONFIG$storage_mode)) {
    storage_mode <- tolower(APP_CONFIG$storage_mode)
  }

  if (identical(storage_mode, "sqlserver")) {
    dbExecute(CON, "
      IF OBJECT_ID('dbo.app_logs', 'U') IS NULL
      BEGIN
        CREATE TABLE dbo.app_logs (
          log_id BIGINT IDENTITY(1,1) PRIMARY KEY,
          timestamp DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
          level NVARCHAR(16) NOT NULL,
          area NVARCHAR(100) NOT NULL,
          message NVARCHAR(4000) NOT NULL,
          user_id INT NULL,
          username NVARCHAR(255) NULL,
          session_id NVARCHAR(255) NULL,
          cpms_id NVARCHAR(255) NULL,
          upload_id NVARCHAR(255) NULL,
          details_json NVARCHAR(MAX) NULL,
          app_version NVARCHAR(64) NULL
        );
      END
    ")

    dbExecute(CON, "
      IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'ix_app_logs_timestamp' AND object_id = OBJECT_ID('dbo.app_logs'))
      CREATE INDEX ix_app_logs_timestamp ON dbo.app_logs (timestamp DESC);
    ")
    dbExecute(CON, "
      IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'ix_app_logs_level_timestamp' AND object_id = OBJECT_ID('dbo.app_logs'))
      CREATE INDEX ix_app_logs_level_timestamp ON dbo.app_logs (level, timestamp DESC);
    ")
    dbExecute(CON, "
      IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'ix_app_logs_area_timestamp' AND object_id = OBJECT_ID('dbo.app_logs'))
      CREATE INDEX ix_app_logs_area_timestamp ON dbo.app_logs (area, timestamp DESC);
    ")
    dbExecute(CON, "
      IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'ix_app_logs_cpms_upload' AND object_id = OBJECT_ID('dbo.app_logs'))
      CREATE INDEX ix_app_logs_cpms_upload ON dbo.app_logs (cpms_id, upload_id);
    ")
  } else {
    dbExecute(CON, "CREATE SEQUENCE IF NOT EXISTS app_log_id_seq;")
    dbExecute(CON, "
      CREATE TABLE IF NOT EXISTS app_logs (
        log_id       INTEGER PRIMARY KEY DEFAULT nextval('app_log_id_seq'),
        timestamp    TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        level        TEXT NOT NULL,
        area         TEXT NOT NULL,
        message      TEXT NOT NULL,
        user_id      INTEGER,
        username     TEXT,
        session_id   TEXT,
        cpms_id      TEXT,
        upload_id    TEXT,
        details_json TEXT,
        app_version  TEXT
      );
    ")
    dbExecute(CON, "CREATE INDEX IF NOT EXISTS idx_app_logs_timestamp ON app_logs (timestamp);")
    dbExecute(CON, "CREATE INDEX IF NOT EXISTS idx_app_logs_level_timestamp ON app_logs (level, timestamp);")
    dbExecute(CON, "CREATE INDEX IF NOT EXISTS idx_app_logs_area_timestamp ON app_logs (area, timestamp);")
    dbExecute(CON, "CREATE INDEX IF NOT EXISTS idx_app_logs_cpms_upload ON app_logs (cpms_id, upload_id);")
  }
}

# Specialities lookup ----------------------------------------------------------
specialities_table <- function() {
  dbExecute(CON, "
    CREATE SEQUENCE IF NOT EXISTS specialities_id_seq START 1;
  ")
  
  dbExecute(CON, "
    CREATE TABLE IF NOT EXISTS specialities (
      id           INTEGER PRIMARY KEY DEFAULT nextval('specialities_id_seq'),
      name         TEXT NOT NULL UNIQUE,
      archived_at  TIMESTAMP,
      created_at   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
  ")
  
  count <- dbGetQuery(CON, "SELECT COUNT(*) AS n FROM specialities")$n
  if (count == 0) {
    seed <- c(
      "Cancer", "Cardiology", "Dendron", "Dermatology", "ED",
      "Gastro", "Geriatric", "Orthopedics & Rheumatology",
      "Paediatric", "Renal", "Respiratory", "Stroke", "Urology"
    )
    
    for (nm in seed) {
      dbExecute(CON,
                "INSERT INTO specialities (name) VALUES (?) ON CONFLICT (name) DO NOTHING",
                params = list(nm)
      )
    }
  }
}

# Core costing table -----------------------------------------------------------
posting_lines_table <- function() {
  dbExecute(CON, "
    CREATE TABLE IF NOT EXISTS posting_lines (
      row_id               INTEGER,
      scenario_id          VARCHAR,
      row_category_auto    VARCHAR,
      calc_tag             VARCHAR,
      row_category         VARCHAR,
      is_medic             BOOLEAN,
      cpms_id              VARCHAR,
      study_site           VARCHAR,
      study_name           VARCHAR,
      Study_Arm            VARCHAR,
      Activity             VARCHAR,
      Visit                VARCHAR,
      posting_line_type_id VARCHAR,
      posting_amount       DOUBLE,
      destination_bucket   VARCHAR,
      destination_entity   VARCHAR,
      cost_code            VARCHAR,
      sheet_name           VARCHAR,
      Visit_Label          VARCHAR,
      staff_group          INTEGER,
      contract_cost        DOUBLE,
      Department           VARCHAR,
      Staff_Role           VARCHAR,
      activity_type        VARCHAR,
      time_required        VARCHAR,
      contract_price       DOUBLE,
      base_sum             DOUBLE,
      multiplier           DOUBLE,
      adjusted_amount      DOUBLE,
      residual             DOUBLE,
      is_residual_row      BOOLEAN,
      adjusted_sum_check   DOUBLE,
      diff_check           DOUBLE,
      edge_key             VARCHAR
    );
  ")

  posting_cols <- dbListFields(CON, "posting_lines")
  if (!"study_site" %in% posting_cols) {
    dbExecute(CON, "ALTER TABLE posting_lines ADD COLUMN study_site VARCHAR;")
  }
  if (!"activity_type" %in% posting_cols) {
    dbExecute(CON, "ALTER TABLE posting_lines ADD COLUMN activity_type VARCHAR;")
  }
  if (!"time_required" %in% posting_cols) {
    dbExecute(CON, "ALTER TABLE posting_lines ADD COLUMN time_required VARCHAR;")
  }
}

## Main Entry Point ------------------------------------------------------------
db_main <- function() {
  ict_table()
  meta_table()
  init_db()
  user_tables()
  build_rules_tables()
  settings_table()
  app_logs_table()
  specialities_table()
  posting_lines_table()
  ca_init_table()
  ca_init_ref_activities()
}
