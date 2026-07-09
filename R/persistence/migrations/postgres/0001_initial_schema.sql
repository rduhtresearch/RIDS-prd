-- PostgreSQL initial schema: the complete current RIDS schema (including
-- MFA tables), equivalent to the DuckDB migrations 0001+0005. The DuckDB
-- legacy fixups (0002-0004) and the developer-role collapse (0006) concern
-- pre-existing DuckDB databases only; a fresh PostgreSQL database starts
-- at the current shape. PostgreSQL folds unquoted identifiers to lowercase;
-- the application restores canonical mixed-case column names at the
-- repository boundary (R/persistence/db_helpers.R).

-- Initial schema: faithful transcription of the DDL previously embedded in
-- R/setup.r, R/addons/custom_activities/ca_schema.R, and ca_ref_activities.R.
-- All statements are idempotent so a pre-migration-era database adopts
-- versioning without modification. Column shapes are unchanged.

-- Sequences ------------------------------------------------------------------
CREATE SEQUENCE IF NOT EXISTS upload_id_seq;
CREATE SEQUENCE IF NOT EXISTS user_id_seq;
CREATE SEQUENCE IF NOT EXISTS auth_session_id_seq;
CREATE SEQUENCE IF NOT EXISTS auth_audit_id_seq;
CREATE SEQUENCE IF NOT EXISTS user_api_credential_id_seq;
CREATE SEQUENCE IF NOT EXISTS app_log_id_seq;
CREATE SEQUENCE IF NOT EXISTS specialities_id_seq START 1;
CREATE SEQUENCE IF NOT EXISTS addon_ca_row_seq START 1;
CREATE SEQUENCE IF NOT EXISTS ref_custom_activities_id_seq START 1;

-- ICT costing ------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS ict_costing_tbl (
  CPMS_ID                VARCHAR,
  study_site             VARCHAR,
  scenario_id            VARCHAR,
  Study                  VARCHAR,
  Visit_Number           VARCHAR,
  Study_Arm              VARCHAR,
  Visit_Label            VARCHAR,
  Activity_Name          VARCHAR,
  ICT_Cost               DOUBLE PRECISION,
  Contract_Cost          DOUBLE PRECISION,
  activity_occurrence_id INTEGER,
  staff_group            INTEGER
);

-- Study upload metadata ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS meta_data (
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
  mff_split_pct     DOUBLE PRECISION DEFAULT 0
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_meta_data_unique_study_identity
  ON meta_data (cpms_id, study_site, scenario_id);

-- Auth ---------------------------------------------------------------------------
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

-- Deliberately no FK to users: see 0003 (legacy FK removal) for history.
CREATE TABLE IF NOT EXISTS user_api_credentials (
  credential_id      INTEGER PRIMARY KEY DEFAULT nextval('user_api_credential_id_seq'),
  user_id            INTEGER NOT NULL,
  provider           TEXT NOT NULL,
  secret_ciphertext  TEXT NOT NULL,
  secret_nonce       TEXT NOT NULL,
  created_at         TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at         TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_user_api_credentials_user_provider
  ON user_api_credentials (user_id, provider);

-- Finance rules -------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS rulesets (
  ruleset_id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  version TEXT NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  notes TEXT
);

CREATE TABLE IF NOT EXISTS provider_orgs (
  provider_org TEXT PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS posting_line_types (
  posting_line_type_id TEXT PRIMARY KEY,
  label TEXT NOT NULL
);

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

CREATE TABLE IF NOT EXISTS amount_map (
  posting_line_type_id TEXT PRIMARY KEY,
  base_mult DOUBLE PRECISION NOT NULL,
  split_mult DOUBLE PRECISION NOT NULL,
  applies_to_row_category TEXT NOT NULL,
  calc_method TEXT NOT NULL DEFAULT 'STANDARD',
  notes TEXT,
  FOREIGN KEY (posting_line_type_id) REFERENCES posting_line_types(posting_line_type_id)
);

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

-- App settings ----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS app_settings (
  key   TEXT PRIMARY KEY,
  value TEXT NOT NULL
);

-- App logs --------------------------------------------------------------------------
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

CREATE INDEX IF NOT EXISTS idx_app_logs_timestamp ON app_logs (timestamp);
CREATE INDEX IF NOT EXISTS idx_app_logs_level_timestamp ON app_logs (level, timestamp);
CREATE INDEX IF NOT EXISTS idx_app_logs_area_timestamp ON app_logs (area, timestamp);
CREATE INDEX IF NOT EXISTS idx_app_logs_cpms_upload ON app_logs (cpms_id, upload_id);

-- Specialities lookup ------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS specialities (
  id           INTEGER PRIMARY KEY DEFAULT nextval('specialities_id_seq'),
  name         TEXT NOT NULL UNIQUE,
  archived_at  TIMESTAMP,
  created_at   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Posting lines --------------------------------------------------------------------------
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
  posting_amount       DOUBLE PRECISION,
  destination_bucket   VARCHAR,
  destination_entity   VARCHAR,
  cost_code            VARCHAR,
  sheet_name           VARCHAR,
  Visit_Label          VARCHAR,
  staff_group          INTEGER,
  contract_cost        DOUBLE PRECISION,
  Department           VARCHAR,
  Staff_Role           VARCHAR,
  activity_type        VARCHAR,
  time_required        VARCHAR,
  contract_price       DOUBLE PRECISION,
  base_sum             DOUBLE PRECISION,
  multiplier           DOUBLE PRECISION,
  adjusted_amount      DOUBLE PRECISION,
  residual             DOUBLE PRECISION,
  is_residual_row      BOOLEAN,
  adjusted_sum_check   DOUBLE PRECISION,
  diff_check           DOUBLE PRECISION,
  edge_key             VARCHAR
);

-- Custom activities addon --------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS addon_custom_activities (
  id                  INTEGER PRIMARY KEY DEFAULT nextval('addon_ca_row_seq'),
  custom_activity_id  VARCHAR NOT NULL,
  cpms_id             VARCHAR NOT NULL,
  study_site          VARCHAR,
  study_name          VARCHAR,
  scenario_id         VARCHAR,
  Study_Arm           VARCHAR NOT NULL,
  Activity            VARCHAR NOT NULL,
  mode                VARCHAR NOT NULL,
  slot_num            INTEGER NOT NULL,
  cost_centre         VARCHAR NOT NULL,
  amount              DOUBLE PRECISION NOT NULL,
  created_by          INTEGER,
  created_at          TIMESTAMP DEFAULT current_timestamp
);

CREATE INDEX IF NOT EXISTS idx_addon_ca_cpms
  ON addon_custom_activities (cpms_id, study_site, scenario_id, custom_activity_id);

CREATE TABLE IF NOT EXISTS ref_custom_activities (
  id           INTEGER PRIMARY KEY DEFAULT nextval('ref_custom_activities_id_seq'),
  name         TEXT NOT NULL UNIQUE,
  archived_at  TIMESTAMP,
  created_at   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Multi-factor authentication tables.
-- mfa_factors: one TOTP factor per user; the shared secret is stored
--   encrypted with the app credential key (same scheme as
--   user_api_credentials). last_used_step prevents TOTP replay within a
--   time window.
-- mfa_recovery_codes: hashed one-time recovery codes issued at enrollment.

CREATE SEQUENCE IF NOT EXISTS mfa_factor_id_seq;
CREATE SEQUENCE IF NOT EXISTS mfa_recovery_code_id_seq;

CREATE TABLE IF NOT EXISTS mfa_factors (
  factor_id          INTEGER PRIMARY KEY DEFAULT nextval('mfa_factor_id_seq'),
  user_id            INTEGER NOT NULL,
  method             TEXT NOT NULL DEFAULT 'totp',
  secret_ciphertext  TEXT NOT NULL,
  secret_nonce       TEXT NOT NULL,
  verified_at        TIMESTAMP,
  last_used_step     BIGINT,
  created_at         TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_mfa_factors_user_method
  ON mfa_factors (user_id, method);

CREATE TABLE IF NOT EXISTS mfa_recovery_codes (
  code_id     INTEGER PRIMARY KEY DEFAULT nextval('mfa_recovery_code_id_seq'),
  user_id     INTEGER NOT NULL,
  code_hash   TEXT NOT NULL,
  used_at     TIMESTAMP,
  created_at  TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_mfa_recovery_codes_user
  ON mfa_recovery_codes (user_id);
