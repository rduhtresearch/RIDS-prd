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
