# ==============================================================================
# SETUP & AUTHENTICATION
#
# Schema DDL lives in versioned migrations under R/persistence/migrations/
# (see R/persistence/migrate.R). This file keeps:
#   - startup guards that must run on every boot (legacy auth schema checks)
#   - idempotent seed data (finance rules, settings defaults, specialities)
#   - db_main(), the boot entry point called from global.R
#
# The per-table functions (ict_table, meta_table, user_tables, ...) are kept
# as entry points for existing callers (R/SETUP/new_setup.R, tests); each
# ensures the schema via migrations and applies its seeds.
# ==============================================================================
source("R/utils/auth.r", local = FALSE)
source("R/persistence/migrate.R", local = FALSE)
source("R/addons/custom_activities/ca_schema.R", local = FALSE)
source("R/addons/custom_activities/ca_ref_activities.R", local = FALSE)

## Init database ---------------------------------------------------------------
init_db <- function() {
  tryCatch({
    dbGetQuery(CON, "SELECT 1")
  }, error = function(e) {
    stop("DB ERROR: ", e$message)
  })
}

## Schema (versioned migrations) ------------------------------------------------
rids_ensure_schema <- function(con = CON) {
  run_migrations(con)
}

## Startup guards ----------------------------------------------------------------
# These intentionally BLOCK startup on databases whose auth schema this code
# does not understand; they are checks, not migrations, and run every boot.
check_legacy_auth_schema <- function(con = CON) {
  existing_tables <- tryCatch(dbListTables(con), error = function(e) character())

  if ("tokens" %in% existing_tables) {
    stop(
      "Legacy auth table 'tokens' was found. Startup will not modify auth tables automatically. ",
      "Take a database backup and run a manual auth migration before launching RIDS."
    )
  }

  users_expected_cols <- c(
    "user_id", "name", "username", "email", "password_hash", "role",
    "active", "force_password_change", "created_at", "updated_at", "last_login_at"
  )

  if ("users" %in% existing_tables) {
    existing_users_cols <- dbListFields(con, "users")
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

  invisible(TRUE)
}

## Per-table entry points (schema via migrations, plus seeds) -------------------
ict_table <- function() {
  rids_ensure_schema()
}

meta_table <- function() {
  rids_ensure_schema()
}

user_tables <- function() {
  tryCatch({
    check_legacy_auth_schema()
    rids_ensure_schema()
  }, error = function(e) {
    stop("Failed to initialise user tables: ", e$message)
  })
}

app_logs_table <- function() {
  rids_ensure_schema()
}

posting_lines_table <- function() {
  rids_ensure_schema()
}

## Rules seed data ---------------------------------------------------------------
build_rules_tables <- function() {
  rids_ensure_schema()

  upsert_rule_row <- function(table, key_col, key_val, sql, params) {
    exists <- rids_dbGetQuery(
      CON,
      paste0("SELECT 1 FROM ", table, " WHERE ", key_col, " = ? LIMIT 1"),
      params = list(key_val)
    )
    if (nrow(exists) == 0) {
      rids_dbExecute(CON, sql, params = params)
    }
  }

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

  # Seed dist_rules
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

  # Rule Vectors
  baseline_std <- c("DIRECT", "CAPACITY_RD", "INDIRECT_50_DELIVERY", "INDIRECT_25_TRUST", "INDIRECT_25_PI")
  invest_std <- c("DIRECT", "CAPACITY_RD")
  training_std <- baseline_std
  setup_close_departmental_std <- c("DIRECT")
  baseline_std_mff <- c(baseline_std, "MFF_SPLIT_NEW_CC")
  invest_std_mff <- c(invest_std, "MFF_SPLIT_NEW_CC")
  training_std_mff <- c(training_std, "MFF_SPLIT_NEW_CC")
  setup_close_departmental_std_mff <- c(setup_close_departmental_std, "MFF_SPLIT_NEW_CC")

  # Scenarios like A
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

  # TRD scenarios: B, D, F
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

  # Seed routing_rules
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

  # Internal Routing
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

  # External Routing
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

# Admin settings seed ------------------------------------------------------------
settings_table <- function() {
  rids_ensure_schema()

  # Seed defaults if empty
  count <- rids_dbGetQuery(CON, "SELECT COUNT(*) AS n FROM app_settings")$n
  if (count == 0) {
    rids_dbExecute(CON,
              "INSERT INTO app_settings (key, value) VALUES (?, ?)",
              params = list("ict_upload_dir", ICT_UPLOAD_DIR)
    )
    rids_dbExecute(CON,
              "INSERT INTO app_settings (key, value) VALUES (?, ?)",
              params = list("edge_output_dir", EDGE_OUTPUT_DIR)
    )
  }

  existing_keys <- tryCatch(
    rids_dbGetQuery(CON, "SELECT key FROM app_settings")$key,
    error = function(e) character()
  )

  if (!"log_retention_days" %in% existing_keys) {
    rids_dbExecute(
      CON,
      "INSERT INTO app_settings (key, value) VALUES (?, ?)",
      params = list("log_retention_days", "90")
    )
  }

  if (!"cost_centre_matrix_file" %in% existing_keys) {
    rids_dbExecute(
      CON,
      "INSERT INTO app_settings (key, value) VALUES (?, ?)",
      params = list("cost_centre_matrix_file", "")
    )
  }

}

# Specialities seed ---------------------------------------------------------------
specialities_table <- function() {
  rids_ensure_schema()

  count <- rids_dbGetQuery(CON, "SELECT COUNT(*) AS n FROM specialities")$n
  if (count == 0) {
    seed <- c(
      "Cancer", "Cardiology", "Dendron", "Dermatology", "ED",
      "Gastro", "Geriatric", "Orthopedics & Rheumatology",
      "Paediatric", "Renal", "Respiratory", "Stroke", "Urology"
    )

    for (nm in seed) {
      rids_dbExecute(CON,
                "INSERT INTO specialities (name) VALUES (?) ON CONFLICT (name) DO NOTHING",
                params = list(nm)
      )
    }
  }
}

## Main Entry Point ------------------------------------------------------------
db_main <- function() {
  init_db()
  user_tables()          # startup guards + schema via migrations
  build_rules_tables()   # rules seed data
  settings_table()       # settings defaults
  specialities_table()   # specialities seed
  ca_init_table()        # custom-activities addon schema entry point
  ca_init_ref_activities() # custom-activities reference data seed/top-up
}
