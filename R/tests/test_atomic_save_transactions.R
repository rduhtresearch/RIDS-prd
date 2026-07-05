suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(tibble)
})

.atomic_passed <- 0L
.atomic_failed <- 0L

.atomic_expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .atomic_passed <<- .atomic_passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .atomic_failed <<- .atomic_failed + 1L
  }
}

.with_atomic_db <- function(fn) {
  db_path <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

  old_con <- if (exists("CON", inherits = TRUE)) get("CON", inherits = TRUE) else NULL
  assign("CON", con, envir = .GlobalEnv)

  on.exit({
    if (is.null(old_con)) {
      rm("CON", envir = .GlobalEnv)
    } else {
      assign("CON", old_con, envir = .GlobalEnv)
    }

    suppressWarnings(try(dbDisconnect(con, shutdown = TRUE), silent = TRUE))
    if (file.exists(db_path)) {
      unlink(db_path, force = TRUE)
    }
  }, add = TRUE)

  fn(con, db_path)
}

run_atomic_save_transaction_tests <- function() {
  cat("\n=== atomic save transaction tests ===\n\n")
  .atomic_passed <<- 0L
  .atomic_failed <<- 0L

  source("R/utils/pipeline_fixed.r")

  .with_atomic_db(function(con, db_path) {
    dbExecute(con, "
      CREATE TABLE ict_costing_tbl (
        CPMS_ID VARCHAR,
        study_site VARCHAR,
        scenario_id VARCHAR,
        Study VARCHAR,
        Visit_Number VARCHAR,
        Study_Arm VARCHAR,
        Visit_Label VARCHAR,
        Activity_Name VARCHAR,
        ICT_Cost DOUBLE CHECK (ICT_Cost >= 0),
        Contract_Cost DOUBLE,
        activity_occurrence_id VARCHAR,
        staff_group INTEGER
      )
    ")

    dbExecute(con, "
      INSERT INTO ict_costing_tbl (
        CPMS_ID, study_site, scenario_id, Study, Visit_Number, Study_Arm,
        Visit_Label, Activity_Name, ICT_Cost, Contract_Cost, activity_occurrence_id, staff_group
      ) VALUES ('CP1', 'RDUHT', 'A', 'Study A', 'VISIT - 001', 'Arm A',
                'Screening', 'Blood Test', 123.45, NULL, 'AO1', 1)
    ")

    save_error <- tryCatch({
      DBI::dbWithTransaction(con, {
        dbExecute(
          con,
          paste(
            "DELETE FROM ict_costing_tbl",
            "WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?"
          ),
          params = list("CP1", "RDUHT", "A")
        )
        dbAppendTable(con, "ict_costing_tbl", tibble(
          CPMS_ID = "CP1",
          study_site = "RDUHT",
          scenario_id = "A",
          Study = "Study A",
          Visit_Number = "VISIT - 001",
          Study_Arm = "Arm A",
          Visit_Label = "Screening",
          Activity_Name = "Blood Test",
          ICT_Cost = -1,
          Contract_Cost = NA_real_,
          activity_occurrence_id = "AO2",
          staff_group = 1L
        ))
      })
      NULL
    }, error = function(e) e)

    survivors <- dbGetQuery(
      con,
      paste(
        "SELECT CPMS_ID, study_site, scenario_id, ICT_Cost",
        "FROM ict_costing_tbl",
        "WHERE CPMS_ID = 'CP1' AND study_site = 'RDUHT' AND scenario_id = 'A'"
      )
    )

    .atomic_expect("step2-style save fails when replacement rows violate checks", inherits(save_error, "error"))
    .atomic_expect("step2-style save keeps prior ict_costing rows on failure", nrow(survivors) == 1L)
    .atomic_expect("step2-style save preserves prior ict_costing values", identical(survivors$ICT_Cost[[1]], 123.45))
  })

  .with_atomic_db(function(con, db_path) {
    dbExecute(con, "
      CREATE TABLE posting_lines (
        cpms_id VARCHAR,
        study_site VARCHAR,
        scenario_id VARCHAR,
        posting_amount DOUBLE CHECK (posting_amount >= 0)
      )
    ")

    dbExecute(con, "
      INSERT INTO posting_lines (cpms_id, study_site, scenario_id, posting_amount)
      VALUES ('CP1', 'RDUHT', 'A', 200.50)
    ")

    save_error <- tryCatch({
      DBI::dbWithTransaction(con, {
        dbExecute(
          con,
          paste(
            "DELETE FROM posting_lines",
            "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
          ),
          params = list("CP1", "RDUHT", "A")
        )
        dbAppendTable(con, "posting_lines", tibble(
          cpms_id = "CP1",
          study_site = "RDUHT",
          scenario_id = "A",
          posting_amount = -1
        ))
      })
      NULL
    }, error = function(e) e)

    survivors <- dbGetQuery(
      con,
      paste(
        "SELECT cpms_id, study_site, scenario_id, posting_amount",
        "FROM posting_lines",
        "WHERE cpms_id = 'CP1' AND study_site = 'RDUHT' AND scenario_id = 'A'"
      )
    )

    .atomic_expect("step4-style save fails when replacement posting_lines violate checks", inherits(save_error, "error"))
    .atomic_expect("step4-style save keeps prior posting_lines rows on failure", nrow(survivors) == 1L)
    .atomic_expect("step4-style save preserves prior posting_lines values", identical(survivors$posting_amount[[1]], 200.50))
  })

  .with_atomic_db(function(con, db_path) {
    dbExecute(con, "
      CREATE TABLE ict_costing_tbl (
        CPMS_ID VARCHAR,
        study_site VARCHAR,
        scenario_id VARCHAR,
        Study VARCHAR,
        Visit_Number VARCHAR,
        Study_Arm VARCHAR,
        Visit_Label VARCHAR,
        Activity_Name VARCHAR,
        ICT_Cost DOUBLE CHECK (ICT_Cost >= 0),
        Contract_Cost DOUBLE,
        activity_occurrence_id VARCHAR,
        staff_group INTEGER
      )
    ")

    dbExecute(con, "
      INSERT INTO ict_costing_tbl (
        CPMS_ID, study_site, scenario_id, Study, Visit_Number, Study_Arm,
        Visit_Label, Activity_Name, ICT_Cost, Contract_Cost, activity_occurrence_id, staff_group
      ) VALUES ('CP2', 'RDUHT', 'B', 'Study B', 'VISIT - 001', 'Arm B',
                'Baseline', 'MRI', 99.99, NULL, 'AO1', 1)
    ")

    persist_error <- tryCatch({
      persist_ict_to_duckdb(
        db_path,
        tibble(
          CPMS_ID = "CP2",
          study_site = "RDUHT",
          scenario_id = "B",
          Study = "Study B",
          Visit_Number = "VISIT - 001",
          Study_Arm = "Arm B",
          Visit_Label = "Baseline",
          Activity_Name = "MRI",
          ICT_Cost = -10,
          activity_occurrence_id = "AO2",
          staff_group = 1L
        )
      )
      NULL
    }, error = function(e) e)

    survivors <- dbGetQuery(
      con,
      paste(
        "SELECT CPMS_ID, study_site, scenario_id, ICT_Cost",
        "FROM ict_costing_tbl",
        "WHERE CPMS_ID = 'CP2' AND study_site = 'RDUHT' AND scenario_id = 'B'"
      )
    )
    tables_after <- dbListTables(con)

    .atomic_expect("persist_ict_to_duckdb surfaces insert failures", inherits(persist_error, "error"))
    .atomic_expect("persist_ict_to_duckdb keeps prior keyed rows on failure", nrow(survivors) == 1L)
    .atomic_expect("persist_ict_to_duckdb preserves prior keyed values", identical(survivors$ICT_Cost[[1]], 99.99))
    .atomic_expect("persist_ict_to_duckdb does not leave staging table behind after rollback", !"stg_ict_costing_tbl" %in% tables_after)
  })

  .with_atomic_db(function(con, db_path) {
    dbExecute(con, "
      CREATE TABLE user_api_credentials (
        credential_id INTEGER PRIMARY KEY,
        user_id INTEGER NOT NULL,
        provider TEXT NOT NULL,
        secret_ciphertext TEXT NOT NULL,
        secret_nonce TEXT NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")

    dbExecute(con, "
      INSERT INTO user_api_credentials
        (credential_id, user_id, provider, secret_ciphertext, secret_nonce)
      VALUES (1, 10, 'edge', 'cipher', 'nonce')
    ")

    rebuild_error <- tryCatch({
      dbExecute(con, "DROP TABLE IF EXISTS user_api_credentials__migrate")
      DBI::dbWithTransaction(con, {
        dbExecute(con, "
          CREATE TABLE user_api_credentials__migrate (
            credential_id INTEGER PRIMARY KEY,
            user_id INTEGER NOT NULL,
            provider TEXT NOT NULL,
            secret_ciphertext TEXT NOT NULL,
            secret_nonce TEXT NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
          )
        ")
        dbExecute(con, "
          INSERT INTO user_api_credentials__migrate
            (credential_id, user_id, provider, secret_ciphertext, secret_nonce, created_at, updated_at)
          SELECT
            credential_id, user_id, provider, secret_ciphertext, secret_nonce, created_at, updated_at
          FROM user_api_credentials
        ")
        dbExecute(con, "DROP TABLE user_api_credentials")
        dbExecute(con, "ALTER TABLE user_api_credentials__migrate RENAME TO missing_target_table")
        stop("forced failure before live credentials table is restored")
      })
      NULL
    }, error = function(e) e)

    live_count <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM user_api_credentials")$n[[1]]
    tables_after <- dbListTables(con)

    .atomic_expect("credentials rebuild surfaces errors inside the transaction", inherits(rebuild_error, "error"))
    .atomic_expect("credentials rebuild keeps live table after rollback", "user_api_credentials" %in% tables_after)
    .atomic_expect("credentials rebuild preserves existing rows after rollback", live_count == 1L)
    .atomic_expect("credentials rebuild rollback clears migrate table", !"user_api_credentials__migrate" %in% tables_after)
  })

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .atomic_passed, "    FAILED: ", .atomic_failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  invisible(list(passed = .atomic_passed, failed = .atomic_failed))
}
