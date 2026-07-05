suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(y)
  }
  x
}

.setup_passed <- 0L
.setup_failed <- 0L

.setup_expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .setup_passed <<- .setup_passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .setup_failed <<- .setup_failed + 1L
  }
}

.with_setup_db <- function(fn) {
  db_path <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

  old_con <- if (exists("CON", inherits = TRUE)) get("CON", inherits = TRUE) else NULL
  had_app_log_exception <- exists("app_log_exception", inherits = TRUE)
  old_app_log_exception <- if (had_app_log_exception) get("app_log_exception", inherits = TRUE) else NULL

  assign("CON", con, envir = .GlobalEnv)
  assign("app_log_exception", function(...) NULL, envir = .GlobalEnv)

  on.exit({
    if (is.null(old_con)) {
      rm("CON", envir = .GlobalEnv)
    } else {
      assign("CON", old_con, envir = .GlobalEnv)
    }

    if (had_app_log_exception) {
      assign("app_log_exception", old_app_log_exception, envir = .GlobalEnv)
    } else if (exists("app_log_exception", envir = .GlobalEnv, inherits = FALSE)) {
      rm("app_log_exception", envir = .GlobalEnv)
    }

    dbDisconnect(con, shutdown = TRUE)
    if (file.exists(db_path)) {
      unlink(db_path, force = TRUE)
    }
  }, add = TRUE)

  fn(con)
}

.seed_auth_tables <- function(con, extra_user_column = FALSE, missing_last_login = FALSE) {
  dbExecute(con, "CREATE SEQUENCE user_id_seq;")
  dbExecute(con, "CREATE SEQUENCE auth_session_id_seq;")
  dbExecute(con, "CREATE SEQUENCE auth_audit_id_seq;")

  last_login_col <- if (isTRUE(missing_last_login)) {
    ""
  } else {
    ", last_login_at TIMESTAMP"
  }
  extra_col <- if (isTRUE(extra_user_column)) {
    ", future_auth_flag BOOLEAN"
  } else {
    ""
  }

  users_sql <- paste0(
    "CREATE TABLE users (
      user_id INTEGER PRIMARY KEY DEFAULT nextval('user_id_seq'),
      name TEXT,
      username TEXT UNIQUE NOT NULL,
      email TEXT,
      password_hash TEXT,
      role TEXT NOT NULL DEFAULT 'user',
      active BOOLEAN NOT NULL DEFAULT TRUE,
      force_password_change BOOLEAN NOT NULL DEFAULT FALSE,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
      last_login_col,
      extra_col,
    ")"
  )
  dbExecute(con, users_sql)

  dbExecute(con, "
    CREATE TABLE auth_sessions (
      session_id INTEGER PRIMARY KEY DEFAULT nextval('auth_session_id_seq'),
      user_id INTEGER NOT NULL,
      token_hash TEXT NOT NULL,
      expires_at TIMESTAMP NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      revoked_at TIMESTAMP,
      user_agent TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE auth_audit_log (
      audit_id INTEGER PRIMARY KEY DEFAULT nextval('auth_audit_id_seq'),
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      event_type TEXT NOT NULL,
      user_id INTEGER,
      actor_user_id INTEGER,
      username TEXT,
      success BOOLEAN NOT NULL DEFAULT TRUE,
      message TEXT,
      session_id INTEGER
    )
  ")

  dbExecute(
    con,
    "INSERT INTO users (name, username, password_hash, role) VALUES (?, ?, ?, ?)",
    params = list("Alice Admin", "alice", "hash", "admin")
  )
  dbExecute(
    con,
    "INSERT INTO auth_sessions (user_id, token_hash, expires_at) VALUES (1, 'token', CURRENT_TIMESTAMP)"
  )
  dbExecute(
    con,
    "INSERT INTO auth_audit_log (event_type, user_id, username) VALUES ('login_success', 1, 'alice')"
  )
}

run_setup_migration_tests <- function() {
  cat("\n=== setup migration tests ===\n\n")
  .setup_passed <<- 0L
  .setup_failed <<- 0L

  source("R/setup.r")

  .with_setup_db(function(con) {
    .seed_auth_tables(con, missing_last_login = TRUE)
    user_tables()

    .setup_expect(
      "missing users column is added",
      "last_login_at" %in% dbListFields(con, "users")
    )
    .setup_expect(
      "users survive additive migration",
      dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n[[1]] == 1L
    )
    .setup_expect(
      "sessions survive additive migration",
      dbGetQuery(con, "SELECT COUNT(*) AS n FROM auth_sessions")$n[[1]] == 1L
    )
    .setup_expect(
      "audit rows survive additive migration",
      dbGetQuery(con, "SELECT COUNT(*) AS n FROM auth_audit_log")$n[[1]] == 1L
    )
    .setup_expect(
      "user_api_credentials table is created",
      "user_api_credentials" %in% dbListTables(con)
    )
  })

  .with_setup_db(function(con) {
    .seed_auth_tables(con, extra_user_column = TRUE)
    result <- tryCatch({
      user_tables()
      NULL
    }, error = function(e) e)

    .setup_expect("extra users column stops startup", inherits(result, "error"))
    .setup_expect(
      "extra users column message asks for manual migration",
      grepl("manual auth migration", result$message, fixed = TRUE)
    )
    .setup_expect(
      "users survive unsupported schema stop",
      dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n[[1]] == 1L
    )
    .setup_expect(
      "audit rows survive unsupported schema stop",
      dbGetQuery(con, "SELECT COUNT(*) AS n FROM auth_audit_log")$n[[1]] == 1L
    )
  })

  .with_setup_db(function(con) {
    .seed_auth_tables(con)
    dbExecute(con, "CREATE TABLE tokens (token TEXT)")
    result <- tryCatch({
      user_tables()
      NULL
    }, error = function(e) e)

    .setup_expect("legacy tokens table stops startup", inherits(result, "error"))
    .setup_expect(
      "legacy tokens message asks for backup",
      grepl("Take a database backup", result$message, fixed = TRUE)
    )
    .setup_expect(
      "users survive legacy tokens stop",
      dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n[[1]] == 1L
    )
    .setup_expect(
      "legacy tokens table is preserved",
      "tokens" %in% dbListTables(con)
    )
  })

  .with_setup_db(function(con) {
    dbExecute(con, "CREATE TABLE ict_uploads (id INTEGER)")
    dbExecute(con, "INSERT INTO ict_uploads VALUES (1)")
    meta_table()

    .setup_expect(
      "meta_table does not drop ict_uploads",
      "ict_uploads" %in% dbListTables(con)
    )
    .setup_expect(
      "ict_uploads rows survive startup",
      dbGetQuery(con, "SELECT COUNT(*) AS n FROM ict_uploads")$n[[1]] == 1L
    )
  })

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .setup_passed, "    FAILED: ", .setup_failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  invisible(list(passed = .setup_passed, failed = .setup_failed))
}
