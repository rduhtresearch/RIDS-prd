.credential_passed <- 0L
.credential_failed <- 0L

.credential_expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .credential_passed <<- .credential_passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .credential_failed <<- .credential_failed + 1L
  }
}

.with_credential_db <- function(fn) {
  db_path <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

  old_con <- if (exists("CON", inherits = TRUE)) get("CON", inherits = TRUE) else NULL
  old_secret <- if (exists("CREDENTIAL_SECRET", inherits = TRUE)) get("CREDENTIAL_SECRET", inherits = TRUE) else NULL
  had_log_event <- exists("log_event", inherits = TRUE)
  old_log_event <- if (had_log_event) get("log_event", inherits = TRUE) else NULL
  had_app_log_exception <- exists("app_log_exception", inherits = TRUE)
  old_app_log_exception <- if (had_app_log_exception) get("app_log_exception", inherits = TRUE) else NULL

  assign("CON", con, envir = .GlobalEnv)
  assign("CREDENTIAL_SECRET", "credential-test-secret-1234", envir = .GlobalEnv)
  assign("log_event", function(...) invisible(TRUE), envir = .GlobalEnv)
  assign("app_log_exception", function(...) NULL, envir = .GlobalEnv)

  on.exit({
    if (is.null(old_con)) {
      rm("CON", envir = .GlobalEnv)
    } else {
      assign("CON", old_con, envir = .GlobalEnv)
    }

    if (is.null(old_secret)) {
      rm("CREDENTIAL_SECRET", envir = .GlobalEnv)
    } else {
      assign("CREDENTIAL_SECRET", old_secret, envir = .GlobalEnv)
    }

    if (had_log_event) {
      assign("log_event", old_log_event, envir = .GlobalEnv)
    } else if (exists("log_event", envir = .GlobalEnv, inherits = FALSE)) {
      rm("log_event", envir = .GlobalEnv)
    }

    if (had_app_log_exception) {
      assign("app_log_exception", old_app_log_exception, envir = .GlobalEnv)
    } else if (exists("app_log_exception", envir = .GlobalEnv, inherits = FALSE)) {
      rm("app_log_exception", envir = .GlobalEnv)
    }

    suppressWarnings(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE))
    if (file.exists(db_path)) {
      unlink(db_path, force = TRUE)
    }
  }, add = TRUE)

  source("R/setup.r")
  source("R/utils/user_credentials.R")

  user_tables()

  DBI::dbExecute(
    con,
    "INSERT INTO users (name, username, password_hash, role) VALUES ('Alice Admin', 'alice', 'hash', 'admin')"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO users (name, username, password_hash, role) VALUES ('Bob User', 'bob', 'hash', 'user')"
  )

  fn(con, db_path)
}

run_user_api_credential_tests <- function() {
  cat("\n=== user api credential tests ===\n\n")
  .credential_passed <<- 0L
  .credential_failed <<- 0L

  source("R/utils/deployment_config.R")

  .with_credential_db(function(con, db_path) {
    save_result <- save_user_api_credential(1L, "edge", "edge-key-123456")
    status <- get_user_api_credential_status(1L, "edge")
    decrypted <- get_user_api_credential(1L, "edge")
    stored_row <- DBI::dbGetQuery(
      con,
      "SELECT secret_ciphertext FROM user_api_credentials WHERE user_id = 1 AND provider = 'edge'"
    )

    .credential_expect("credential saves successfully", isTRUE(save_result$success))
    .credential_expect("credential status shows configured", isTRUE(status$configured))
    .credential_expect("credential decrypts to original value", identical(decrypted, "edge-key-123456"))
    .credential_expect("credential status is masked", !identical(status$masked_secret, "edge-key-123456"))
    .credential_expect(
      "database does not store plaintext key",
      nrow(stored_row) == 1L && !grepl("edge-key-123456", stored_row$secret_ciphertext[[1]], fixed = TRUE)
    )
  })

  .with_credential_db(function(con, db_path) {
    save_user_api_credential(1L, "edge", "first-key")
    save_user_api_credential(1L, "edge", "second-key")
    count <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM user_api_credentials WHERE user_id = 1 AND provider = 'edge'"
    )$n[[1]]
    secret <- get_user_api_credential(1L, "edge")

    .credential_expect("saving again replaces the same credential row", count == 1L)
    .credential_expect("replacement keeps latest key", identical(secret, "second-key"))
  })

  .with_credential_db(function(con, db_path) {
    save_user_api_credential(1L, "edge", "restore-key")

    backup_path <- tempfile(fileext = ".duckdb")
    on.exit(if (file.exists(backup_path)) unlink(backup_path, force = TRUE), add = TRUE)
    DBI::dbDisconnect(con, shutdown = TRUE)
    file.copy(db_path, backup_path, overwrite = TRUE)

    restored_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = backup_path)
    on.exit(suppressWarnings(try(DBI::dbDisconnect(restored_con, shutdown = TRUE), silent = TRUE)), add = TRUE)
    assign("CON", restored_con, envir = .GlobalEnv)

    restored_secret <- get_user_api_credential(1L, "edge")
    .credential_expect("credential survives backup-style copy", identical(restored_secret, "restore-key"))
  })

  .with_credential_db(function(con, db_path) {
    save_user_api_credential(2L, "edge", "bob-key")
    missing <- get_user_api_credential(1L, "edge")
    deleted <- delete_user_api_credential(2L, "edge")
    after_delete <- get_user_api_credential_status(2L, "edge")

    .credential_expect("another user cannot read a missing credential", is.null(missing))
    .credential_expect("delete returns true when a credential exists", isTRUE(deleted))
    .credential_expect("deleted credential is no longer configured", !isTRUE(after_delete$configured))
  })

  .with_credential_db(function(con, db_path) {
    save_user_api_credential(2L, "edge", "bob-key")
    result <- update_user_account(
      user_id = 2L,
      name = "Bob Updated",
      username = "bob",
      email = "bob@example.com",
      role = "user",
      active = TRUE,
      actor_user_id = 1L
    )

    .credential_expect("user can still be updated with saved credential", isTRUE(result$success))
    .credential_expect(
      "saved credential still decrypts after user update",
      identical(get_user_api_credential(2L, "edge"), "bob-key")
    )
  })

  temp_root <- tempfile("rids_missing_secret_")
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)

  dir.create(file.path(temp_root, "shared"), recursive = TRUE, showWarnings = FALSE)
  config_path <- file.path(temp_root, "shared", "deployment_config.R")
  writeLines(c(
    'STORAGE_MODE <- "duckdb"',
    paste0('DB_DIR <- "', normalizePath(file.path(temp_root, "db.duckdb"), winslash = "/", mustWork = FALSE), '"'),
    paste0('ICT_UPLOAD_DIR <- "', normalizePath(file.path(temp_root, "uploads"), winslash = "/", mustWork = FALSE), '"'),
    paste0('EDGE_OUTPUT_DIR <- "', normalizePath(file.path(temp_root, "outputs"), winslash = "/", mustWork = FALSE), '"'),
    paste0('APP_LOG_DIR <- "', normalizePath(file.path(temp_root, "logs"), winslash = "/", mustWork = FALSE), '"'),
    'APP_HOST <- "127.0.0.1"',
    'APP_PORT <- 3838L'
  ), config_path, useBytes = TRUE)

  config_error <- tryCatch({
    load_runtime_config(temp_root)
    NULL
  }, error = function(e) e)

  .credential_expect("runtime config fails clearly when secret is missing", inherits(config_error, "error"))
  .credential_expect(
    "runtime config error mentions CREDENTIAL_SECRET",
    grepl("CREDENTIAL_SECRET", config_error$message, fixed = TRUE)
  )

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .credential_passed, "    FAILED: ", .credential_failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  invisible(list(passed = .credential_passed, failed = .credential_failed))
}
