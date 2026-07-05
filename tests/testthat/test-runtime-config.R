# Tests for the env-var-first config reader (R/config/runtime_config.R),
# including parity with the legacy deployment_config.R file reader.

config_env_vars <- c(
  "RIDS_STORAGE_MODE", "RIDS_DB_DIR", "RIDS_ICT_UPLOAD_DIR",
  "RIDS_EDGE_OUTPUT_DIR", "RIDS_CREDENTIAL_SECRET", "RIDS_APP_STATUS",
  "RIDS_APP_LOG_DIR", "RIDS_APP_HOST", "RIDS_APP_PORT", "RIDS_CONFIG_PATH"
)

with_clean_config_env <- function(code) {
  withr::with_envvar(
    setNames(rep(NA_character_, length(config_env_vars)), config_env_vars),
    code
  )
}

write_test_config <- function(dir) {
  config <- list(
    storage_mode = "duckdb",
    db_dir = file.path(dir, "data", "RIDS.duckdb"),
    ict_upload_dir = file.path(dir, "uploads"),
    edge_output_dir = file.path(dir, "outputs"),
    credential_secret = "file-secret-file-secret",
    app_status = "test",
    app_log_dir = file.path(dir, "logs"),
    app_host = "127.0.0.1",
    app_port = 4000L
  )
  path <- file.path(dir, "deployment_config.R")
  write_deployment_config(path, config)
  list(path = path, config = config)
}

test_that("config resolves entirely from environment variables (no file)", {
  source_from_root("R/config/runtime_config.R")
  temp_root <- withr::local_tempdir()

  with_clean_config_env({
    withr::with_envvar(c(
      RIDS_STORAGE_MODE = "duckdb",
      RIDS_DB_DIR = file.path(temp_root, "db", "app.duckdb"),
      RIDS_ICT_UPLOAD_DIR = file.path(temp_root, "up"),
      RIDS_EDGE_OUTPUT_DIR = file.path(temp_root, "out"),
      RIDS_CREDENTIAL_SECRET = "env-secret-env-secret",
      RIDS_APP_STATUS = "dev",
      RIDS_APP_PORT = "5001"
    ), {
      cfg <- load_app_config(temp_root)
      expect_identical(cfg$storage_mode, "duckdb")
      expect_identical(cfg$db_dir, file.path(temp_root, "db", "app.duckdb"))
      expect_identical(cfg$credential_secret, "env-secret-env-secret")
      expect_identical(cfg$app_status, "dev")
      expect_identical(cfg$app_port, 5001L)
      expect_identical(cfg$source_path, "environment")
      expect_identical(cfg$app_host, "127.0.0.1")
    })
  })
})

test_that("config falls back to a legacy deployment_config.R file", {
  source_from_root("R/config/runtime_config.R")
  temp_root <- withr::local_tempdir()
  written <- write_test_config(temp_root)

  with_clean_config_env({
    withr::with_envvar(c(RIDS_CONFIG_PATH = written$path), {
      cfg <- load_app_config(temp_root)
      legacy <- read_runtime_config(written$path)

      expect_identical(cfg$storage_mode, legacy$storage_mode)
      expect_identical(cfg$db_dir, legacy$db_dir)
      expect_identical(cfg$ict_upload_dir, legacy$ict_upload_dir)
      expect_identical(cfg$edge_output_dir, legacy$edge_output_dir)
      expect_identical(cfg$credential_secret, legacy$credential_secret)
      expect_identical(cfg$app_status, legacy$app_status)
      expect_identical(cfg$app_port, legacy$app_port)
      expect_identical(cfg$source_path, legacy$source_path)
    })
  })
})

test_that("environment variables override file values key by key", {
  source_from_root("R/config/runtime_config.R")
  temp_root <- withr::local_tempdir()
  written <- write_test_config(temp_root)

  with_clean_config_env({
    withr::with_envvar(c(
      RIDS_CONFIG_PATH = written$path,
      RIDS_APP_STATUS = "dev",
      RIDS_APP_PORT = "5002"
    ), {
      cfg <- load_app_config(temp_root)
      expect_identical(cfg$app_status, "dev")     # overridden
      expect_identical(cfg$app_port, 5002L)       # overridden
      expect_identical(cfg$credential_secret, "file-secret-file-secret") # from file
    })
  })
})

test_that("validation matches legacy rules", {
  source_from_root("R/config/runtime_config.R")
  temp_root <- withr::local_tempdir()

  with_clean_config_env({
    # missing everything
    expect_error(load_app_config(temp_root), "RIDS_DB_DIR")

    # short secret rejected
    withr::with_envvar(c(
      RIDS_DB_DIR = file.path(temp_root, "x.duckdb"),
      RIDS_ICT_UPLOAD_DIR = file.path(temp_root, "up"),
      RIDS_EDGE_OUTPUT_DIR = file.path(temp_root, "out"),
      RIDS_CREDENTIAL_SECRET = "too-short"
    ), {
      expect_error(load_app_config(temp_root), "at least 16 characters")
    })

    # invalid app_status coerced to live; invalid port coerced to 3838
    withr::with_envvar(c(
      RIDS_DB_DIR = file.path(temp_root, "x.duckdb"),
      RIDS_ICT_UPLOAD_DIR = file.path(temp_root, "up"),
      RIDS_EDGE_OUTPUT_DIR = file.path(temp_root, "out"),
      RIDS_CREDENTIAL_SECRET = "long-enough-secret-value",
      RIDS_APP_STATUS = "banana",
      RIDS_APP_PORT = "not-a-port"
    ), {
      cfg <- load_app_config(temp_root)
      expect_identical(cfg$app_status, "live")
      expect_identical(cfg$app_port, 3838L)
    })
  })
})
