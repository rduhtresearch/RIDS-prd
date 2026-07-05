# Replicates the bootstrap + working-tree release smoke checks from
# R/CI/run_ci_checks.R using the same underlying functions.

smoke_config <- function(temp_root, secret_stub, nested = FALSE) {
  base <- if (nested) file.path(temp_root, "shared") else temp_root
  config <- list(
    storage_mode = "duckdb",
    db_dir = normalizePath(file.path(base, "data", "RIDS.duckdb"), winslash = "/", mustWork = FALSE),
    ict_upload_dir = normalizePath(file.path(base, "uploads"), winslash = "/", mustWork = FALSE),
    edge_output_dir = normalizePath(file.path(base, "outputs"), winslash = "/", mustWork = FALSE),
    credential_secret = paste(rep(secret_stub, 2), collapse = "-"),
    app_log_dir = normalizePath(file.path(base, "logs"), winslash = "/", mustWork = FALSE),
    app_host = "127.0.0.1",
    app_port = 3838L
  )
  dir.create(dirname(config$db_dir), recursive = TRUE, showWarnings = FALSE)
  dir.create(config$ict_upload_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$edge_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$app_log_dir, recursive = TRUE, showWarnings = FALSE)
  config
}

test_that("bootstrap smoke check passes", {
  repo_dir <- rids_repo_root()
  temp_root <- tempfile("rids_ci_runtime_")
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)

  config_path <- file.path(temp_root, "deployment_config.R")
  write_deployment_config(config_path, smoke_config(temp_root, "ci-bootstrap-secret"))
  expect_no_error(run_release_smoke_check(repo_dir, config_path))
})

test_that("working-tree release smoke check passes", {
  repo_dir <- rids_repo_root()
  temp_root <- tempfile("rids_ci_release_")
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)

  config_path <- file.path(temp_root, "shared", "deployment_config.R")
  release_dir <- file.path(temp_root, "releases", default_bootstrap_release_version())

  write_deployment_config(config_path, smoke_config(temp_root, "ci-release-secret", nested = TRUE))
  expect_no_error(export_working_tree_snapshot(repo_dir, release_dir, overwrite = TRUE))
  expect_no_error(run_release_smoke_check(release_dir, config_path))
})
