repo_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(repo_dir, "R/utils/deployment_config.R"))
source(file.path(repo_dir, "R/utils/release_management.R"))

run_test_result <- function(label, fn) {
  result <- fn()
  failed <- result$failed %||% 0L

  if (!identical(as.integer(failed), 0L)) {
    stop(label, " failed with ", failed, " failing checks.")
  }

  invisible(result)
}

run_bootstrap_check <- function() {
  temp_root <- tempfile("rids_ci_runtime_")
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)

  config_path <- file.path(temp_root, "deployment_config.R")
  config <- list(
    storage_mode = "duckdb",
    db_dir = normalizePath(file.path(temp_root, "data", "RIDS.duckdb"), winslash = "/", mustWork = FALSE),
    ict_upload_dir = normalizePath(file.path(temp_root, "uploads"), winslash = "/", mustWork = FALSE),
    edge_output_dir = normalizePath(file.path(temp_root, "outputs"), winslash = "/", mustWork = FALSE),
    credential_secret = paste(rep("ci-bootstrap-secret", 2), collapse = "-"),
    app_log_dir = normalizePath(file.path(temp_root, "logs"), winslash = "/", mustWork = FALSE),
    app_host = "127.0.0.1",
    app_port = 3838L,
    sql_server = "",
    sql_database = "",
    sql_driver = ""
  )

  dir.create(dirname(config$db_dir), recursive = TRUE, showWarnings = FALSE)
  dir.create(config$ict_upload_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$edge_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$app_log_dir, recursive = TRUE, showWarnings = FALSE)

  write_deployment_config(config_path, config)
  run_release_smoke_check(repo_dir, config_path)
  message("Bootstrap check passed.")
}

run_working_tree_release_check <- function() {
  temp_root <- tempfile("rids_ci_release_")
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)

  config_path <- file.path(temp_root, "shared", "deployment_config.R")
  release_dir <- file.path(temp_root, "releases", default_bootstrap_release_version())

  config <- list(
    storage_mode = "duckdb",
    db_dir = normalizePath(file.path(temp_root, "shared", "data", "RIDS.duckdb"), winslash = "/", mustWork = FALSE),
    ict_upload_dir = normalizePath(file.path(temp_root, "shared", "uploads"), winslash = "/", mustWork = FALSE),
    edge_output_dir = normalizePath(file.path(temp_root, "shared", "outputs"), winslash = "/", mustWork = FALSE),
    credential_secret = paste(rep("ci-release-secret", 2), collapse = "-"),
    app_log_dir = normalizePath(file.path(temp_root, "shared", "logs"), winslash = "/", mustWork = FALSE),
    app_host = "127.0.0.1",
    app_port = 3838L,
    sql_server = "",
    sql_database = "",
    sql_driver = ""
  )

  dir.create(dirname(config$db_dir), recursive = TRUE, showWarnings = FALSE)
  dir.create(config$ict_upload_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$edge_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$app_log_dir, recursive = TRUE, showWarnings = FALSE)

  write_deployment_config(config_path, config)
  export_working_tree_snapshot(repo_dir, release_dir, overwrite = TRUE)
  run_release_smoke_check(release_dir, config_path)
  message("Working-tree release bootstrap check passed.")
}

source(file.path(repo_dir, "R/addons/custom_activities/ca_build_custom_rows.R"), local = FALSE)
source(file.path(repo_dir, "R/addons/custom_activities/ca_schema.R"), local = FALSE)
source(file.path(repo_dir, "R/addons/custom_activities/ca_ref_activities.R"), local = FALSE)
source(file.path(repo_dir, "R/addons/custom_activities/ca_queries.R"), local = FALSE)
source(file.path(repo_dir, "R/addons/custom_activities/ca_assign_edge_keys.R"), local = FALSE)
source(file.path(repo_dir, "R/addons/custom_activities/apply_custom_activities.R"), local = FALSE)

run_bootstrap_check()
run_working_tree_release_check()

source(file.path(repo_dir, "R/tests/test_setup_migrations.R"), local = FALSE)
run_test_result("test_setup_migrations.R", run_setup_migration_tests)

source(file.path(repo_dir, "R/tests/test_ca_build_custom_rows.R"), local = FALSE)
run_test_result("test_ca_build_custom_rows.R", run_ca_tests)

source(file.path(repo_dir, "R/tests/test_ca_ref_activities.R"), local = FALSE)
run_test_result("test_ca_ref_activities.R", run_ca_ref_tests)

source(file.path(repo_dir, "R/tests/test_ca_queries.R"), local = FALSE)
run_test_result("test_ca_queries.R", run_ca_query_tests)

source(file.path(repo_dir, "R/tests/test_ca_chunk3.R"), local = FALSE)
run_test_result("test_ca_chunk3.R", run_ca_chunk3_tests)

source(file.path(repo_dir, "R/modules/edge_builder_mod.R"), local = FALSE)
source(file.path(repo_dir, "R/tests/test_edge_builder_module.R"), local = FALSE)
run_test_result("test_edge_builder_module.R", run_edge_builder_module_tests)

source(file.path(repo_dir, "R/modules/step2_mod.R"), local = FALSE)
source(file.path(repo_dir, "R/tests/test_step2_filters.R"), local = FALSE)
run_test_result("test_step2_filters.R", run_step2_filter_tests)

source(file.path(repo_dir, "R/modules/step4_mod.R"), local = FALSE)
source(file.path(repo_dir, "R/tests/test_step4_persistence.R"), local = FALSE)
run_test_result("test_step4_persistence.R", run_step4_persistence_tests)

source(file.path(repo_dir, "R/tests/test_atomic_save_transactions.R"), local = FALSE)
run_test_result("test_atomic_save_transactions.R", run_atomic_save_transaction_tests)

source(file.path(repo_dir, "R/tests/test_wal_backup_handling.R"), local = FALSE)
run_test_result("test_wal_backup_handling.R", run_wal_backup_handling_tests)

source(file.path(repo_dir, "R/tests/test_contract_cost_source_of_truth.R"), local = FALSE)
run_test_result("test_contract_cost_source_of_truth.R", run_contract_cost_source_of_truth_tests)

source(file.path(repo_dir, "R/tests/test_study_deletion.R"), local = FALSE)
run_test_result("test_study_deletion.R", run_study_deletion_tests)

source(file.path(repo_dir, "R/tests/test_cost_centre_matrix_simple.R"), local = FALSE)
run_test_result("test_cost_centre_matrix_simple.R", run_cost_centre_matrix_simple_tests)

source(file.path(repo_dir, "R/tests/test_auth_password_reset.R"), local = FALSE)
run_test_result("test_auth_password_reset.R", run_auth_password_reset_tests)

source(file.path(repo_dir, "R/tests/test_user_api_credentials.R"), local = FALSE)
run_test_result("test_user_api_credentials.R", run_user_api_credential_tests)

source(file.path(repo_dir, "R/tests/test_release_workflow.R"), local = FALSE)
run_test_result("test_release_workflow.R", run_release_workflow_tests)

message("All CI checks passed.")
