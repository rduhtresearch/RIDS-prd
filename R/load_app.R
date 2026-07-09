# Single manifest of the app's source files, in load order.
#
# global.R calls rids_source_utils() (helpers + addon logic, needed before
# config/DB init); app.R calls rids_source_modules() (Shiny modules, needed
# after global.R has initialized config, CON, and the schema). Order within
# each list is preserved from the original source() chains.

RIDS_UTIL_FILES <- c(
  "R/utils/deployment_config.R",
  "R/config/runtime_config.R",
  "R/persistence/migrate.R",
  "R/persistence/connection.R",
  "R/persistence/repositories/settings_repository.R",
  "R/persistence/repositories/app_log_repository.R",
  "R/persistence/repositories/api_credential_repository.R",
  "R/persistence/repositories/user_repository.R",
  "R/persistence/repositories/session_repository.R",
  "R/persistence/repositories/auth_audit_repository.R",
  "R/persistence/repositories/study_repository.R",
  "R/persistence/repositories/ict_costing_repository.R",
  "R/persistence/repositories/posting_line_repository.R",
  "R/persistence/repositories/rules_repository.R",
  "R/persistence/repositories/speciality_repository.R",
  "R/persistence/repositories/mfa_repository.R",
  "R/utils/auth.r",
  "R/auth/totp.R",
  "R/auth/mfa.R",
  "R/auth/auth_provider.R",
  "R/utils/logging.R",
  "R/utils/user_credentials.R",
  "R/utils/db_error_handling.R",
  "R/utils/add_study_arm.r",
  "R/utils/pipeline_fixed.r",
  "R/utils/posting_engine.r",
  "R/utils/extract_cpms_id.r",
  "R/utils/template_build_main.r",
  "R/utils/posting_lines.r",
  "R/utils/dev_banner.r",
  "R/utils/loading_state_ui.R",
  "R/utils/add_cost_centres.r",
  "R/utils/screening_failure_transform.R",
  "R/utils/assign_edge_keys.R",
  "R/utils/adjust.r",
  "R/utils/build_template.r",
  "R/utils/validate_ict_workbook.r",
  "R/utils/study_deletion.R",
  "R/addons/custom_activities/ca_build_custom_rows.R",
  "R/addons/custom_activities/ca_schema.R",
  "R/addons/custom_activities/ca_ref_activities.R",
  "R/addons/custom_activities/ca_queries.R",
  "R/addons/custom_activities/ca_assign_edge_keys.R",
  "R/addons/custom_activities/apply_custom_activities.R"
)

RIDS_MODULE_FILES <- c(
  "R/modules/login_mod.R",
  "R/modules/sidebar_mod.R",
  "R/modules/app_mod.R",
  "R/modules/reporting_mod.R",
  "R/modules/settings_mod.R",
  "R/modules/integrations_mod.R",
  "R/modules/step1_mod.R",
  "R/modules/step2_mod.R",
  "R/modules/step3_mod.R",
  "R/modules/step4_mod.R",
  "R/modules/support_mod.R",
  "R/modules/admin_mod.r",
  "R/modules/progress_mod.R",
  "R/modules/help_mod.R",
  "R/modules/library_mod.R",
  "R/modules/edge_builder_mod.R",
  "R/modules/study_workspace_mod.R",
  "R/modules/custom_activity_module.R"
)

rids_source_files <- function(files) {
  for (f in files) {
    source(f, local = FALSE)
  }
  invisible(files)
}

rids_source_utils <- function() rids_source_files(RIDS_UTIL_FILES)
rids_source_modules <- function() rids_source_files(RIDS_MODULE_FILES)
