test_that("legacy suite: test_step4_persistence.R", {
  run_legacy_suite(
    "run_step4_persistence_tests",
    "test_step4_persistence.R",
    deps = c(
      "R/utils/amendment_workflow_ui.R",
      "R/utils/amendment_template_names.R",
      "R/modules/step4_mod.R"
    )
  )
})
