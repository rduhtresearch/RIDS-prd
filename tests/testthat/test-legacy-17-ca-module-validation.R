test_that("legacy suite: test_custom_activity_module_validation.R", {
  run_legacy_suite(
    "run_custom_activity_module_validation_tests",
    "test_custom_activity_module_validation.R",
    deps = "R/modules/custom_activity_module.R"
  )
})
