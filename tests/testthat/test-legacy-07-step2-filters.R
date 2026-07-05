test_that("legacy suite: test_step2_filters.R", {
  run_legacy_suite("run_step2_filter_tests", "test_step2_filters.R", deps = "R/modules/step2_mod.R")
})
