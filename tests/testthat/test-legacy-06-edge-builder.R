test_that("legacy suite: test_edge_builder_module.R", {
  run_legacy_suite("run_edge_builder_module_tests", "test_edge_builder_module.R", deps = "R/modules/edge_builder_mod.R")
})
