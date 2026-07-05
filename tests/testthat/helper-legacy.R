# Shared helpers for wrapping the legacy R/tests suites in testthat.
#
# Each legacy suite defines run_xxx_tests() returning list(passed, failed) and
# prints per-assertion PASS/FAIL lines. run_legacy_suite() executes a suite
# from the repo root (legacy files source dependencies via repo-relative
# paths) and converts its result into testthat expectations, so every legacy
# assertion still runs unchanged.

rids_repo_root <- function() {
  root <- Sys.getenv("RIDS_REPO_ROOT", "")
  if (nzchar(root)) {
    return(root)
  }

  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(root, "app.R"))) {
    parent <- dirname(root)
    if (identical(parent, root)) {
      stop("Could not locate repo root (no app.R found)")
    }
    root <- parent
  }
  root
}

# Source repo files into the global environment, mirroring how
# R/CI/run_ci_checks.R prepares dependencies before running a suite.
source_from_root <- function(...) {
  root <- rids_repo_root()
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)
  for (path in c(...)) {
    source(file.path(root, path), local = FALSE)
  }
}

run_legacy_suite <- function(runner_name, test_file, deps = character(0)) {
  root <- rids_repo_root()
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)

  source_from_root(deps)
  source(file.path(root, "R", "tests", test_file), local = FALSE)

  runner <- get(runner_name, envir = .GlobalEnv)
  result <- runner()

  passed <- result$passed %||% 0L
  failed <- result$failed %||% 0L

  testthat::expect_gt(passed, 0)
  testthat::expect_equal(
    as.integer(failed), 0L,
    label = sprintf("failing checks in %s", test_file)
  )
  invisible(result)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(y)
  }
  x
}

# Common dependencies for the legacy suites, mirroring the preamble of
# R/CI/run_ci_checks.R (which sources these before running any suite).
source_from_root(
  "R/utils/deployment_config.R",
  "R/utils/release_management.R",
  "R/addons/custom_activities/ca_build_custom_rows.R",
  "R/addons/custom_activities/ca_schema.R",
  "R/addons/custom_activities/ca_ref_activities.R",
  "R/addons/custom_activities/ca_queries.R",
  "R/addons/custom_activities/ca_assign_edge_keys.R",
  "R/addons/custom_activities/apply_custom_activities.R"
)
