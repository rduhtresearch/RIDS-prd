# Run the RIDS test suite: legacy wrapped suites + native testthat tests.
# Invoke from the repo root with: Rscript tests/testthat.R
library(testthat)

repo_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
while (!file.exists(file.path(repo_root, "app.R"))) {
  parent <- dirname(repo_root)
  if (identical(parent, repo_root)) {
    stop("Could not locate repo root (no app.R found walking up from ", getwd(), ")")
  }
  repo_root <- parent
}

Sys.setenv(RIDS_REPO_ROOT = repo_root)
results <- test_dir(file.path(repo_root, "tests", "testthat"), stop_on_failure = TRUE)
