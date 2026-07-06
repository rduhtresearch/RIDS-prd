# CI entry point: runs the full testthat suite (which wraps every legacy
# R/tests suite plus the native tests). Kept for compatibility with
# `Rscript R/CI/run_ci_checks.R`; equivalent to `Rscript tests/testthat.R`.
#
# Set RIDS_TEST_PG_URL to a disposable PostgreSQL database to include the
# PostgreSQL integration tests (skipped otherwise).

source("tests/testthat.R")

message("All CI checks passed.")
