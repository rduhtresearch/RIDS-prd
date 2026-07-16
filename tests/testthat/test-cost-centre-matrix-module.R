test_that("cost centre matrix module exposes the operational workspace", {
  suppressPackageStartupMessages({
    library(shiny)
    library(reactable)
  })

  module_env <- new.env(parent = .GlobalEnv)
  sys.source(
    file.path(rids_repo_root(), "R", "utils", "add_cost_centres.r"),
    envir = module_env
  )
  sys.source(
    file.path(rids_repo_root(), "R", "modules", "cost_centre_matrix_mod.R"),
    envir = module_env
  )

  markup <- htmltools::renderTags(module_env$costCentreMatrixUI("matrix"))$html

  expect_match(markup, "Cost Centre Matrix", fixed = TRUE)
  expect_match(markup, "Active split columns", fixed = TRUE)
  expect_match(markup, "Matrix viewer", fixed = TRUE)
  expect_match(markup, "matrix-matrix_overview", fixed = TRUE)
  expect_match(markup, "matrix-active_split_columns", fixed = TRUE)
  expect_match(markup, "matrix-matrix_table_state", fixed = TRUE)
})

test_that("matrix presentation helpers preserve canonical split columns", {
  suppressPackageStartupMessages({
    library(shiny)
    library(reactable)
  })

  module_env <- new.env(parent = .GlobalEnv)
  sys.source(
    file.path(rids_repo_root(), "R", "utils", "add_cost_centres.r"),
    envir = module_env
  )
  sys.source(
    file.path(rids_repo_root(), "R", "modules", "cost_centre_matrix_mod.R"),
    envir = module_env
  )

  expect_identical(module_env$matrix_canonical_column_name("DIRECT_COST"), "DIRECT")
  expect_identical(module_env$matrix_canonical_column_name("Department"), "Department")
  expect_identical(module_env$matrix_format_count(1200L), "1,200")

  badge_markup <- htmltools::renderTags(
    module_env$matrix_split_badge("DIRECT", 12L)
  )$html
  expect_match(badge_markup, "12 mapped rows", fixed = TRUE)
})

test_that("matrix upload preview validates CSV input before activation", {
  suppressPackageStartupMessages({
    library(shiny)
    library(reactable)
  })

  module_env <- new.env(parent = .GlobalEnv)
  sys.source(
    file.path(rids_repo_root(), "R", "utils", "add_cost_centres.r"),
    envir = module_env
  )
  sys.source(
    file.path(rids_repo_root(), "R", "modules", "cost_centre_matrix_mod.R"),
    envir = module_env
  )

  module_env$is_admin <- function(role) identical(role, "admin")
  module_env$cc_get_setting <- function(key, default = "") default
  module_env$cc_allowed_posting_line_types <- function() "DIRECT"

  matrix_path <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      "Department" = "Study Team",
      "Activity Type" = "Baseline",
      "Staff Role" = "Research Nurse",
      "DIRECT" = "52010",
      check.names = FALSE
    ),
    matrix_path,
    row.names = FALSE
  )

  auth_state <- reactiveValues(
    logged_in = TRUE,
    role = "admin",
    name = "Test Admin",
    username = "test.admin"
  )

  testServer(
    module_env$costCentreMatrixServer,
    args = list(auth_state = auth_state),
    {
      session$setInputs(matrix_upload = list(
        name = "matrix.csv",
        size = file.info(matrix_path)$size[[1]],
        type = "text/csv",
        datapath = matrix_path
      ))
      session$flushReact()

      expect_match(output$upload_preview$html, "Validation passed", fixed = TRUE)
      expect_match(output$upload_preview$html, "Detected active splits", fixed = TRUE)
      expect_match(output$upload_primary_action$html, "Review activation", fixed = TRUE)
      expect_false(grepl("disabled", output$upload_primary_action$html, fixed = TRUE))
    }
  )
})
