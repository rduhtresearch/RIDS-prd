render_ui_markup <- function(ui) {
  htmltools::renderTags(ui)$html
}

ui_module_environment <- function(path) {
  env <- new.env(parent = .GlobalEnv)
  sys.source(file.path(rids_repo_root(), path), envir = env)
  env
}

test_that("contextual help exposes an accessible dialog contract", {
  suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
  })

  module_env <- ui_module_environment("R/modules/help_mod.R")
  markup <- render_ui_markup(module_env$helpUI("help"))

  expect_match(markup, "rids-help-toggle", fixed = TRUE)
  expect_match(markup, "rids-help-panel", fixed = TRUE)
  expect_match(markup, "rids-help-close", fixed = TRUE)
  expect_match(markup, 'aria-label="Open help panel"', fixed = TRUE)
  expect_match(markup, 'aria-controls="help-panel"', fixed = TRUE)
  expect_match(markup, 'aria-expanded="false"', fixed = TRUE)
  expect_match(markup, 'role="dialog"', fixed = TRUE)
  expect_match(markup, 'aria-modal="true"', fixed = TRUE)
  expect_match(markup, 'aria-hidden="true"', fixed = TRUE)
  expect_match(markup, 'aria-labelledby="help-title"', fixed = TRUE)
  expect_match(markup, 'id="help-title"', fixed = TRUE)
  expect_match(markup, 'aria-label="Close help panel"', fixed = TRUE)
  expect_false(grepl("width: 360px", markup, fixed = TRUE))
})

test_that("contextual help content renders while its dialog is hidden", {
  suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
  })

  module_env <- ui_module_environment("R/modules/help_mod.R")
  content <- list(
    title = "Study setup",
    sections = list(list(
      heading = "Workbook",
      body = "Upload the completed file."
    ))
  )

  testServer(module_env$helpServer, args = list(content = content), {
    rendered <- output$help_content
    expect_match(rendered$html, "Study setup", fixed = TRUE)
    expect_match(rendered$html, "Workbook", fixed = TRUE)
    expect_match(rendered$html, "Upload the completed file.", fixed = TRUE)
  })
})

test_that("custom activity cost mode uses a labelled full-size switch", {
  suppressPackageStartupMessages({
    library(shiny)
    library(bs4Dash)
    library(reactable)
    library(shinyjs)
  })

  module_env <- ui_module_environment("R/modules/custom_activity_module.R")
  markup <- render_ui_markup(
    module_env$custom_activity_mode_control(NS("custom"))
  )

  expect_match(markup, "ca-mode-row", fixed = TRUE)
  expect_match(markup, "ca-mode-switch-input", fixed = TRUE)
  expect_match(markup, 'aria-label="Use baseline five-row cost mode"', fixed = TRUE)
  expect_match(markup, 'aria-describedby="custom-mode_left_label custom-mode_right_label"', fixed = TRUE)
  expect_false(grepl("opacity: 0", markup, fixed = TRUE))
  expect_false(grepl("width: 0", markup, fixed = TRUE))
  expect_false(grepl("height: 0", markup, fixed = TRUE))

  hint_markup <- render_ui_markup(
    module_env[[".field_hint"]](FALSE, NULL, "Choose the matching activity.")
  )
  expect_match(hint_markup, "ca-field-hint", fixed = TRUE)
  expect_false(grepl("#9aa4ad", hint_markup, fixed = TRUE))
})

test_that("admin UI uses responsive settings and table wrappers", {
  suppressPackageStartupMessages({
    library(shiny)
    library(bs4Dash)
    library(reactable)
  })

  module_env <- new.env(parent = .GlobalEnv)
  module_env$ICT_UPLOAD_DIR <- "/tmp/uploads"
  module_env$EDGE_OUTPUT_DIR <- "/tmp/outputs"
  sys.source(
    file.path(rids_repo_root(), "R", "modules", "admin_mod.r"),
    envir = module_env
  )
  markup <- render_ui_markup(module_env$adminUI("admin"))

  expect_match(markup, "rids-table-scroll", fixed = TRUE)
  expect_match(markup, 'role="region"', fixed = TRUE)
  expect_match(markup, 'aria-label="User accounts table"', fixed = TRUE)
  expect_match(markup, 'tabindex="0"', fixed = TRUE)
  expect_equal(length(gregexpr("rids-admin-setting-row", markup, fixed = TRUE)[[1]]), 2L)
  expect_equal(length(gregexpr("rids-admin-setting-field", markup, fixed = TRUE)[[1]]), 2L)
  expect_equal(length(gregexpr("rids-admin-setting-action", markup, fixed = TRUE)[[1]]), 2L)
  expect_match(markup, 'id="admin-users_table"', fixed = TRUE)
  expect_match(markup, 'id="admin-ict_dir"', fixed = TRUE)
  expect_match(markup, 'id="admin-save_ict_dir"', fixed = TRUE)
  expect_match(markup, 'id="admin-edge_dir"', fixed = TRUE)
  expect_match(markup, 'id="admin-save_edge_dir"', fixed = TRUE)
  expect_false(grepl("width: 500px", markup, fixed = TRUE))
  expect_false(grepl("padding-top: 31px", markup, fixed = TRUE))
})

test_that("reporting filters expose responsive layout hooks", {
  suppressPackageStartupMessages(library(shiny))

  module_env <- ui_module_environment("R/modules/reporting_mod.R")
  markup <- render_ui_markup(
    module_env$edge_cost_events_filter_controls(NS("report"), "components")
  )

  expect_match(markup, "rids-reporting-filters", fixed = TRUE)
  expect_match(markup, "rids-reporting-action", fixed = TRUE)
  expect_match(markup, 'id="report-run_report"', fixed = TRUE)
  expect_false(grepl("align-self: flex-end", markup, fixed = TRUE))
})

test_that("populated data tables expose labelled keyboard regions", {
  suppressPackageStartupMessages({
    library(shiny)
    library(bs4Dash)
    library(reactable)
    library(shinyjs)
  })

  modules <- list(
    list(path = "R/modules/step2_mod.R", function_name = "step2_UI", id = "step2"),
    list(path = "R/modules/step3_mod.R", function_name = "step3_UI", id = "step3"),
    list(path = "R/modules/reporting_mod.R", function_name = "reportingUI", id = "report"),
    list(path = "R/modules/edge_builder_mod.R", function_name = "edgeBuilderUI", id = "builder"),
    list(path = "R/modules/custom_activity_module.R", function_name = "customActivityUI", id = "custom")
  )

  for (module in modules) {
    module_env <- ui_module_environment(module$path)
    markup <- render_ui_markup(module_env[[module$function_name]](module$id))
    expect_match(markup, "rids-table-region", fixed = TRUE, info = module$path)
    expect_match(markup, 'role="region"', fixed = TRUE, info = module$path)
    expect_match(markup, "aria-label=", fixed = TRUE, info = module$path)
    expect_false(
      grepl("rids-table-region[^>]*tabindex", markup),
      info = module$path
    )
  }

  step4_source <- paste(
    readLines(file.path(rids_repo_root(), "R/modules/step4_mod.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(step4_source, 'aria-label` = "EDGE template preview table"', fixed = TRUE)
  expect_match(step4_source, "rids-step4-save-path", fixed = TRUE)
})

library_test_data <- function(populated = FALSE) {
  empty <- data.frame(
    cpms_id = character(),
    study_site = character(),
    scenario_id = character(),
    study_name = character(),
    edge_id = character(),
    speciality_name = character(),
    uploaded_by = character(),
    upload_timestamp = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )

  if (!populated) {
    return(empty)
  }

  data.frame(
    cpms_id = "12345",
    study_site = "RDUHT",
    scenario_id = "A",
    study_name = "Responsive Study",
    edge_id = "EDGE-42",
    speciality_name = "Cardiology",
    uploaded_by = "Test User",
    upload_timestamp = as.POSIXct("2026-07-16 09:00:00", tz = "UTC"),
    stringsAsFactors = FALSE
  )
}

test_that("library empty state uses the shared empty-state component", {
  suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
  })

  module_env <- ui_module_environment("R/modules/library_mod.R")
  module_env$rids_repos <- function() {
    list(studies = list(list_studies = function() library_test_data(FALSE)))
  }
  auth_state <- reactiveValues(logged_in = TRUE, user_id = 1L)
  shared_state <- reactiveValues(library_refresh = 0L, current_study = NULL)

  testServer(
    module_env$libraryServer,
    args = list(auth_state = auth_state, shared_state = shared_state),
    {
      session$flushReact()
      markup <- output$study_cards$html
      expect_match(markup, "rids-empty-state rids-library-empty", fixed = TRUE)
      expect_match(markup, "rids-empty-icon", fixed = TRUE)
      expect_match(markup, "No studies are available in the library yet.", fixed = TRUE)
    }
  )
})

test_that("populated library uses the responsive study grid", {
  suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
  })

  module_env <- ui_module_environment("R/modules/library_mod.R")
  module_env$rids_repos <- function() {
    list(studies = list(list_studies = function() library_test_data(TRUE)))
  }
  auth_state <- reactiveValues(logged_in = TRUE, user_id = 1L)
  shared_state <- reactiveValues(library_refresh = 0L, current_study = NULL)

  testServer(
    module_env$libraryServer,
    args = list(auth_state = auth_state, shared_state = shared_state),
    {
      session$setInputs(
        search = "",
        site_filter = "",
        speciality_filter = "",
        uploaded_by_filter = "",
        sort_by = "newest"
      )
      session$flushReact()
      markup <- output$study_cards$html
      expect_match(markup, "rids-library-grid", fixed = TRUE)
      expect_match(markup, "rids-study-card", fixed = TRUE)
      expect_match(markup, 'id="[^"]+-open_study_1"')
      expect_match(markup, 'id="[^"]+-delete_study_1"')
      expect_false(grepl("grid-template-columns: repeat(3, 1fr)", markup, fixed = TRUE))
    }
  )
})

test_that("study metadata helper uses a stackable key-value contract", {
  suppressPackageStartupMessages(library(shiny))

  module_env <- ui_module_environment("R/modules/study_workspace_mod.R")
  markup <- render_ui_markup(module_env$study_workspace_kv("CPMS ID", "12345"))

  expect_match(markup, "rids-study-kv", fixed = TRUE)
  expect_match(markup, "rids-study-kv-label", fixed = TRUE)
  expect_match(markup, "rids-study-kv-value", fixed = TRUE)
  expect_match(markup, "CPMS ID", fixed = TRUE)
  expect_match(markup, "12345", fixed = TRUE)
  expect_false(grepl("grid-template-columns: 180px 1fr", markup, fixed = TRUE))
})

test_that("shared polish styles retain accessibility and semantic contracts", {
  styles <- paste(
    readLines(file.path(rids_repo_root(), "www/styles.css"), warn = FALSE),
    collapse = "\n"
  )
  workspace_source <- paste(
    readLines(
      file.path(rids_repo_root(), "R/modules/study_workspace_mod.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(styles, ".ca-mode-switch-input:focus-visible", fixed = TRUE)
  expect_match(styles, "--rids-focus: 0 0 0 0.2rem rgba(31, 98, 136, 0.14)", fixed = TRUE)
  expect_match(styles, "outline: 3px solid rgba(31, 98, 136, 0.72)", fixed = TRUE)
  expect_false(grepl("a, button, input, select, textarea", styles, fixed = TRUE))
  expect_false(grepl("body.app-shell .selectize-input.focus", styles, fixed = TRUE))
  expect_false(grepl(".reactable .rt-select-input:focus-visible", styles, fixed = TRUE))
  expect_match(styles, ":not(textarea):not(.selectize-input)", fixed = TRUE)
  expect_match(styles, "body.app-shell .checkbox-inline", fixed = TRUE)
  expect_match(styles, "body.app-shell .checkbox label > input[type=\"checkbox\"]", fixed = TRUE)
  expect_match(styles, "gap: 0.5rem", fixed = TRUE)
  expect_match(styles, ".reactable .rt-select-input:checked", fixed = TRUE)
  expect_match(styles, ".rids-interactive-table .rt-tbody .rt-td", fixed = TRUE)
  expect_match(styles, ".rids-edge-template-link", fixed = TRUE)
  expect_match(styles, ".rids-step4-save-path", fixed = TRUE)
  expect_match(styles, ".rids-version-card.is-archived", fixed = TRUE)
  expect_match(styles, ".rids-matrix-upload-step small", fixed = TRUE)
  expect_match(styles, ".rids-filter-bar .form-control", fixed = TRUE)
  expect_match(styles, "#login-overlay code", fixed = TRUE)
  expect_match(styles, ".btn-default.btn-outline-danger", fixed = TRUE)
  expect_match(styles, ".modal-footer > .shiny-html-output", fixed = TRUE)
  expect_false(grepl("opacity: 0.68", workspace_source, fixed = TRUE))

  for (path in c(
    "R/modules/custom_activity_module.R",
    "R/modules/edge_builder_mod.R",
    "R/modules/step4_mod.R",
    "R/modules/study_workspace_mod.R"
  )) {
    source <- paste(
      readLines(file.path(rids_repo_root(), path), warn = FALSE),
      collapse = "\n"
    )
    expect_false(grepl("style\\s*=", source), info = path)
  }
})
