appUI <- function(id) {
  app_version_label <- get0("APP_VERSION_LABEL", ifnotfound = "v1.0.0")

  tagList(
    progressUI(NS(id, "progress")),
    tabItems(
      tagAppendAttributes(
        tabItem(
          "tab_dashboard",
          div(
            class = "rids-page rids-dashboard",
            div(
              class = "rids-dashboard-hero",
              div(class = "rids-dashboard-mark", icon("layer-group")),
              div(class = "rids-page-eyebrow", "Research operations platform"),
              h1(
                "Welcome to RIDS",
                span(class = "rids-version-chip", app_version_label)
              ),
              p(
                class = "rids-dashboard-lead",
                "Research Income Distribution System"
              ),
              p(
                "A rules-driven commercial research finance platform that applies AcoRD-based distribution logic to ICT costings, creating consistent income distribution outputs, automated reporting and EDGE-ready templates."
              ),
              div(
                class = "rids-dashboard-callout",
                div(class = "rids-dashboard-callout-icon", icon("arrow-left")),
                div(
                  strong("Start a workflow"),
                  span("Use Process ICT in the sidebar to upload a workbook and begin costing.")
                ),
                div(class = "rids-dashboard-callout-status", icon("shield-alt"), " Validated workflow")
              ),
              p(
                class = "rids-dashboard-credit",
                "Developed by the Research & Development Department at Royal Devon University Healthcare NHS Foundation Trust."
              )
            )
          )
        ),
        class = "active"
      ),
      tabItem("tab_reporting", reportingUI(NS(id, "reporting"))),
      tabItem("tab_settings",  settingsUI(NS(id, "settings"))),
      tabItem("tab_cost_centre_matrix", costCentreMatrixUI(NS(id, "cost_centre_matrix"))),
      tabItem("tab_integrations", integrationsUI(NS(id, "integrations"))),
      tabItem("tab_library",   libraryUI(NS(id, "library"))),
      tabItem("tab_study",     studyWorkspaceUI(NS(id, "study_workspace"))),
      tabItem("tab_support",   supportUI(NS(id, "support"))),
      tabItem("tab_step1", step1_UI(NS(id, "step1"))),
      tabItem("tab_step2", step2_UI(NS(id, "step2"))),
      tabItem("tab_step3", step3_UI(NS(id, "step3"))),
      tabItem("tab_step4", step4_UI(NS(id, "step4"))),
      tabItem("tab_admin", uiOutput(NS(id, "admin_tab")))
    )
  )
}

appServer <- function(id, auth_state, current_step) {
  moduleServer(id, function(input, output, session) {
    shared_state <- reactiveValues(
      scenario_id     = NULL,
      study_site      = NULL,
      edge_id         = NULL,
      cpms_id         = NULL,
      study_name      = NULL,
      upload_id       = NULL,
      template_version_id = NULL,
      template_version_number = NULL,
      template_version_type = NULL,
      template_version_effective_date = NULL,
      filename        = NULL,
      upload_meta     = NULL,
      raw_ict         = NULL,
      posting_plan    = NULL,
      evaluated_plan  = NULL,
      processed_ict   = NULL,
      edge_templates  = NULL,
      speciality_id   = NULL,
      speciality_name = NULL,
      mff_split_enabled = FALSE,
      mff_split_pct   = 0,
      include_screening_failure = FALSE,
      screening_failure_arm = NULL,
      current_step    = NULL,
      timestamp       = NULL,
      current_study   = NULL,
      library_refresh = 0L
    )

    session$userData$reset_app_state <- function(reset_library_refresh = TRUE) {
      current_library_refresh <- isolate(shared_state$library_refresh)
      if (is.null(current_library_refresh) || is.na(current_library_refresh)) {
        current_library_refresh <- 0L
      }

      shared_state$scenario_id <- NULL
      shared_state$study_site <- NULL
      shared_state$edge_id <- NULL
      shared_state$cpms_id <- NULL
      shared_state$study_name <- NULL
      shared_state$upload_id <- NULL
      shared_state$template_version_id <- NULL
      shared_state$template_version_number <- NULL
      shared_state$template_version_type <- NULL
      shared_state$template_version_effective_date <- NULL
      shared_state$filename <- NULL
      shared_state$upload_meta <- NULL
      shared_state$raw_ict <- NULL
      shared_state$posting_plan <- NULL
      shared_state$evaluated_plan <- NULL
      shared_state$processed_ict <- NULL
      shared_state$edge_templates <- NULL
      shared_state$speciality_id <- NULL
      shared_state$speciality_name <- NULL
      shared_state$mff_split_enabled <- FALSE
      shared_state$mff_split_pct <- 0
      shared_state$include_screening_failure <- FALSE
      shared_state$screening_failure_arm <- NULL
      shared_state$current_step <- NULL
      shared_state$timestamp <- NULL
      shared_state$current_study <- NULL
      shared_state$library_refresh <- if (isTRUE(reset_library_refresh)) 0L else current_library_refresh
      current_step(NULL)
    }
    
    step1_Server("step1", auth_state, shared_state, current_step)
    step2_Server("step2", auth_state, shared_state, current_step)
    step3_Server("step3", auth_state, shared_state, current_step)
    step4_Server("step4", auth_state, shared_state, current_step)
    progressServer("progress", current_step, shared_state)
    reportingServer("reporting", auth_state)
    settingsServer("settings", auth_state)
    costCentreMatrixServer("cost_centre_matrix", auth_state)
    integrationsServer("integrations", auth_state)
    libraryServer("library", auth_state, shared_state)
    supportServer("support", auth_state)
    studyWorkspaceServer("study_workspace", auth_state, shared_state, current_step)

    output$admin_tab <- renderUI({
      if (!isTRUE(is_admin(auth_state$role))) {
        return(
          div(
            class = "rids-page rids-empty-state",
            div(class = "rids-empty-icon", icon("lock")),
            h2("Admin access required"),
            p("Your account does not have permission to open this area.")
          )
        )
      }

      adminUI("admin")
    })
    
    observe({
      shared_state$current_step <- current_step()
    })
    
    
  })
}
