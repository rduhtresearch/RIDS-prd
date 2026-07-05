appUI <- function(id) {
  app_version_label <- get0("APP_VERSION_LABEL", ifnotfound = "v1.0.0")

  tagList(
    progressUI(NS(id, "progress")),
    tabItems(
      tagAppendAttributes(
        tabItem(
          "tab_dashboard",
          div(
            style = paste(
              "min-height: calc(100vh - 12rem);",
              "display: flex;",
              "align-items: center;",
              "justify-content: center;",
              "padding: 2rem 1rem;"
            ),
            div(
              style = paste(
                "width: 100%;",
                "max-width: 52rem;"
              ),
              bs4Card(
                width = 12,
                status = "white",
                solidHeader = FALSE,
                collapsible = FALSE,
                title = tagList(
                  "Welcome to RIDS ",
                  span(
                    style = "font-size: 0.85rem; color: #697786; font-weight: 400;",
                    app_version_label
                  )
                ),
                div(
                  style = "padding: 0.5rem 0.25rem 0.25rem;",
                  p(
                    style = "margin-bottom: 0.75rem; color: #1d2a36; font-size: 1.05rem; font-weight: 600;",
                    "Research Income Distribution System"
                  ),
                  p(
                    style = "margin-bottom: 0.75rem; color: #697786;",
                    "Developed by the Research & Development Department at Royal Devon University Healthcare NHS Foundation Trust."
                  ),
                  p(
                    style = "margin-bottom: 0.75rem; color: #697786;",
                    "A rules-driven commercial research finance platform that applies AcoRD-based distribution logic to iCT costings, transforming them into consistent income distribution outputs, automated reporting, and EDGE-ready templates, with configurable rules to reflect local R&D finance policy and operational requirements."
                  ),
                  p(
                    style = "margin-bottom: 0; color: #697786; font-weight: 600;",
                    "Use the sidebar to get started."
                  )
                )
              )
            )
          )
        ),
        class = "active"
      ),
      tabItem("tab_reporting", reportingUI(NS(id, "reporting"))),
      tabItem("tab_settings",  settingsUI(NS(id, "settings"))),
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
      filename        = NULL,
      upload_meta     = NULL,
      raw_ict         = NULL,
      posting_plan    = NULL,
      processed_ict   = NULL,
      edge_templates  = NULL,
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
      shared_state$filename <- NULL
      shared_state$upload_meta <- NULL
      shared_state$raw_ict <- NULL
      shared_state$posting_plan <- NULL
      shared_state$processed_ict <- NULL
      shared_state$edge_templates <- NULL
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
    progressServer("progress", current_step)
    reportingServer("reporting", auth_state)
    settingsServer("settings", auth_state)
    integrationsServer("integrations", auth_state)
    libraryServer("library", auth_state, shared_state)
    supportServer("support", auth_state)
    studyWorkspaceServer("study_workspace", shared_state)

    output$admin_tab <- renderUI({
      if (!isTRUE(is_admin(auth_state$role))) {
        return(
          div(
            style = "padding: 1.5rem; color: #697786;",
            "Admin access is required for this area."
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
