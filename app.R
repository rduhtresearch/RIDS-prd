source("R/setup.r", local = FALSE)
source("global.R", local = FALSE)
rids_source_modules()

ui <- tagList(
  tags$head(
    tags$title(APP_TITLE),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = sprintf("styles.css?v=%s", as.integer(file.info("www/styles.css")$mtime))
    ),
    tags$script(src = "app-shell.js")
  ),
  dashboardPage(
    dark = NULL,
    help = NULL,
    header = dashboardHeader(title = dashboardBrand(title = span(
      class = "rids-header-brand",
      span(class = "rids-header-brand-mark", icon("layer-group")),
      span("RIDS"),
      span(class = "rids-header-version", APP_VERSION_LABEL)
    )), rightUi = uiOutput("user_badge")),
    sidebar = dashboardSidebar(
      skin = "light",
      collapsed = FALSE,
      minified = FALSE,
      expandOnHover = FALSE,
      fixed = TRUE,
      width = 250,
      sidebarUI("sidebar")
    ),
    body = dashboardBody(
      useWaiter() ,
      useShinyjs(),
      dev_banner(),
      useShinyFeedback(),
      loginUI("login"),
      appUI("app"),
      div(
        class = "rids-build-badge",
        paste0(APP_VERSION_LABEL, " · last updated ", APP_LAST_UPDATED)
      )
    ) 
  )
)

# Clean shutdown on last session end (live deployments only).
# Each laptop runs its own R process serving a single user; when the last browser
# session goes away we let runApp() return so the existing onStop() handler in
# global.R checkpoints DuckDB cleanly (folding the WAL back into the main file)
# instead of relying on the user hard-killing the launcher terminal. A short grace
# window tolerates page refreshes, which briefly drop to zero sessions before the
# browser reconnects.
.rids_session_tracker <- new.env(parent = emptyenv())
.rids_session_tracker$count <- 0L
RIDS_SHUTDOWN_GRACE_SECONDS <- suppressWarnings(
  as.numeric(Sys.getenv("RIDS_SHUTDOWN_GRACE_SECONDS", "25"))
)
if (is.na(RIDS_SHUTDOWN_GRACE_SECONDS) || RIDS_SHUTDOWN_GRACE_SECONDS < 0) {
  RIDS_SHUTDOWN_GRACE_SECONDS <- 25
}

server <- function(input, output, session) {
  .rids_session_tracker$count <- .rids_session_tracker$count + 1L

  activate_dashboard_tab <- function() {
    updateTabItems(session, "sidebar", selected = "tab_dashboard")
    shinyjs::runjs('$("[data-value=\'tab_dashboard\']").tab("show")')
  }
  
  session$onSessionEnded(function() {
    app_log_info("session", "Browser session ended")
    .rids_session_tracker$count <- .rids_session_tracker$count - 1L

    if (identical(APP_STATUS, "live") && .rids_session_tracker$count <= 0L) {
      later::later(function() {
        # Re-check after the grace window: a refresh will have reconnected and
        # bumped the count back above zero, so we only stop when nobody is left.
        if (.rids_session_tracker$count <= 0L) {
          app_log_info("shutdown", "No active sessions remaining; stopping app for clean DuckDB shutdown")
          shiny::stopApp()
        }
      }, delay = RIDS_SHUTDOWN_GRACE_SECONDS)
    }
  })
  
  output$user_badge <- renderUI({
    req(auth_state$logged_in)
    
    div(
      class = "rids-user-badge",
      span(
        class = "rids-user-name",
        auth_state$name %||% auth_state$username
      ),
      span(
        class = paste("rids-role-chip", if (isTRUE(is_admin(auth_state$role))) "is-admin" else ""),
        style = sprintf(
          "background: %s; color: %s;",
          if (isTRUE(is_admin(auth_state$role))) "#e8f4fd" else "#f0f4f8",
          if (isTRUE(is_admin(auth_state$role))) "#1f5f8b" else "#6c757d"
        ),
        tools::toTitleCase(auth_state$role %||% "user")
      )
    )
  })
  
  pipeline_tabs <- c("tab_step1", "tab_step2", "tab_step3", "tab_step4")
  current_step <- reactiveVal(NULL)
  auth_state <- loginServer("login")

  observe({
    current <- input$sidebar
    if (!is.null(current) && current %in% pipeline_tabs) {
      current_step(gsub("tab_", "", current))
    } else {
      current_step(NULL)
    }
  })

  observe({
    if (!isTRUE(is_admin(auth_state$role)) && identical(input$sidebar, "tab_admin")) {
      activate_dashboard_tab()
    }
  })

  observe({
    session$sendCustomMessage(
      "setAppShell",
      isTRUE(auth_state$auth_ready) &&
        isTRUE(auth_state$logged_in) &&
        !isTRUE(auth_state$must_change_password)
    )
  })
  
  observe({
    if (!isTRUE(auth_state$auth_ready)) {
      shinyjs::hide("login-overlay")
      return()
    }

    if (isTRUE(auth_state$logged_in) && !isTRUE(auth_state$must_change_password)) {
      shinyjs::hide("login-overlay")
      activate_dashboard_tab()
    } else {
      shinyjs::show("login-overlay")
      if (!isTRUE(auth_state$logged_in)) {
        activate_dashboard_tab()
      }
    }
  })
  
  sidebarServer("sidebar", auth_state, session, current_step)
  appServer("app", auth_state, current_step)
  adminServer("admin", auth_state)
}

shinyApp(ui, server)
