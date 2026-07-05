sidebarUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("menu"))
}

sidebarServer <- function(id, auth_state, parent_session, current_step) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    observeEvent(input$new_ict, {
      current_step("step1")
    })

    observeEvent(input$new_ict, {
      current_step("step1")
      shinyjs::runjs('$("[data-value=\'tab_step1\']").tab("show")')
      shinyjs::runjs("$('body').addClass('sidebar-collapse')")
    })
    
    # output$menu <- renderUI({
    #   req(auth_state$logged_in)
    #   sidebarMenu(
    #     id = "sidebar",
    #     actionButton(ns('new_ict'), 'Process ICT'),
    #     menuItem("Reporting", tabName = "tab_reporting",  icon = ionicon(name = "analytics")),
    #     menuItem("Study Library",  tabName = "tab_library",  icon = ionicon(name = "book")),
    #     menuItem("Settings",  tabName = "tab_settings",  icon = ionicon(name = "settings")),
    #     menuItem("Support",  tabName = "tab_support",  icon = ionicon(name = "help-buoy")),
    #     if (auth_state$role == "admin") {
    #       menuItem("Admin", tabName = "tab_admin", icon = icon("users-cog"))
    #     },
    #     
    #     # step / workflow menu items (Hidden)
    #     tags$div(
    #       style = "display:none",
    #       menuItem("ICT", tabName = "tab_step1", icon = icon("file")),
    #       menuItem("ICT Step 2", tabName = "tab_step2", icon = icon("file")),
    #       menuItem("ICT Step 3", tabName = "tab_step3", icon = icon("file")),
    #     ),
    #     
    #     actionButton(ns('logout'), 'Logout')
    #   )
    # })
    
    output$menu <- renderUI({
      req(auth_state$logged_in)
      
      tagList(
        div(
          class = "rids-sidebar-shell",
          div(
            class = "rids-sidebar-top",
            actionButton(
              ns('new_ict'),
              label = tagList(icon("upload"), " Process ICT"),
              class = "rids-sidebar-primary"
            )
          ),
          
          div(
            class = "rids-sidebar-nav",
            div(class = "rids-sidebar-section-label", "Work"),
            sidebarMenu(
              id = "sidebar",
              menuItem("Reporting",     tabName = "tab_reporting", icon = icon("chart-line")),
              menuItem("Study Library", tabName = "tab_library",   icon = icon("book-open")),
              menuItem("Settings",      tabName = "tab_settings",  icon = icon("cog")),
              menuItem("Integrations",  tabName = "tab_integrations", icon = icon("plug")),
              menuItem("Support",       tabName = "tab_support",   icon = icon("life-ring")),
              if (isTRUE(is_admin(auth_state$role))) {
                tagList(
                  tags$li(class = "rids-sidebar-section-label nav-item", "Admin"),
                  menuItem("Admin", tabName = "tab_admin", icon = icon("users-cog"))
                )
              },
              tagAppendAttributes(
                menuItem("Home", tabName = "tab_dashboard", icon = icon("home"), selected = TRUE),
                style = "display:none;"
              ),
              tagAppendAttributes(
                menuItem("Study Workspace", tabName = "tab_study", icon = icon("folder-open")),
                style = "display:none;"
              ),
              tagAppendAttributes(
                menuItem("ICT", tabName = "tab_step1", icon = icon("file")),
                style = "display:none;"
              ),
              tagAppendAttributes(
                menuItem("ICT Step 2", tabName = "tab_step2", icon = icon("file")),
                style = "display:none;"
              ),
              tagAppendAttributes(
                menuItem("ICT Step 3", tabName = "tab_step3", icon = icon("file")),
                style = "display:none;"
              ),
              tagAppendAttributes(
                menuItem("ICT Step 4", tabName = "tab_step4", icon = icon("file")),
                style = "display:none;"
              )
            )
          ),
          
          div(
            class = "rids-sidebar-bottom",
            div(class = "rids-sidebar-section-label", "Account"),
            actionButton(
              ns('logout'),
              label = tagList(icon("sign-out-alt"), " Logout"),
              class = "rids-sidebar-logout"
            )
          )
        )
      )
    })
    
    observeEvent(input$logout, {
      if (is.function(auth_state$logout)) {
        auth_state$logout()
      } else {
        auth_state$logged_in <- FALSE
      }
    })

    observeEvent(input$new_ict, {
      shinyjs::runjs('$("[data-value=\'tab_step1\']").tab("show")')
      shinyjs::runjs("$('body').addClass('sidebar-collapse')")
    })
    
  })
}
