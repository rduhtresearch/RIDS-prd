settingsUI <- function(id) {
  ns <- NS(id)

  div(
    class = "rids-page rids-form-page",
    div(class = "rids-page-header", div(div(class = "rids-page-eyebrow", "Account"), h1("Settings"), p("Manage your profile security and account details.")), div(class = "rids-page-mark", icon("cog"))),
    fluidRow(
      bs4Card(
        title = tagList(icon("key"), " Change password"),
        width = 6,
        status = "primary",
        solidHeader = FALSE,
        passwordInput(ns("current_password"), "Current password"),
        passwordInput(ns("new_password"), "New password"),
        passwordInput(ns("confirm_password"), "Confirm new password"),
        actionButton(ns("change_password"), "Update password", class = "btn-primary")
      ),
      bs4Card(
        title = tagList(icon("user-circle"), " Account details"),
        width = 6,
        status = "primary",
        solidHeader = FALSE,
        uiOutput(ns("account_summary"))
      )
    )
  )
}

settingsServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
    output$account_summary <- renderUI({
      req(auth_state$logged_in)

      tagList(
        tags$p(tags$strong("Name: "), auth_state$name %||% "Not set"),
        tags$p(tags$strong("Username: "), auth_state$username %||% ""),
        tags$p(tags$strong("Role: "), tools::toTitleCase(auth_state$role %||% "user")),
        tags$p(
          class = "rids-form-copy mb-0",
          "Password changes take effect immediately for your account."
        )
      )
    })

    observeEvent(input$change_password, {
      req(auth_state$logged_in)

      if (nchar(input$new_password) < 8) {
        feedbackDanger("new_password", show = TRUE, text = "Minimum 8 characters")
        return()
      }

      if (!identical(input$new_password, input$confirm_password)) {
        feedbackDanger("confirm_password", show = TRUE, text = "Passwords do not match")
        return()
      }

      result <- auth_state$change_password(
        current_password = input$current_password,
        new_password = input$new_password
      )

      if (!isTRUE(result$success)) {
        feedbackDanger("current_password", show = TRUE, text = result$message %||% "Password change failed.")
        showNotification(result$message %||% "Password change failed.", type = "warning", duration = 5)
        return()
      }

      updateTextInput(session, "current_password", value = "")
      updateTextInput(session, "new_password", value = "")
      updateTextInput(session, "confirm_password", value = "")
      showNotification("Password updated.", type = "message", duration = 5)
    })
  })
}
