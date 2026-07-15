integrationsUI <- function(id) {
  ns <- NS(id)

  div(
    class = "rids-page rids-form-page",
    div(class = "rids-page-header", div(div(class = "rids-page-eyebrow", "Connections"), h1("Integrations"), p("Connect RIDS securely to external research systems.")), div(class = "rids-page-mark", icon("plug"))),
    fluidRow(
      bs4Card(
        title = tagList(icon("link"), " EDGE integration"),
        width = 8,
        status = "primary",
        solidHeader = FALSE,
        div(
          style = "display: flex; flex-direction: column; gap: 1rem;",
          uiOutput(ns("edge_status")),
          passwordInput(ns("edge_api_key"), "EDGE API key"),
          div(
            style = "display: flex; gap: 0.75rem; flex-wrap: wrap;",
            actionButton(ns("save_edge_api_key"), "Save API key", class = "btn-primary"),
            actionButton(ns("delete_edge_api_key"), "Delete saved key", class = "btn-outline-danger")
          ),
          div(
            style = "color: #697786; font-size: 0.92rem;",
            "Your EDGE API key is stored for your account only and is never shown again after saving."
          )
        )
      ),
      bs4Card(
        title = tagList(icon("info-circle"), " How it works"),
        width = 4,
        status = "white",
        solidHeader = FALSE,
        div(
          style = "display: flex; flex-direction: column; gap: 0.75rem; color: #1d2a36;",
          tags$p(style = "margin: 0;", "Use this page to connect your RIDS account to EDGE with your own API key."),
          tags$p(style = "margin: 0;", "RIDS stores the key securely for your account and will use it later when EDGE workflow actions are added."),
          tags$p(style = "margin: 0; color: #697786;", "If you remove the key, future EDGE actions will prompt you to add it again.")
        )
      )
    )
  )
}

integrationsServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
    edge_refresh <- reactiveVal(0L)
    if (is.null(session$userData$edge_credential_refresh)) {
      session$userData$edge_credential_refresh <- reactiveVal(0L)
    }

    edge_status <- reactive({
      req(auth_state$logged_in)
      edge_refresh()
      get_user_api_credential_status(auth_state$user_id, "edge")
    })

    output$edge_status <- renderUI({
      status <- edge_status()

      if (!isTRUE(status$configured)) {
        return(
          div(
            style = "padding: 0.8rem 0.9rem; background: #f7f9fc; border-radius: 6px; color: #697786;",
            tags$strong(style = "color: #1d2a36;", "Status: "),
            "No EDGE API key saved for your account."
          )
        )
      }

      updated_label <- ""
      if (!is.null(status$updated_at) && !is.na(status$updated_at)) {
        updated_label <- paste("Last updated", format(as.POSIXct(status$updated_at), "%Y-%m-%d %H:%M"))
      }

      div(
        style = "padding: 0.8rem 0.9rem; background: #f1f8ef; border-radius: 6px;",
        div(
          style = "font-weight: 600; color: #2e7d32;",
          "Configured"
        ),
        div(
          style = "margin-top: 0.25rem; color: #1d2a36;",
          paste("Saved key:", status$masked_secret)
        ),
        if (nzchar(updated_label)) {
          div(
            style = "margin-top: 0.25rem; color: #697786;",
            updated_label
          )
        }
      )
    })

    observeEvent(input$save_edge_api_key, {
      req(auth_state$logged_in)

      api_key <- trimws(input$edge_api_key %||% "")
      if (!nzchar(api_key)) {
        feedbackDanger("edge_api_key", show = TRUE, text = "Enter your EDGE API key.")
        return()
      }

      result <- save_user_api_credential(auth_state$user_id, "edge", api_key)
      if (!isTRUE(result$success)) {
        feedbackDanger("edge_api_key", show = TRUE, text = result$message %||% "Unable to save the API key.")
        showNotification(result$message %||% "Unable to save the API key.", type = "error")
        return()
      }

      updateTextInput(session, "edge_api_key", value = "")
      edge_refresh(edge_refresh() + 1L)
      session$userData$edge_credential_refresh(session$userData$edge_credential_refresh() + 1L)
      showNotification("EDGE API key saved.", type = "message", duration = 5)
    })

    observeEvent(input$delete_edge_api_key, {
      req(auth_state$logged_in)

      showModal(modalDialog(
        title = "Delete EDGE API key?",
        "This will remove the saved API key from your account.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_delete_edge_api_key"), "Delete key", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_delete_edge_api_key, {
      req(auth_state$logged_in)

      removeModal()
      deleted <- delete_user_api_credential(auth_state$user_id, "edge")

      if (!isTRUE(deleted)) {
        showNotification("No saved EDGE API key was found.", type = "warning", duration = 5)
        return()
      }

      updateTextInput(session, "edge_api_key", value = "")
      edge_refresh(edge_refresh() + 1L)
      session$userData$edge_credential_refresh(session$userData$edge_credential_refresh() + 1L)
      showNotification("EDGE API key deleted.", type = "message", duration = 5)
    })
  })
}
