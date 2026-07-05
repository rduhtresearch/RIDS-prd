adminUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      bs4Card(
        title = "User Management",
        width = 5,
        status = "primary",
        textInput(ns("user_name"), "Name"),
        textInput(ns("user_username"), "Username"),
        textInput(ns("user_email"), "Email (optional)"),
        selectInput(ns("user_role"), "Role", choices = c("user", "developer", "admin")),
        checkboxInput(ns("user_active"), "Active", value = TRUE),
        passwordInput(ns("temporary_password"), "Temporary password"),
        div(
          style = "display: flex; gap: 0.75rem; flex-wrap: wrap;",
          actionButton(ns("save_user"), "Save User", class = "btn-primary"),
          actionButton(ns("clear_form"), "New User", class = "btn-secondary"),
          actionButton(ns("toggle_active"), "Deactivate / Reactivate", class = "btn-outline-danger")
        ),
        hr(),
        passwordInput(ns("reset_password_value"), "Reset password to"),
        actionButton(ns("reset_password"), "Reset Password", class = "btn-outline-primary"),
        hr(),
        verbatimTextOutput(ns("password_notice"))
      ),
      bs4Card(
        title = "Users",
        width = 7,
        status = "primary",
        reactableOutput(ns("users_table"))
      )
    ),
    fluidRow(
      bs4Card(
        title = "Settings",
        width = 12,
        status = "primary",
        div(
          style = "display: flex; flex-direction: column; gap: 1.5rem;",
          div(
            style = "display: flex; gap: 1rem;",
            div(
              style = "width: 500px;",
              textInput(ns("ict_dir"), "ICT Upload Directory", value = ICT_UPLOAD_DIR, width = "100%")
            ),
            div(
              style = "padding-top: 31px;",
              actionButton(ns("save_ict_dir"), "Save", class = "btn-primary")
            )
          ),
          div(
            style = "display: flex; gap: 1rem;",
            div(
              style = "width: 500px;",
              textInput(ns("edge_dir"), "EDGE Output Directory", value = EDGE_OUTPUT_DIR, width = "100%")
            ),
            div(
              style = "padding-top: 31px;",
              actionButton(ns("save_edge_dir"), "Save", class = "btn-primary")
            )
          ),
          hr(style = "margin: 0;"),
          div(
            style = "display: flex; flex-direction: column; gap: 0.85rem;",
            div(
              style = "font-weight: 600; color: #1d2a36;",
              "Cost Centre Matrix"
            ),
            uiOutput(ns("cost_centre_matrix_current")),
            fileInput(
              ns("cost_centre_matrix_upload"),
              "Upload matrix CSV",
              accept = c(".csv")
            ),
            uiOutput(ns("cost_centre_matrix_validation")),
            div(
              style = "display: flex; gap: 1rem; align-items: center;",
              actionButton(ns("save_cost_centre_matrix"), "Save matrix", class = "btn-primary")
            )
          )
        )
      )
    ),
    fluidRow(
      bs4Card(
        title = "App Log Files",
        width = 12,
        status = "primary",
        uiOutput(ns("log_files"))
      )
    )
  )
}

adminServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
    refresh <- reactiveVal(0)
    selected_user_id <- reactiveVal(NULL)
    password_notice <- reactiveVal("")

    reset_form <- function() {
      selected_user_id(NULL)
      updateTextInput(session, "user_name", value = "")
      updateTextInput(session, "user_username", value = "")
      updateTextInput(session, "user_email", value = "")
      updateSelectInput(session, "user_role", selected = "user")
      updateCheckboxInput(session, "user_active", value = TRUE)
      updateTextInput(session, "temporary_password", value = "")
      updateTextInput(session, "reset_password_value", value = "")
    }

    current_users <- reactive({
      refresh()
      list_users_for_admin()
    })

    current_log_files <- reactive({
      invalidateLater(5000, session)
      prune_app_run_log_files(retention_hours = 24L)
      list_app_run_log_files()
    })

    current_cost_centre_matrix <- reactive({
      refresh()
      list(
        file_path = cc_get_setting("cost_centre_matrix_file", "")
      )
    })

    output$cost_centre_matrix_current <- renderUI({
      current <- current_cost_centre_matrix()

      if (!nzchar(current$file_path)) {
        return(
          div(
            style = "color: #697786;",
            "No cost centre matrix is currently configured."
          )
        )
      }

      validation <- validate_cost_centre_matrix_file(current$file_path)

      div(
        style = "padding: 0.75rem 0.9rem; background: #f7f9fc; border-radius: 6px;",
        div(style = "font-weight: 600; color: #1d2a36;", "Current active matrix"),
        div(style = "margin-top: 0.35rem; color: #697786;", paste("File:", current$file_path)),
        div(
          style = paste(
            "margin-top: 0.35rem;",
            if (isTRUE(validation$valid)) "color: #2e7d32;" else "color: #c0392b;"
          ),
          validation$message
        )
      )
    })

    output$cost_centre_matrix_validation <- renderUI({
      upload <- input$cost_centre_matrix_upload

      if (is.null(upload) || is.null(upload$datapath) || !file.exists(upload$datapath)) {
        return(
          div(
            style = "color: #697786;",
            "Upload a CSV file to validate it."
          )
        )
      }

      validation <- validate_cost_centre_matrix_file(upload$datapath)

      div(
        style = paste(
          "padding: 0.65rem 0.8rem;",
          "border-radius: 6px;",
          if (isTRUE(validation$valid)) "background: #f1f8ef; color: #2e7d32;" else "background: #fff4f2; color: #c0392b;"
        ),
        validation$message
      )
    })

    observeEvent(input$save_ict_dir, {
      req(input$ict_dir != "")

      tryCatch({
        dbExecute(
          CON,
          "UPDATE app_settings SET value = ? WHERE key = 'ict_upload_dir'",
          params = list(input$ict_dir)
        )

        ICT_UPLOAD_DIR <<- input$ict_dir
        log_event(
          level = "INFO",
          area = "admin",
          message = "Settings updated",
          user_id = auth_state$user_id,
          username = auth_state$username,
          session_id = auth_state$session_id,
          details = list(setting_key = "ict_upload_dir")
        )
        showNotification("Settings saved", type = "message", duration = 5)
      }, error = function(e) {
        app_log_exception("admin", "ICT upload directory save failed", e)
        showNotification("Failed to save settings", type = "error")
      })
    })

    observeEvent(input$save_edge_dir, {
      req(input$edge_dir != "")

      tryCatch({
        dbExecute(
          CON,
          "UPDATE app_settings SET value = ? WHERE key = 'edge_output_dir'",
          params = list(input$edge_dir)
        )
        EDGE_OUTPUT_DIR <<- input$edge_dir
        log_event(
          level = "INFO",
          area = "admin",
          message = "Settings updated",
          user_id = auth_state$user_id,
          username = auth_state$username,
          session_id = auth_state$session_id,
          details = list(setting_key = "edge_output_dir")
        )
        showNotification("Edge output directory saved", type = "message", duration = 5)
      }, error = function(e) {
        app_log_exception("admin", "EDGE output directory save failed", e)
        showNotification("Failed to save settings", type = "error")
      })
    })

    observeEvent(input$save_cost_centre_matrix, {
      req(isTRUE(is_admin(auth_state$role)))

      upload <- input$cost_centre_matrix_upload

      if (is.null(upload) || is.null(upload$datapath) || !file.exists(upload$datapath)) {
        showNotification("Upload a cost centre matrix CSV first.", type = "warning")
        return()
      }

      validation <- validate_cost_centre_matrix_file(upload$datapath)
      if (!isTRUE(validation$valid)) {
        showNotification(validation$message, type = "error", duration = 10)
        return()
      }

      tryCatch({
        matrix_dir <- file.path(ICT_UPLOAD_DIR, "cost_centre_matrices")
        if (!dir.exists(matrix_dir)) dir.create(matrix_dir, recursive = TRUE)

        saved_path <- file.path(matrix_dir, "active_cost_centre_matrix.csv")
        file.copy(upload$datapath, saved_path, overwrite = TRUE)

        dbExecute(
          CON,
          "UPDATE app_settings SET value = ? WHERE key = 'cost_centre_matrix_file'",
          params = list(saved_path)
        )

        log_event(
          level = "INFO",
          area = "admin",
          message = "Cost centre matrix updated",
          user_id = auth_state$user_id,
          username = auth_state$username,
          session_id = auth_state$session_id,
          details = list(
            setting_key = "cost_centre_matrix",
            file_path = saved_path
          )
        )

        showNotification("Cost centre matrix saved.", type = "message", duration = 5)
        refresh(refresh() + 1L)
      }, error = function(e) {
        app_log_exception("admin", "Cost centre matrix save failed", e)
        showNotification("Failed to save cost centre matrix", type = "error")
      })
    })

    observe({
      if (!isTRUE(is_admin(auth_state$role))) {
        shinyjs::runjs('$("[data-value=\'tab_dashboard\']").tab("show")')
      }
    })

    output$users_table <- renderReactable({
      users <- current_users()

      if (nrow(users) == 0) {
        users <- data.frame(
          user_id = integer(),
          name = character(),
          username = character(),
          email = character(),
          role = character(),
          active = logical(),
          force_password_change = logical(),
          created_at = character(),
          updated_at = character(),
          last_login_at = character(),
          stringsAsFactors = FALSE
        )
      }

      reactable(
        users,
        selection = "single",
        onClick = "select",
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        columns = list(
          user_id = colDef(name = "ID"),
          name = colDef(name = "Name"),
          username = colDef(name = "Username"),
          email = colDef(name = "Email"),
          role = colDef(name = "Role"),
          active = colDef(name = "Active"),
          force_password_change = colDef(name = "Force Change"),
          created_at = colDef(show = FALSE),
          updated_at = colDef(show = FALSE),
          last_login_at = colDef(name = "Last Login")
        )
      )
    })

    observeEvent(getReactableState("users_table", "selected"), {
      users <- current_users()
      selected <- getReactableState("users_table", "selected")

      if (is.null(selected) || selected < 1 || selected > nrow(users)) {
        return()
      }

      row <- users[selected, , drop = FALSE]
      selected_user_id(row$user_id[[1]])
      updateTextInput(session, "user_name", value = row$name[[1]] %||% "")
      updateTextInput(session, "user_username", value = row$username[[1]])
      updateTextInput(session, "user_email", value = row$email[[1]] %||% "")
      updateSelectInput(session, "user_role", selected = normalize_role(row$role[[1]]))
      updateCheckboxInput(session, "user_active", value = isTRUE(row$active[[1]]))
      updateTextInput(session, "temporary_password", value = "")
      updateTextInput(session, "reset_password_value", value = "")
      password_notice("")
    })

    observeEvent(input$clear_form, {
      password_notice("")
      reset_form()
    })

    observeEvent(input$save_user, {
      req(isTRUE(is_admin(auth_state$role)))

      if (trimws(input$user_username) == "") {
        feedbackDanger("user_username", show = TRUE, text = "Required")
        return()
      }

      user_id <- selected_user_id()

      if (is.null(user_id)) {
        if (nchar(input$temporary_password) < 8) {
          feedbackDanger("temporary_password", show = TRUE, text = "Minimum 8 characters")
          return()
        }

        result <- create_user_account(
          name = input$user_name,
          username = input$user_username,
          email = input$user_email,
          role = input$user_role,
          temporary_password = input$temporary_password,
          active = isTRUE(input$user_active),
          actor_user_id = auth_state$user_id
        )

        if (!isTRUE(result$success)) {
          showNotification(result$message %||% "Failed to create user.", type = "error")
          return()
        }

        password_notice(
          paste(
            "Temporary password (share once):",
            result$temporary_password
          )
        )
        showNotification("User created.", type = "message")
      } else {
        result <- update_user_account(
          user_id = user_id,
          name = input$user_name,
          username = input$user_username,
          email = input$user_email,
          role = input$user_role,
          active = isTRUE(input$user_active),
          actor_user_id = auth_state$user_id
        )

        if (!isTRUE(result$success)) {
          showNotification(result$message %||% "Failed to update user.", type = "error")
          return()
        }

        password_notice("")
        showNotification("User updated.", type = "message")
      }

      refresh(refresh() + 1)
      updateTextInput(session, "temporary_password", value = "")
    })

    observeEvent(input$toggle_active, {
      req(isTRUE(is_admin(auth_state$role)))
      req(!is.null(selected_user_id()))

      current_user <- get_user_by_id(selected_user_id())
      if (is.null(current_user)) {
        showNotification("User not found.", type = "warning")
        return()
      }

      if (identical(current_user$user_id[[1]], auth_state$user_id)) {
        showNotification("You cannot deactivate your own account.", type = "warning")
        return()
      }

      result <- set_user_active(
        user_id = current_user$user_id[[1]],
        active = !isTRUE(current_user$active[[1]]),
        actor_user_id = auth_state$user_id
      )

      if (!isTRUE(result$success)) {
        showNotification(result$message %||% "Failed to update user.", type = "error")
        return()
      }

      refresh(refresh() + 1)
      updateCheckboxInput(session, "user_active", value = isTRUE(result$user$active[[1]]))
      showNotification("User status updated.", type = "message")
    })

    observeEvent(input$reset_password, {
      req(isTRUE(is_admin(auth_state$role)))
      req(!is.null(selected_user_id()))

      if (nchar(input$reset_password_value) < 8) {
        feedbackDanger("reset_password_value", show = TRUE, text = "Minimum 8 characters")
        return()
      }

      result <- reset_user_password(
        user_id = selected_user_id(),
        temporary_password = input$reset_password_value,
        actor_user_id = auth_state$user_id
      )

      if (!isTRUE(result$success)) {
        showNotification(result$message %||% "Password reset failed.", type = "error")
        return()
      }

      refresh(refresh() + 1)
      password_notice(
        paste(
          "Temporary password reset (share once):",
          result$temporary_password
        )
      )
      updateTextInput(session, "reset_password_value", value = "")
      showNotification("Temporary password reset.", type = "message")
    })

    observe({
      files <- current_log_files()

      if (nrow(files) == 0) {
        return()
      }

      lapply(seq_len(nrow(files)), function(i) {
        local({
          row <- files[i, , drop = FALSE]
          output[[paste0("download_log_", i)]] <- downloadHandler(
            filename = function() {
              row$file_name[[1]]
            },
            content = function(file) {
              file.copy(row$file_path[[1]], file, overwrite = TRUE)
            }
          )
        })
      })
    })

    output$log_files <- renderUI({
      files <- current_log_files()

      if (nrow(files) == 0) {
        return(
          div(
            style = "color: #697786;",
            "No app log files found yet."
          )
        )
      }

      div(
        style = paste(
          "display: flex;",
          "flex-direction: column;",
          "gap: 0.75rem;",
          "max-height: 420px;",
          "overflow-y: auto;",
          "padding-right: 0.25rem;"
        ),
        lapply(seq_len(nrow(files)), function(i) {
          row <- files[i, , drop = FALSE]

          div(
            style = paste(
              "display: flex;",
              "justify-content: space-between;",
              "align-items: center;",
              "gap: 1rem;",
              "padding: 0.75rem 1rem;",
              "border: 1px solid #e9ecef;",
              "border-radius: 0.5rem;"
            ),
            div(
              div(style = "font-weight: 600; color: #1d2a36;", row$file_name[[1]]),
              div(
                style = "font-size: 0.85rem; color: #697786;",
                paste0("Modified: ", row$modified_at[[1]], " | Size: ", row$size_kb[[1]], " KB")
              )
            ),
            downloadButton(
              outputId = session$ns(paste0("download_log_", i)),
              label = "Download",
              class = "btn-secondary btn-sm"
            )
          )
        })
      )
    })

    output$password_notice <- renderText({
      password_notice()
    })
  })
}
