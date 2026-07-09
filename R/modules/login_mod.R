loginUI <- function(id) {
  ns <- NS(id)
  app_version_label <- get0("APP_VERSION_LABEL", ifnotfound = "v1.0.0")

  div(
    id = "login-overlay",
    class = "login-screen",
    style = "display: none;",
    div(
      class = "login-card-shell",
      div(
        class = "card login-card",
        div(
          class = "card-body",
          h1(
            class = "login-title",
            "RIDS ",
            span(
              style = "font-size: 0.95rem; color: #697786; font-weight: 400;",
              app_version_label
            )
          ),

          div(
            id = ns("login_view"),
            div(
              class = "login-form",
              textInput(ns("username"), "Username"),
              passwordInput(ns("password"), "Password")
            ),
            actionButton(ns("login"), "Sign in", class = "btn-primary login-button"),
            actionLink(ns("forgot_password"), "Forgot password?", class = "login-link")
          ),

          shinyjs::hidden(
            div(
              id = ns("bootstrap_view"),
              div(
                class = "login-form",
                textInput(ns("bootstrap_name"), "Full name"),
                textInput(ns("bootstrap_username"), "Admin username"),
                passwordInput(ns("bootstrap_password"), "Password"),
                passwordInput(ns("bootstrap_confirm_password"), "Confirm password")
              ),
              actionButton(ns("bootstrap_admin"), "Create admin account", class = "btn-primary login-button"),
              p(
                class = "login-note",
                "First use setup for the initial system administrator."
              )
            )
          ),

          shinyjs::hidden(
            div(
              id = ns("password_change_view"),
              div(
                class = "login-form",
                passwordInput(ns("current_password"), "Current password"),
                passwordInput(ns("new_password"), "New password"),
                passwordInput(ns("confirm_password"), "Confirm new password")
              ),
              actionButton(ns("change_password"), "Update password", class = "btn-primary login-button"),
              p(
                class = "login-note",
                "Your password must be changed before continuing."
              )
            )
          ),

          shinyjs::hidden(
            div(
              id = ns("mfa_view"),
              div(
                class = "login-form",
                textInput(ns("mfa_code"), "Authentication code")
              ),
              actionButton(ns("verify_mfa"), "Verify", class = "btn-primary login-button"),
              actionLink(ns("mfa_back_to_login"), "Back to sign in", class = "login-link"),
              p(
                class = "login-note",
                "Enter the 6-digit code from your authenticator app, or one of your recovery codes."
              )
            )
          ),

          shinyjs::hidden(
            div(
              id = ns("mfa_enroll_view"),
              p(
                class = "login-note",
                "Two-factor authentication is required. Add this account to your",
                " authenticator app (Google Authenticator, Authy, 1Password, ...)",
                " using the setup key below, then enter the current code to finish."
              ),
              uiOutput(ns("enroll_secret_ui")),
              div(
                class = "login-form",
                textInput(ns("enroll_code"), "Code from your authenticator app")
              ),
              actionButton(ns("confirm_enrollment"), "Activate two-factor authentication", class = "btn-primary login-button"),
              actionLink(ns("enroll_back_to_login"), "Back to sign in", class = "login-link")
            )
          ),

          shinyjs::hidden(
            div(
              id = ns("password_reset_view"),
              div(
                class = "login-form",
                textInput(ns("reset_username"), "Username"),
                textInput(ns("reset_code"), "Authentication code"),
                passwordInput(ns("reset_new_password"), "New password"),
                passwordInput(ns("reset_confirm_password"), "Confirm new password")
              ),
              actionButton(ns("reset_password"), "Reset password", class = "btn-primary login-button"),
              actionLink(ns("back_to_login"), "Back to sign in", class = "login-link"),
              p(
                class = "login-note",
                "Enter the 6-digit code from your authenticator app (or a recovery code)",
                " to prove it's you. If you've lost access to your authenticator,",
                " contact an administrator."
              )
            )
          ),

          tags$script(HTML(sprintf(
            paste(
              "(function() {",
              "  var inputId = %s;",
              "  var cookieName = %s;",
              "  var readyId = %s;",
              "  function publishToken() {",
              "    if (window.requestRidsAuthToken) {",
              "      window.requestRidsAuthToken(inputId, cookieName);",
              "    }",
              "    if (window.Shiny && typeof window.Shiny.setInputValue === 'function') {",
              "      window.Shiny.setInputValue(readyId, true, { priority: 'event' });",
              "    }",
              "  }",
              "  document.addEventListener('shiny:connected', publishToken);",
              "  document.addEventListener('DOMContentLoaded', publishToken);",
              "  setTimeout(publishToken, 100);",
              "})();"
            ),
            jsonlite::toJSON(ns("client_auth_token"), auto_unbox = TRUE),
            jsonlite::toJSON(AUTH_COOKIE_NAME, auto_unbox = TRUE),
            jsonlite::toJSON(ns("auth_restore_ready"), auto_unbox = TRUE)
          )))
        )
      )
    )
  )
}

loginServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    auth_provider <- build_auth_provider()

    auth_state <- reactiveValues(
      logged_in = FALSE,
      user_id = NULL,
      username = NULL,
      name = NULL,
      role = NULL,
      session_id = NULL,
      must_change_password = FALSE,
      auth_ready = FALSE,
      logout = NULL,
      change_password = NULL
    )

    bootstrap_needed <- reactiveVal(!users_exist())
    pending_mfa_user <- reactiveVal(NULL)
    pending_enrollment <- reactiveVal(NULL)

    show_view <- function(view_id) {
      shinyjs::hide("login_view")
      shinyjs::hide("bootstrap_view")
      shinyjs::hide("password_change_view")
      shinyjs::hide("mfa_view")
      shinyjs::hide("mfa_enroll_view")
      shinyjs::hide("password_reset_view")
      shinyjs::show(view_id)
    }

    sync_bootstrap_state <- function() {
      bootstrap_needed(!users_exist())
    }

    clear_password_fields <- function() {
      updateTextInput(session, "password", value = "")
      updateTextInput(session, "bootstrap_password", value = "")
      updateTextInput(session, "bootstrap_confirm_password", value = "")
      updateTextInput(session, "current_password", value = "")
      updateTextInput(session, "new_password", value = "")
      updateTextInput(session, "confirm_password", value = "")
      updateTextInput(session, "reset_username", value = "")
      updateTextInput(session, "reset_code", value = "")
      updateTextInput(session, "reset_new_password", value = "")
      updateTextInput(session, "reset_confirm_password", value = "")
      updateTextInput(session, "mfa_code", value = "")
      updateTextInput(session, "enroll_code", value = "")
    }

    clear_pending_mfa <- function() {
      pending_mfa_user(NULL)
      pending_enrollment(NULL)
    }

    clear_auth_state <- function(reset_app_state = TRUE) {
      auth_state$logged_in <- FALSE
      auth_state$user_id <- NULL
      auth_state$username <- NULL
      auth_state$name <- NULL
      auth_state$role <- NULL
      auth_state$session_id <- NULL
      auth_state$must_change_password <- FALSE

      if (isTRUE(reset_app_state) && is.function(session$userData$reset_app_state)) {
        session$userData$reset_app_state()
      }
    }

    apply_user_state <- function(user_row, session_id = NULL) {
      auth_state$logged_in <- TRUE
      auth_state$user_id <- user_row$user_id[[1]]
      auth_state$username <- user_row$username[[1]]
      auth_state$name <- user_row$name[[1]]
      auth_state$role <- normalize_role(user_row$role[[1]])
      auth_state$session_id <- session_id %||% auth_state$session_id
      auth_state$must_change_password <- isTRUE(user_row$force_password_change[[1]])
    }

    auth_state$logout <- function(reset_app_state = TRUE) {
      if (!is.null(auth_state$session_id)) {
        auth_provider$revoke_session(
          session_id = auth_state$session_id,
          actor_user_id = auth_state$user_id,
          event_type = "logout",
          message = "User logged out"
        )
      }

      session$sendCustomMessage(
        "clearAuthCookie",
        list(name = AUTH_COOKIE_NAME)
      )

      clear_auth_state(reset_app_state = reset_app_state)
      sync_bootstrap_state()
    }

    auth_state$change_password <- function(current_password, new_password) {
      if (is.null(auth_state$user_id)) {
        return(list(success = FALSE, message = "No active user session."))
      }

      result <- auth_provider$change_password(
        user_id = auth_state$user_id,
        current_password = current_password,
        new_password = new_password,
        actor_user_id = auth_state$user_id
      )

      if (isTRUE(result$success)) {
        refreshed_user <- auth_provider$get_user_by_id(auth_state$user_id)
        if (!is.null(refreshed_user)) {
          apply_user_state(refreshed_user, session_id = auth_state$session_id)
        }
      }

      result
    }

    complete_login <- function(user_row, event_label = "Login successful.") {
      session_result <- auth_provider$create_session(
        user_id = user_row$user_id[[1]],
        user_agent = session$request$HTTP_USER_AGENT,
        duration_hours = session_duration_hours()
      )

      auth_provider$touch_last_login(user_row$user_id[[1]])
      refreshed_user <- auth_provider$get_user_by_id(user_row$user_id[[1]])
      apply_user_state(refreshed_user, session_id = session_result$session_id)

      session$sendCustomMessage(
        "setAuthCookie",
        list(
          name = AUTH_COOKIE_NAME,
          value = session_result$token,
          maxAge = session_result$max_age
        )
      )

      log_auth_event(
        event_type = "login_success",
        user_id = refreshed_user$user_id[[1]],
        actor_user_id = refreshed_user$user_id[[1]],
        username = refreshed_user$username[[1]],
        success = TRUE,
        message = if (isTRUE(auth_state$must_change_password)) {
          "Login successful; password change required"
        } else {
          "Login successful"
        },
        session_id = session_result$session_id
      )

      showNotification(event_label, type = "message", duration = 5)
    }

    # After password verification, gate session issuance behind MFA:
    # enrolled users get the challenge view; unenrolled users must enroll.
    begin_mfa_stage <- function(user_row, event_label = "Login successful.") {
      pending_mfa_user(list(user = user_row, event_label = event_label))

      if (isTRUE(auth_provider$mfa_enrolled(user_row$user_id[[1]]))) {
        show_view("mfa_view")
      } else {
        enrollment <- auth_provider$start_mfa_enrollment(
          user_id = user_row$user_id[[1]],
          username = user_row$username[[1]]
        )
        pending_enrollment(enrollment)
        show_view("mfa_enroll_view")
      }
    }

    finish_mfa_stage <- function() {
      pending <- pending_mfa_user()
      req(pending)

      complete_login(pending$user, event_label = pending$event_label)
      clear_pending_mfa()
      clear_password_fields()
      auth_state$auth_ready <- TRUE
      show_view("login_view")
    }

    output$enroll_secret_ui <- renderUI({
      enrollment <- pending_enrollment()
      req(enrollment)

      div(
        class = "login-note",
        style = "text-align: left; word-break: break-all;",
        p(tags$strong("Setup key: "), tags$code(enrollment$secret)),
        p(
          style = "font-size: 0.7rem;",
          "Or add by URL: ",
          tags$code(enrollment$provisioning_uri)
        )
      )
    })

    observe({
      if (!isTRUE(auth_state$auth_ready)) {
        return()
      }

      if (!is.null(pending_mfa_user())) {
        return()
      }

      if (isTRUE(bootstrap_needed())) {
        show_view("bootstrap_view")
      } else if (isTRUE(auth_state$must_change_password)) {
        show_view("password_change_view")
      } else {
        show_view("login_view")
      }
    })

    observeEvent(input$login, {
      req(trimws(input$username) != "")
      req(input$password != "")

      login_result <- tryCatch({
        auth_provider$authenticate(input$username, input$password)
      }, error = function(e) {
        app_log_exception("auth", "Login request failed", e, list(username = trimws(input$username)))
        NULL
      })

      if (is.null(login_result) || !isTRUE(login_result$success)) {
        feedbackDanger("username", show = TRUE, text = "Invalid username or password.")
        feedbackDanger("password", show = TRUE, text = "")
        showNotification("Unable to sign in.", type = "warning", duration = 5)
        return()
      }

      begin_mfa_stage(login_result$user)
    })

    observeEvent(input$verify_mfa, {
      pending <- pending_mfa_user()
      req(pending)

      result <- tryCatch({
        auth_provider$verify_mfa(pending$user$user_id[[1]], input$mfa_code)
      }, error = function(e) {
        app_log_exception("auth", "MFA verification failed", e, list(user_id = pending$user$user_id[[1]]))
        NULL
      })

      if (is.null(result) || !isTRUE(result$success)) {
        feedbackDanger("mfa_code", show = TRUE, text = "Invalid code.")
        showNotification("That code didn't work. Try again.", type = "warning", duration = 5)
        return()
      }

      finish_mfa_stage()
    })

    observeEvent(input$confirm_enrollment, {
      pending <- pending_mfa_user()
      req(pending)

      result <- tryCatch({
        auth_provider$confirm_mfa_enrollment(pending$user$user_id[[1]], input$enroll_code)
      }, error = function(e) {
        app_log_exception("auth", "MFA enrollment failed", e, list(user_id = pending$user$user_id[[1]]))
        NULL
      })

      if (is.null(result) || !isTRUE(result$success)) {
        feedbackDanger("enroll_code", show = TRUE, text = result$message %||% "Invalid code.")
        return()
      }

      showModal(modalDialog(
        title = "Recovery codes",
        p(
          "Store these one-time recovery codes somewhere safe. Each works once",
          " if you ever lose access to your authenticator app. They will not be",
          " shown again."
        ),
        tags$pre(paste(result$recovery_codes, collapse = "\n")),
        easyClose = FALSE,
        footer = modalButton("I've saved these codes")
      ))

      finish_mfa_stage()
    })

    observeEvent(input$mfa_back_to_login, {
      clear_pending_mfa()
      clear_password_fields()
      show_view("login_view")
    })

    observeEvent(input$enroll_back_to_login, {
      clear_pending_mfa()
      clear_password_fields()
      show_view("login_view")
    })

    observeEvent(input$bootstrap_admin, {
      req(isTRUE(bootstrap_needed()))

      if (trimws(input$bootstrap_name) == "") {
        feedbackDanger("bootstrap_name", show = TRUE, text = "Required")
        return()
      }

      if (trimws(input$bootstrap_username) == "") {
        feedbackDanger("bootstrap_username", show = TRUE, text = "Required")
        return()
      }

      if (nchar(input$bootstrap_password) < 8) {
        feedbackDanger("bootstrap_password", show = TRUE, text = "Minimum 8 characters")
        return()
      }

      if (!identical(input$bootstrap_password, input$bootstrap_confirm_password)) {
        feedbackDanger("bootstrap_confirm_password", show = TRUE, text = "Passwords do not match")
        return()
      }

      result <- tryCatch({
        auth_provider$bootstrap_admin(
          name = input$bootstrap_name,
          username = input$bootstrap_username,
          password = input$bootstrap_password
        )
      }, error = function(e) {
        app_log_exception("auth", "Bootstrap admin creation failed", e, list(username = trimws(input$bootstrap_username)))
        NULL
      })

      if (is.null(result) || !isTRUE(result$success)) {
        feedbackDanger(
          "bootstrap_username",
          show = TRUE,
          text = result$message %||% "Unable to create the admin account."
        )
        return()
      }

      sync_bootstrap_state()
      begin_mfa_stage(result$user, event_label = "Admin account created.")
    })

    observeEvent(input$forgot_password, {
      req(!isTRUE(bootstrap_needed()))
      clear_password_fields()
      show_view("password_reset_view")
    })

    observeEvent(input$back_to_login, {
      clear_password_fields()
      show_view("login_view")
    })

    observeEvent(input$change_password, {
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

      auth_state$must_change_password <- FALSE
      clear_password_fields()
      auth_state$auth_ready <- TRUE
      showNotification("Password updated.", type = "message", duration = 5)
    })

    observeEvent(input$reset_password, {
      req(!isTRUE(bootstrap_needed()))

      if (trimws(input$reset_username) == "") {
        feedbackDanger("reset_username", show = TRUE, text = "Required")
        return()
      }

      if (trimws(input$reset_code) == "") {
        feedbackDanger("reset_code", show = TRUE, text = "Required")
        return()
      }

      if (nchar(input$reset_new_password) < 8) {
        feedbackDanger("reset_new_password", show = TRUE, text = "Minimum 8 characters")
        return()
      }

      if (!identical(input$reset_new_password, input$reset_confirm_password)) {
        feedbackDanger("reset_confirm_password", show = TRUE, text = "Passwords do not match")
        return()
      }

      result <- tryCatch({
        auth_provider$reset_password_with_mfa(
          username = input$reset_username,
          code = input$reset_code,
          new_password = input$reset_new_password
        )
      }, error = function(e) {
        app_log_exception("auth", "MFA password reset failed", e, list(username = trimws(input$reset_username)))
        NULL
      })

      if (is.null(result) || !isTRUE(result$success)) {
        feedbackDanger(
          "reset_username",
          show = TRUE,
          text = result$message %||% "Password reset failed."
        )
        showNotification(result$message %||% "Password reset failed.", type = "warning", duration = 5)
        return()
      }

      clear_password_fields()
      show_view("login_view")
      showNotification("Password reset. Sign in with your new password.", type = "message", duration = 5)
    })

    observe({
      if (isTRUE(bootstrap_needed())) {
        auth_state$auth_ready <- TRUE
        return()
      }

      if (isTRUE(auth_state$logged_in) && !is.null(auth_state$session_id)) {
        return()
      }

      token <- get_cookie_value(session$request$HTTP_COOKIE %||% "", AUTH_COOKIE_NAME)
      if (identical(token, "")) {
        return()
      }

      restore_result <- tryCatch({
        auth_provider$restore_session(token)
      }, error = function(e) {
        app_log_exception("auth", "Session restore failed", e)
        list(success = FALSE, status = "invalid")
      })

      if (!isTRUE(restore_result$success)) {
        session$sendCustomMessage("clearAuthCookie", list(name = AUTH_COOKIE_NAME))
        clear_auth_state(reset_app_state = TRUE)
        return()
      }

      apply_user_state(restore_result$session, session_id = restore_result$session$session_id[[1]])
      auth_state$auth_ready <- TRUE
    })

    observeEvent(input$client_auth_token, {
      token <- input$client_auth_token %||% ""

      if (identical(token, "")) {
        return()
      }

      if (isTRUE(bootstrap_needed())) {
        return()
      }

      if (isTRUE(auth_state$logged_in) && !is.null(auth_state$session_id)) {
        return()
      }

      restore_result <- tryCatch({
        auth_provider$restore_session(token)
      }, error = function(e) {
        app_log_exception("auth", "Client token restore failed", e)
        list(success = FALSE, status = "invalid")
      })

      if (!isTRUE(restore_result$success)) {
        session$sendCustomMessage("clearAuthCookie", list(name = AUTH_COOKIE_NAME))
        clear_auth_state(reset_app_state = TRUE)
        return()
      }

      apply_user_state(restore_result$session, session_id = restore_result$session$session_id[[1]])
      auth_state$auth_ready <- TRUE
    }, ignoreInit = FALSE)

    observeEvent(input$auth_restore_ready, {
      auth_state$auth_ready <- TRUE
    }, ignoreInit = FALSE)

    auth_state
  })
}
