get_app_session <- function(session) {
  if (is.null(session)) {
    return(NULL)
  }

  root_scope <- tryCatch(session$rootScope, error = function(e) NULL)
  if (is.function(root_scope)) {
    return(root_scope())
  }

  session
}

invoke_reset_app_state <- function(reset_fn, reset_library_refresh = TRUE) {
  if (!is.function(reset_fn)) {
    return(invisible(FALSE))
  }

  reset_formals <- tryCatch(names(formals(reset_fn)), error = function(e) NULL)

  if (!is.null(reset_formals) && "reset_library_refresh" %in% reset_formals) {
    reset_fn(reset_library_refresh = reset_library_refresh)
  } else {
    reset_fn()
  }

  invisible(TRUE)
}

is_fatal_db_error <- function(error) {
  msg <- tolower(trimws(conditionMessage(error) %||% ""))
  if (!nzchar(msg)) {
    return(FALSE)
  }

  patterns <- c(
    "database has been invalidated",
    "previous fatal error",
    "fatal error",
    "failed to delete all rows from index"
  )

  any(vapply(patterns, grepl, logical(1), x = msg, fixed = TRUE))
}

handle_fatal_db_error <- function(session,
                                  error,
                                  area,
                                  context = list(),
                                  reset_library_refresh = FALSE) {
  if (!is_fatal_db_error(error)) {
    return(FALSE)
  }

  app_session <- get_app_session(session)
  if (is.null(app_session)) {
    return(FALSE)
  }

  app_log_exception(area, "Fatal database error", error, context)

  if (!isTRUE(session$userData$fatal_db_modal_observer_registered)) {
    observeEvent(session$input$fatal_db_ok, {
      removeModal(session = session)
      app_session$userData$fatal_db_modal_active <- FALSE

      invoke_reset_app_state(
        app_session$userData$reset_app_state,
        reset_library_refresh = isTRUE(
          app_session$userData$fatal_db_reset_library_refresh
        )
      )

      updateTabItems(app_session, "sidebar", selected = "tab_dashboard")
    }, ignoreInit = TRUE)

    session$userData$fatal_db_modal_observer_registered <- TRUE
  }

  app_session$userData$fatal_db_reset_library_refresh <- isTRUE(reset_library_refresh)

  if (isTRUE(app_session$userData$fatal_db_modal_active)) {
    return(TRUE)
  }

  app_session$userData$fatal_db_modal_active <- TRUE

  showModal(
    modalDialog(
      title = "Database Error",
      p("RIDS has encountered a database error and cannot continue safely in this session."),
      p("Press OK, close RIDS, and contact the system administrator."),
      tags$p(
        style = "font-size: 0.85rem; color: #697786; margin-bottom: 0;",
        "Technical detail: the database connection was invalidated after a fatal error."
      ),
      easyClose = FALSE,
      footer = actionButton(
        session$ns("fatal_db_ok"),
        "OK",
        class = "btn-danger"
      )
    ),
    session = session
  )

  TRUE
}
