STEP2_FILTER_ALL_ARMS   <- "__all_study_arms__"
STEP2_FILTER_ALL_VISITS <- "__all_visits__"
STEP2_STATE_COLUMNS     <- c(
  ".step2_base_contract_cost",
  ".step2_override_contract_cost",
  ".step2_has_override"
)

step2_filter_label_values <- function(values) {
  vals <- trimws(as.character(values))
  vals[is.na(vals) | vals == ""] <- NA_character_
  sort(unique(vals[!is.na(vals)]))
}

step2_study_arm_choices <- function(df) {
  choices <- stats::setNames(STEP2_FILTER_ALL_ARMS, "All study arms")
  if (is.null(df) || !is.data.frame(df) || !("Study_Arm" %in% names(df))) {
    return(choices)
  }

  arms <- step2_filter_label_values(df$Study_Arm)
  c(choices, stats::setNames(arms, arms))
}

step2_visit_choices <- function(df) {
  choices <- stats::setNames(STEP2_FILTER_ALL_VISITS, "All visits")
  if (is.null(df) || !is.data.frame(df) || !("Visit_Number" %in% names(df))) {
    return(choices)
  }

  visits <- step2_filter_label_values(df$Visit_Number)
  if (length(visits) == 0) return(choices)

  labels <- vapply(visits, function(visit) {
    label <- NA_character_
    if ("Visit_Label" %in% names(df)) {
      label <- df$Visit_Label[match(visit, df$Visit_Number)]
    }
    label <- trimws(as.character(label))
    if (is.na(label) || label == "" || identical(label, visit)) {
      visit
    } else {
      paste(visit, label, sep = " - ")
    }
  }, character(1))

  c(choices, stats::setNames(visits, labels))
}

step2_filter_rows <- function(df,
                              study_arm_filter = STEP2_FILTER_ALL_ARMS,
                              visit_filter = STEP2_FILTER_ALL_VISITS,
                              activity_search = "") {
  if (is.null(df) || !is.data.frame(df)) {
    df <- data.frame()
  }

  df$.step2_source_index <- seq_len(nrow(df))

  if ("Study_Arm" %in% names(df) && !is.null(study_arm_filter) &&
      !identical(study_arm_filter, STEP2_FILTER_ALL_ARMS)) {
    keep <- trimws(as.character(df$Study_Arm)) == study_arm_filter
    df <- df[keep %in% TRUE, , drop = FALSE]
  }

  if ("Visit_Number" %in% names(df) && !is.null(visit_filter) &&
      !identical(visit_filter, STEP2_FILTER_ALL_VISITS)) {
    keep <- trimws(as.character(df$Visit_Number)) == visit_filter
    df <- df[keep %in% TRUE, , drop = FALSE]
  }

  search <- if (is.null(activity_search)) "" else trimws(as.character(activity_search))
  if (nzchar(search) && "Activity_Name" %in% names(df)) {
    activity <- as.character(df$Activity_Name)
    activity[is.na(activity)] <- ""
    df <- df[grepl(tolower(search), tolower(activity), fixed = TRUE), , drop = FALSE]
  }

  df
}

step2_contract_cost_values <- function(ict_cost, use_unrounded_cost = FALSE) {
  if (isTRUE(use_unrounded_cost)) {
    ict_cost
  } else {
    round(ict_cost)
  }
}

step2_uses_unrounded_cost <- function(value) {
  isTRUE(value) || identical(as.character(value), "unrounded")
}

step2_initialize_contract_costs <- function(df, use_unrounded_cost = FALSE) {
  if (is.null(df) || !is.data.frame(df) || !("ICT_Cost" %in% names(df))) {
    return(df)
  }

  if (!("Contract_Cost" %in% names(df))) {
    df$Contract_Cost <- NA_real_
  }

  missing_contract_cost <- is.na(df$Contract_Cost)
  if (any(missing_contract_cost)) {
    df$Contract_Cost[missing_contract_cost] <- step2_contract_cost_values(
      df$ICT_Cost[missing_contract_cost],
      use_unrounded_cost = use_unrounded_cost
    )
  }

  df
}

step2_strip_state_columns <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    return(df)
  }

  keep <- setdiff(names(df), STEP2_STATE_COLUMNS)
  df[, keep, drop = FALSE]
}

step2_prepare_working_data <- function(df, use_unrounded_cost = FALSE) {
  df <- step2_initialize_contract_costs(df, use_unrounded_cost = use_unrounded_cost)
  if (is.null(df) || !is.data.frame(df) || !("ICT_Cost" %in% names(df))) {
    return(df)
  }

  df$.step2_base_contract_cost <- step2_contract_cost_values(
    df$ICT_Cost,
    use_unrounded_cost = use_unrounded_cost
  )
  df$.step2_override_contract_cost <- rep(NA_real_, nrow(df))
  df$.step2_has_override <- rep(FALSE, nrow(df))
  df
}

step2_reset_contract_cost_mode <- function(df, use_unrounded_cost = FALSE) {
  if (is.null(df) || !is.data.frame(df) || !("ICT_Cost" %in% names(df))) {
    return(df)
  }

  df$.step2_base_contract_cost <- step2_contract_cost_values(
    df$ICT_Cost,
    use_unrounded_cost = use_unrounded_cost
  )
  df$.step2_override_contract_cost <- rep(NA_real_, nrow(df))
  df$.step2_has_override <- rep(FALSE, nrow(df))
  df$Contract_Cost <- df$.step2_base_contract_cost
  df
}

step2_apply_contract_override <- function(df, row_index, contract_cost) {
  if (is.null(df) || !is.data.frame(df)) {
    return(df)
  }

  if (length(row_index) != 1L || is.na(row_index) ||
      row_index < 1L || row_index > nrow(df)) {
    stop("step2_apply_contract_override(): invalid row_index.")
  }

  if (!all(STEP2_STATE_COLUMNS %in% names(df))) {
    stop("step2_apply_contract_override(): working data is missing state columns.")
  }

  df$.step2_override_contract_cost[[row_index]] <- as.numeric(contract_cost)
  df$.step2_has_override[[row_index]] <- TRUE
  df$Contract_Cost[[row_index]] <- as.numeric(contract_cost)
  df
}

step2_UI <- function(id) {
  ns <- NS(id)
  div(
    class = "rids-page rids-workflow-page",
    div(class = "rids-page-header rids-workflow-header", div(div(class = "rids-page-eyebrow", "ICT processing · Step 2 of 4"), h1("Review contract costs"), p("Check imported activities and confirm contract cost values.")), div(class = "rids-page-mark", icon("pound-sign"))),
    uiOutput(ns("amendment_banner")),
    bs4Card(
      title      = "Cost review",
      width      = 12,
      status     = "primary",
      solidHeader = FALSE,
      footer = tagList(
        div(
          class = "step2-rounding-wrap",
          span(class = "step2-rounding-title", "Contract cost mode"),
          radioButtons(
            ns("use_unrounded_cost"),
            label = NULL,
            choices = c("Rounded" = "rounded", "Unrounded" = "unrounded"),
            selected = "rounded",
            inline = TRUE
          )
        ),
        actionButton(ns("save"), "Save to database", class = "btn-success"),
        actionButton(ns("next_step"), "Next: Apply Tags", class = "pipeline-next-btn")
      ),
      div(
        class = "rids-filter-bar rids-inline-filters",
        selectInput(
          ns("study_arm_filter"),
          "Study Arm",
          choices = stats::setNames(STEP2_FILTER_ALL_ARMS, "All study arms"),
          width = "220px"
        ),
        selectInput(
          ns("visit_filter"),
          "Visit",
          choices = stats::setNames(STEP2_FILTER_ALL_VISITS, "All visits"),
          width = "220px"
        ),
        textInput(
          ns("activity_search"),
          "Activity search",
          value = "",
          placeholder = "Search activities...",
          width = "260px"
        )
      ),
      div(
        class = "step2-table-hint",
        icon("mouse-pointer"),
        "Select any row to adjust its contract cost."
      ),
      reactableOutput(ns("table"))
    )
  )
}

step2_Server <- function(id, auth_state, shared_state, current_step) {
  moduleServer(id, function(input, output, session) {
    output$amendment_banner <- render_amendment_workflow_banner(shared_state)

          
    working_data <- reactiveValues(df = NULL)
    selected_row_index <- reactiveVal(NULL)
    is_saved <- reactiveVal(FALSE)
    
    observe({
      shinyjs::toggleState("next_step", condition = isTRUE(is_saved()))
    })

    visible_rows <- reactive({
      req(working_data$df)

      study_arm_filter <- input$study_arm_filter
      visit_filter <- input$visit_filter
      if (is.null(study_arm_filter) || !nzchar(study_arm_filter)) {
        study_arm_filter <- STEP2_FILTER_ALL_ARMS
      }
      if (is.null(visit_filter) || !nzchar(visit_filter)) {
        visit_filter <- STEP2_FILTER_ALL_VISITS
      }

      step2_filter_rows(
        step2_strip_state_columns(working_data$df),
        study_arm_filter = study_arm_filter,
        visit_filter = visit_filter,
        activity_search = input$activity_search
      )
    })

    reset_filters <- function() {
      req(working_data$df)
      updateSelectInput(
        session,
        "study_arm_filter",
        choices = step2_study_arm_choices(working_data$df),
        selected = STEP2_FILTER_ALL_ARMS
      )
      updateSelectInput(
        session,
        "visit_filter",
        choices = step2_visit_choices(working_data$df),
        selected = STEP2_FILTER_ALL_VISITS
      )
      updateTextInput(session, "activity_search", value = "")
      selected_row_index(NULL)
    }
    
    # ── Load data ─────────────────────────────────────────────────────────────
    observeEvent(
      list(
        shared_state$cpms_id,
        shared_state$study_site,
        shared_state$scenario_id,
        shared_state$template_version_id
      ),
      {
      req(shared_state$cpms_id, shared_state$study_site, shared_state$scenario_id)

      df <- rids_repos()$ict_costing$find_by_run(
        as.character(shared_state$cpms_id),
        as.character(shared_state$study_site),
        as.character(shared_state$scenario_id),
        shared_state$template_version_id
      )
      
      working_data$df <- step2_prepare_working_data(
        df,
        use_unrounded_cost = step2_uses_unrounded_cost(isolate(input$use_unrounded_cost))
      )
      reset_filters()
      is_saved(FALSE)
    },
    ignoreInit = FALSE
    )
    
    apply_contract_cost_mode <- function(use_unrounded_cost) {
      req(working_data$df)
      working_data$df <- step2_reset_contract_cost_mode(
        working_data$df,
        use_unrounded_cost = use_unrounded_cost
      )
    }

    # ── Toggle rounding mode ──────────────────────────────────────────────────
    observeEvent(input$use_unrounded_cost, {
      apply_contract_cost_mode(step2_uses_unrounded_cost(input$use_unrounded_cost))
      is_saved(FALSE)
    }, ignoreInit = TRUE)

    observeEvent(list(input$study_arm_filter, input$visit_filter, input$activity_search), {
      selected_row_index(NULL)
    }, ignoreInit = TRUE)
    
    # ── Row select → modal ────────────────────────────────────────────────────
    observeEvent(getReactableState("table", "selected"), {
      selected_visible_row <- getReactableState("table", "selected")
      req(selected_visible_row)
      
      df <- visible_rows()
      selected_visible_row <- selected_visible_row[[1]]
      req(selected_visible_row >= 1L, selected_visible_row <= nrow(df))

      selected_row <- df$.step2_source_index[[selected_visible_row]]
      selected_row_index(selected_row)
      row <- working_data$df[selected_row, ]
      
      showModal(modalDialog(
        title = "Set Custom Value",
        p(strong("Visit: "), row$Visit_Label),
        p(strong("Activity: "), row$Activity_Name),
        p(strong("ICT Cost: "), paste0("£", round(row$ICT_Cost, 2))),
        hr(),
        numericInput(
          session$ns("contract_value"),
          label = "Contract Cost (£)",
          value = row$Contract_Cost,
          min   = 0
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            session$ns("confirm_edit"),
            "Confirm",
            class = "btn-primary",
            onclick = sprintf(
              paste0(
                "var field = document.getElementById('%s');",
                "if (field && window.Shiny) {",
                "Shiny.setInputValue('%s', Number(field.value), {priority: 'event'});",
                "}"
              ),
              session$ns("contract_value"),
              session$ns("contract_value")
            )
          )
        )
      ))
    })
    
    # ── Confirm edit ──────────────────────────────────────────────────────────
    observeEvent(input$confirm_edit, {
      req(input$contract_value)
      
      selected_row <- selected_row_index()
      req(selected_row)
      working_data$df <- step2_apply_contract_override(
        working_data$df,
        row_index = selected_row,
        contract_cost = input$contract_value
      )
      
      is_saved(FALSE)
      removeModal()
    })
    
    # ── Save to DB ────────────────────────────────────────────────────────────
    observeEvent(input$save, {
      req(working_data$df)

      log_event(
        level = "INFO",
        area = "step2",
        message = "Step 2 save started",
        user_id = auth_state$user_id,
        username = auth_state$username,
        cpms_id = shared_state$cpms_id,
        upload_id = shared_state$upload_id,
        session_id = auth_state$session_id,
        details = list(rows = nrow(working_data$df))
      )
      app_log_info("step2", "Save started")
      
      tryCatch({
        rids_repos()$ict_costing$replace_run(
          step2_strip_state_columns(working_data$df),
          as.character(shared_state$cpms_id),
          as.character(shared_state$study_site),
          as.character(shared_state$scenario_id),
          shared_state$template_version_id
        )

        log_event(
          level = "INFO",
          area = "step2",
          message = "Step 2 save completed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          session_id = auth_state$session_id,
          details = list(rows = nrow(working_data$df))
        )
        app_log_info("step2", "Save completed")

        is_saved(TRUE)
        showNotification("Saved successfully", type = "message", duration = 5)
      }, error = function(e) {
        if (handle_fatal_db_error(session, e, "step2", list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(working_data$df),
          stage = "save"
        ))) {
          is_saved(FALSE)
          return(NULL)
        }

        app_log_exception("step2", "Step 2 save failed", e, list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(working_data$df)
        ))
        log_event(
          level = "ERROR",
          area = "step2",
          message = "Step 2 save failed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          session_id = auth_state$session_id,
          details = list(
            rows = nrow(working_data$df),
            error = conditionMessage(e)
          )
        )
        is_saved(FALSE)
        showNotification("Save failed", type = "error", duration = 5)
      })
    })
    
    # ── Render table ──────────────────────────────────────────────────────────
    output$table <- renderReactable({
      req(working_data$df)
      reactable(
        visible_rows(),
        selection = "single",
        onClick   = "select",
        rownames  = FALSE,
        columns = list(
          .step2_source_index = colDef(show = FALSE),
          Contract_Cost = colDef(
            name = "Contract Cost",
            headerStyle = list(
              background = "#eef5fa",
              borderLeft = "1px solid #d6e4ef",
              borderRight = "1px solid #d6e4ef"
            ),
            style = list(
              background = "#f8fbfd",
              borderLeft = "1px solid #e2edf5",
              borderRight = "1px solid #e2edf5"
            )
          )
        )
      )
    })
    
    # ── Next step ─────────────────────────────────────────────────────────────
    observeEvent(input$next_step, {
      current_step("step3")
      shared_state$current_step <- "step3"
      shinyjs::runjs('$("[data-value=\'tab_step3\']").tab("show")')
      shinyjs::runjs("$('body').addClass('sidebar-collapse')")
    })
    
  })
}
