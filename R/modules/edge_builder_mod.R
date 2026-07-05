edge_builder_normalize_department <- function(values) {
  if (is.null(values)) return(character(0))

  vals <- as.character(values)
  vals <- trimws(vals)
  vals[vals == ""] <- NA_character_
  vals
}

EDGE_BUILDER_DEPT_ALL   <- "__all_departments__"
EDGE_BUILDER_DEPT_NONE  <- "__no_department__"
EDGE_BUILDER_SORT_NONE  <- "original"
EDGE_BUILDER_SORT_ASC   <- "description_az"
EDGE_BUILDER_SORT_DESC  <- "description_za"

edge_builder_sort_choices <- function() {
  c(
    "Original order"  = EDGE_BUILDER_SORT_NONE,
    "Description A-Z" = EDGE_BUILDER_SORT_ASC,
    "Description Z-A" = EDGE_BUILDER_SORT_DESC
  )
}

edge_builder_department_choices <- function(df) {
  choices <- stats::setNames(EDGE_BUILDER_DEPT_ALL, "All departments")

  if (is.null(df) || !is.data.frame(df) || !("Department" %in% names(df))) {
    return(choices)
  }

  departments <- edge_builder_normalize_department(df$Department)
  named_departments <- sort(unique(departments[!is.na(departments)]))
  if (length(named_departments) > 0) {
    choices <- c(choices, stats::setNames(named_departments, named_departments))
  }
  if (any(is.na(departments))) {
    choices <- c(choices, stats::setNames(EDGE_BUILDER_DEPT_NONE, "(No department)"))
  }

  choices
}

edge_builder_filter_sort_rows <- function(df,
                                          department_filter = EDGE_BUILDER_DEPT_ALL,
                                          sort_order = EDGE_BUILDER_SORT_NONE) {
  if (is.null(df) || !is.data.frame(df)) {
    df <- data.frame()
  }

  df$.edge_builder_source_index <- seq_len(nrow(df))

  if ("Department" %in% names(df) && !is.null(department_filter) &&
      !identical(department_filter, EDGE_BUILDER_DEPT_ALL)) {
    departments <- edge_builder_normalize_department(df$Department)
    keep <- if (identical(department_filter, EDGE_BUILDER_DEPT_NONE)) {
      is.na(departments)
    } else {
      departments == department_filter
    }
    df <- df[keep %in% TRUE, , drop = FALSE]
  }

  if ("Cost Item Description" %in% names(df) && !is.null(sort_order) &&
      !identical(sort_order, EDGE_BUILDER_SORT_NONE)) {
    descriptions <- tolower(as.character(df[["Cost Item Description"]]))
    order_idx <- if (identical(sort_order, EDGE_BUILDER_SORT_DESC)) {
      order(descriptions, na.last = TRUE, decreasing = TRUE)
    } else {
      order(descriptions, df$.edge_builder_source_index, na.last = TRUE)
    }
    df <- df[order_idx, , drop = FALSE]
  }

  df
}

edge_builder_compute_movable <- function(tpls) {
  names(tpls)[vapply(tpls, function(d) {
    if (!"Department" %in% names(d)) return(FALSE)
    any(!is.na(edge_builder_normalize_department(d$Department)))
  }, logical(1))]
}

edge_builder_can_move_from <- function(active, movable) {
  !is.null(active) && nzchar(active) && active %in% movable
}

edge_builder_move_target_choices <- function(active, movable, new_sentinel) {
  if (!edge_builder_can_move_from(active, movable)) return(character(0))

  existing_targets <- setdiff(movable, active)
  choices <- stats::setNames(existing_targets, existing_targets)
  c(choices, stats::setNames(new_sentinel, "+ New template..."))
}

edge_builder_validate_new_name <- function(raw, existing_names) {
  if (is.null(raw)) return(list(valid = FALSE, msg = NULL))

  trimmed <- trimws(raw)
  if (trimmed == "")            return(list(valid = FALSE, msg = "Required"))
  if (trimmed %in% existing_names) {
    return(list(valid = FALSE, msg = "Name already used"))
  }
  if (nchar(trimmed) > 60)      return(list(valid = FALSE, msg = "Too long (max 60 chars)"))

  list(valid = TRUE, msg = NULL, name = trimmed)
}

edge_builder_move_rows <- function(templates, source, target, indices) {
  moving <- templates[[source]][indices, , drop = FALSE]
  moving$`Template Name` <- target

  templates[[source]] <- templates[[source]][-indices, , drop = FALSE]
  templates[[target]] <- bind_rows(templates[[target]], moving)
  templates
}

edgeBuilderUI <- function(id) {
  ns <- NS(id)
  
  bs4Card(
    title       = "Template builder",
    width       = 12,
    status      = "primary",
    solidHeader = FALSE,
    footer = tagList(
      actionButton(ns("reset"), "Reset to original", class = "btn-outline-secondary btn-sm")
    ),
    fluidRow(
      column(
        width = 4,
        h4("Templates"),
        uiOutput(ns("template_list"))
      ),
      column(
        width = 8,
        h4(textOutput(ns("active_title"))),
        uiOutput(ns("readonly_notice")),
        div(
          style = paste(
            "display: flex;",
            "gap: 0.75rem;",
            "align-items: flex-end;",
            "flex-wrap: wrap;",
            "margin: 0.25rem 0 0.75rem;"
          ),
          selectInput(
            ns("department_filter"),
            "Department",
            choices = stats::setNames(EDGE_BUILDER_DEPT_ALL, "All departments"),
            width = "220px"
          ),
          selectInput(
            ns("sort_order"),
            "Sort",
            choices = edge_builder_sort_choices(),
            selected = EDGE_BUILDER_SORT_NONE,
            width = "180px"
          )
        ),
        reactableOutput(ns("rows_table")),
        div(
          style = "margin-top: 0.5rem; display: flex; align-items: center; gap: 1rem;",
          actionButton(ns("move_selected"), "Move selected...", class = "btn-primary"),
          span(
            style = "font-size: 0.85rem; color: #697786;",
            textOutput(ns("selected_count"), inline = TRUE)
          )
        )
      )
    )
  )
}

edgeBuilderServer <- function(id, edge_templates) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    NEW_SENTINEL <- "__new__"
    
    rv <- reactiveValues(
      original  = NULL,
      templates = NULL,
      movable   = character(0),
      active    = NULL,
      selected  = integer(0)
    )
    
    # ── Helpers ──────────────────────────────────────────────────────────────
    is_movable <- function(nm) edge_builder_can_move_from(nm, rv$movable)

    reset_table_controls <- function(active = rv$active) {
      if (is.null(active) || is.null(rv$templates) || !(active %in% names(rv$templates))) {
        return(invisible(NULL))
      }

      updateSelectInput(
        session,
        "department_filter",
        choices = edge_builder_department_choices(rv$templates[[active]]),
        selected = EDGE_BUILDER_DEPT_ALL
      )
      updateSelectInput(
        session,
        "sort_order",
        choices = edge_builder_sort_choices(),
        selected = EDGE_BUILDER_SORT_NONE
      )
      rv$selected <- integer(0)
      invisible(NULL)
    }
    
    new_name_validity <- reactive({
      edge_builder_validate_new_name(input$new_template_name, names(rv$templates))
    })

    visible_rows <- reactive({
      req(rv$active, rv$templates)

      department_filter <- input$department_filter
      sort_order <- input$sort_order
      if (is.null(department_filter) || !nzchar(department_filter)) {
        department_filter <- EDGE_BUILDER_DEPT_ALL
      }
      if (is.null(sort_order) || !nzchar(sort_order)) {
        sort_order <- EDGE_BUILDER_SORT_NONE
      }

      edge_builder_filter_sort_rows(
        rv$templates[[rv$active]],
        department_filter = department_filter,
        sort_order = sort_order
      )
    })
    
    # ── Initialise from upstream ─────────────────────────────────────────────
    observeEvent(edge_templates(), {
      req(edge_templates())
      tpls <- edge_templates()
      
      rv$original  <- tpls
      rv$templates <- tpls
      rv$movable   <- edge_builder_compute_movable(tpls)
      
      if (is.null(rv$active) || !(rv$active %in% names(tpls))) {
        rv$active <- if (length(rv$movable) > 0) rv$movable[1] else names(tpls)[1]
      }
      reset_table_controls()
    })
    
    # ── Reset ────────────────────────────────────────────────────────────────
    observeEvent(input$reset, {
      req(rv$original)
      
      showModal(modalDialog(
        title = "Reset templates?",
        p("This will discard all moves and restore the original templates."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset"), "Reset", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_reset, {
      rv$templates <- rv$original
      rv$selected  <- integer(0)
      rv$active    <- if (length(rv$movable) > 0) rv$movable[1] else names(rv$original)[1]
      reset_table_controls()
      
      removeModal()
      showNotification("Templates reset to original", type = "message", duration = 2)
    })
    
    # ── Left pane ────────────────────────────────────────────────────────────
    output$template_list <- renderUI({
      req(rv$templates)
      
      tagList(
        lapply(names(rv$templates), function(nm) {
          n_rows <- nrow(rv$templates[[nm]])
          label  <- paste0(nm, " (", n_rows, " rows)")
          if (!is_movable(nm)) label <- paste0(label, " — read-only")
          
          div(
            style = "padding: 0.4rem 0;",
            actionLink(
              inputId = ns(paste0("sel_", nm)),
              label   = label
            )
          )
        })
      )
    })
    
    observe({
      req(rv$templates)
      
      lapply(names(rv$templates), function(nm) {
        observeEvent(input[[paste0("sel_", nm)]], {
          rv$active   <- nm
          rv$selected <- integer(0)
          reset_table_controls(nm)
        }, ignoreInit = TRUE)
      })
    })
    
    # ── Right pane: title + read-only notice ────────────────────────────────
    output$active_title <- renderText({
      req(rv$active)
      rv$active
    })
    
    output$readonly_notice <- renderUI({
      req(rv$active)
      if (is_movable(rv$active)) return(NULL)
      
      div(
        style = paste(
          "background: #fff8e1;",
          "border-left: 3px solid #f0ad4e;",
          "padding: 0.5rem 0.75rem;",
          "margin: 0.5rem 0;",
          "font-size: 0.85rem;",
          "color: #6b5400;",
          "border-radius: 3px;"
        ),
        "Main arm template — combined activities, read-only in this view"
      )
    })
    
    # ── Reactable ────────────────────────────────────────────────────────────
    output$rows_table <- renderReactable({
      req(rv$active, rv$templates)
      
      df <- visible_rows()
      
      # Pin column order: Department first, then everything else in its existing order
      preferred_order <- c("Department", "Cost Item Description", "Default Cost", "Analysis Code")
      df_cols         <- c(intersect(preferred_order, names(df)),
                           setdiff(names(df), preferred_order))
      df              <- df[, df_cols, drop = FALSE]
      
      visible_cols <- intersect(
        c("Department", "Cost Item Description", "Default Cost", "Analysis Code"),
        names(df)
      )
      hidden_cols  <- setdiff(names(df), visible_cols)
      
      col_defs <- c(
        list(
          `Department` = colDef(
            name     = "Department",
            minWidth = 140,
            cell = function(value) {
              if (is.na(value) || value == "") return("—")
              tags$span(
                style = paste(
                  "display: inline-block;",
                  "background: #e8f0f7;",
                  "color: #1f5f8b;",
                  "padding: 0.15rem 0.6rem;",
                  "border-radius: 12px;",
                  "font-size: 0.78rem;",
                  "font-weight: 500;",
                  "white-space: nowrap;"
                ),
                value
              )
            },
            html = TRUE
          ),
          `Cost Item Description` = colDef(name = "Description", minWidth = 240),
          `Default Cost` = colDef(
            name     = "Cost",
            minWidth = 100,
            align    = "right",
            format   = colFormat(prefix = "£", separators = TRUE, digits = 2)
          ),
          `Analysis Code` = colDef(name = "Code", minWidth = 110)
        ),
        setNames(lapply(hidden_cols, function(x) colDef(show = FALSE)), hidden_cols)
      )
      
      reactable(
        df,
        columns       = col_defs,
        selection     = "multiple",
        onClick       = "select",
        striped       = TRUE,
        highlight     = TRUE,
        compact       = TRUE,
        rownames      = FALSE,
        pagination    = FALSE,
        height        = 480,
        resizable     = TRUE,
        wrap          = FALSE,
        defaultColDef = colDef(minWidth = 120)
      )
    })
    
    # ── Selection ────────────────────────────────────────────────────────────
    observe({
      df <- visible_rows()
      sel <- getReactableState("rows_table", "selected")
      if (is.null(sel) || length(sel) == 0) {
        rv$selected <- integer(0)
      } else {
        sel <- sel[sel >= 1L & sel <= nrow(df)]
        rv$selected <- as.integer(df$.edge_builder_source_index[sel])
      }
    })

    observeEvent(list(input$department_filter, input$sort_order), {
      rv$selected <- integer(0)
    }, ignoreInit = TRUE)
    
    output$selected_count <- renderText({
      paste0(length(rv$selected), " selected")
    })
    
    observe({
      can_move <- length(rv$selected) > 0 && is_movable(rv$active)
      shinyjs::toggleState("move_selected", condition = can_move)
    })
    
    # ── Move modal ───────────────────────────────────────────────────────────
    observeEvent(input$move_selected, {
      req(length(rv$selected) > 0, rv$active, rv$templates, is_movable(rv$active))
      
      target_choices <- edge_builder_move_target_choices(
        active = rv$active,
        movable = rv$movable,
        new_sentinel = NEW_SENTINEL
      )
      
      showModal(modalDialog(
        title = paste0("Move ", length(rv$selected), " rows to..."),
        selectInput(
          ns("move_target"),
          label   = "Target template",
          choices = target_choices
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] === '%s'", ns("move_target"), NEW_SENTINEL),
          div(
            id = ns("new_name_wrap"),
            textInput(ns("new_template_name"), "New template name", value = "")
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_move"), "Confirm", class = "btn-primary")
        )
      ))
    })
    
    observe({
      req(input$move_target)
      ok <- if (input$move_target == NEW_SENTINEL) {
        new_name_validity()$valid
      } else {
        TRUE
      }
      shinyjs::toggleState("confirm_move", condition = ok)
    })
    
    observe({
      req(input$move_target == NEW_SENTINEL)
      v <- new_name_validity()
      feedbackDanger(
        inputId = ns("new_template_name"),
        show    = !v$valid && !is.null(v$msg),
        text    = v$msg
      )
    })
    
    # ── Confirm move ─────────────────────────────────────────────────────────
    observeEvent(input$confirm_move, {
      req(input$move_target, length(rv$selected) > 0, rv$active)
      
      source  <- rv$active
      indices <- rv$selected
      n       <- length(indices)
      
      if (input$move_target == NEW_SENTINEL) {
        v <- new_name_validity()
        req(v$valid)
        
        new_name <- v$name
        rv$templates[[new_name]] <- rv$templates[[source]][0, ]
        rv$movable <- c(rv$movable, new_name)
        
        rv$templates <- edge_builder_move_rows(rv$templates, source, new_name, indices)
        
        rv$active   <- new_name
        rv$selected <- integer(0)
        removeModal()
        
        showNotification(
          paste0("Created '", new_name, "' and moved ", n, " rows"),
          type = "message", duration = 3
        )
      } else {
        target <- input$move_target
        rv$templates <- edge_builder_move_rows(rv$templates, source, target, indices)
        
        rv$selected <- integer(0)
        removeModal()
        
        showNotification(
          paste0("Moved ", n, " rows to ", target),
          type = "message", duration = 3
        )
      }
    })
    
    return(reactive(rv$templates))
  })
}
