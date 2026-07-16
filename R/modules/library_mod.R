libraryUI <- function(id) {
  ns <- NS(id)
  div(
    class = "rids-page rids-library-page",
    div(class = "rids-page-header", div(div(class = "rids-page-eyebrow", "Portfolio"), h1("Study library"), p("Find, review and continue work across all studies.")), div(class = "rids-page-mark", icon("book-open"))),
    div(
      class = "rids-filter-bar library-filter-bar",
      textInput(ns("search"), "Search studies", placeholder = "Study name, CPMS ID, or EDGE"),
      selectInput(ns("site_filter"), "Study site", choices = c("All sites" = "")),
      selectInput(ns("speciality_filter"), "Speciality", choices = c("All specialities" = "")),
      selectInput(ns("uploaded_by_filter"), "Uploaded by", choices = c("All uploaders" = "")),
      selectInput(
        ns("sort_by"),
        "Sort by",
        choices = c(
          "Newest first" = "newest",
          "Oldest first" = "oldest",
          "Study name A-Z" = "study_name_asc"
        ),
        selected = "newest"
      ),
      div(
        class = "rids-filter-action",
        actionButton(ns("clear_filters"), "Clear filters", class = "btn btn-default")
      )
    ),
    uiOutput(ns("study_cards"))
  )
}

libraryServer <- function(id, auth_state, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    page_size <- 24L
    visible_count <- reactiveVal(page_size)
    pending_delete <- reactiveVal(NULL)

    fetch_studies <- function() {
      rids_repos()$studies$list_studies()
    }

    build_study_ref <- function(row) {
      list(
        cpms_id = as.character(row$cpms_id),
        study_site = as.character(row$study_site),
        scenario_id = as.character(row$scenario_id)
      )
    }

    same_study_ref <- function(lhs, rhs) {
      if (is.null(lhs) || is.null(rhs)) return(FALSE)

      identical(trimws(as.character(lhs$cpms_id)), trimws(as.character(rhs$cpms_id))) &&
        identical(trimws(as.character(lhs$study_site)), trimws(as.character(rhs$study_site))) &&
        identical(trimws(as.character(lhs$scenario_id)), trimws(as.character(rhs$scenario_id)))
    }

    study_key <- function(cpms_id, study_site, scenario_id) {
      paste(cpms_id, study_site, scenario_id, sep = "||")
    }

    build_choice_vector <- function(values, all_label) {
      vals <- sort(unique(trimws(as.character(values))))
      vals <- vals[nzchar(vals)]
      stats::setNames(c("", vals), c(all_label, vals))
    }

    normalize_text_vector <- function(values) {
      vals <- trimws(as.character(values))
      vals[is.na(vals)] <- ""
      vals
    }

    studies <- reactive({
      shared_state$library_refresh
      data <- fetch_studies()

      if (nrow(data) == 0) {
        data$study_key <- character(0)
        return(data)
      }

      data$study_key <- mapply(
        study_key,
        data$cpms_id,
        data$study_site,
        data$scenario_id,
        USE.NAMES = FALSE
      )
      data
    })

    observeEvent(studies(), {
      site_selected <- isolate(input$site_filter) %||% ""
      speciality_selected <- isolate(input$speciality_filter) %||% ""
      uploader_selected <- isolate(input$uploaded_by_filter) %||% ""
      data <- studies()

      site_choices <- build_choice_vector(data$study_site, "All sites")
      speciality_choices <- build_choice_vector(data$speciality_name, "All specialities")
      uploader_choices <- build_choice_vector(data$uploaded_by, "All uploaders")

      updateSelectInput(
        session,
        "site_filter",
        choices = site_choices,
        selected = if (site_selected %in% unname(site_choices)) site_selected else ""
      )
      updateSelectInput(
        session,
        "speciality_filter",
        choices = speciality_choices,
        selected = if (speciality_selected %in% unname(speciality_choices)) speciality_selected else ""
      )
      updateSelectInput(
        session,
        "uploaded_by_filter",
        choices = uploader_choices,
        selected = if (uploader_selected %in% unname(uploader_choices)) uploader_selected else ""
      )
    }, ignoreInit = FALSE)

    filtered_studies <- reactive({
      data <- studies()

      if (nrow(data) == 0) {
        return(data)
      }

      search_value <- trimws(tolower(input$search %||% ""))
      site_value <- trimws(input$site_filter %||% "")
      speciality_value <- trimws(input$speciality_filter %||% "")
      uploader_value <- trimws(input$uploaded_by_filter %||% "")
      sort_value <- input$sort_by %||% "newest"

      if (nzchar(search_value)) {
        haystack <- paste(
          tolower(normalize_text_vector(data$study_name)),
          tolower(normalize_text_vector(data$cpms_id)),
          tolower(normalize_text_vector(data$edge_id))
        )
        data <- data[grepl(search_value, haystack, fixed = TRUE), , drop = FALSE]
      }

      if (nzchar(site_value)) {
        data <- data[data$study_site == site_value, , drop = FALSE]
      }

      if (nzchar(speciality_value)) {
        data <- data[data$speciality_name == speciality_value, , drop = FALSE]
      }

      if (nzchar(uploader_value)) {
        data <- data[data$uploaded_by == uploader_value, , drop = FALSE]
      }

      if (nrow(data) == 0) {
        return(data)
      }

      data <- switch(
        sort_value,
        oldest = data[order(data$upload_timestamp, na.last = TRUE), , drop = FALSE],
        study_name_asc = data[
          order(tolower(normalize_text_vector(data$study_name)), data$upload_timestamp, na.last = TRUE),
          ,
          drop = FALSE
        ],
        data[order(data$upload_timestamp, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
      )

      rownames(data) <- NULL
      data
    })

    visible_studies <- reactive({
      data <- filtered_studies()

      if (nrow(data) == 0) {
        return(data)
      }

      head(data, visible_count())
    })

    observeEvent(
      list(input$search, input$site_filter, input$speciality_filter, input$uploaded_by_filter, input$sort_by, studies()),
      {
        visible_count(page_size)
      },
      ignoreInit = FALSE
    )

    observeEvent(input$load_more, {
      visible_count(visible_count() + page_size)
    }, ignoreInit = TRUE)

    observeEvent(input$clear_filters, {
      updateTextInput(session, "search", value = "")
      updateSelectInput(session, "site_filter", selected = "")
      updateSelectInput(session, "speciality_filter", selected = "")
      updateSelectInput(session, "uploaded_by_filter", selected = "")
      updateSelectInput(session, "sort_by", selected = "newest")
      visible_count(page_size)
    }, ignoreInit = TRUE)
    
    # ── Track selected study ──────────────────────────────────────────────────
    selected_study <- reactiveVal(NULL)
    
    observeEvent(input$open_study, {
      req(input$open_study)

      row_idx <- match(input$open_study, studies()$study_key)
      req(!is.na(row_idx))

      selected_study(studies()[row_idx, , drop = FALSE])
    }, ignoreInit = TRUE)

    observeEvent(input$delete_study, {
      req(input$delete_study)

      row_idx <- match(input$delete_study, studies()$study_key)
      req(!is.na(row_idx))

      row <- studies()[row_idx, , drop = FALSE]
      pending_delete(row)

      showModal(modalDialog(
        title = paste0("Delete ", row$study_name %||% paste0("CPMS ", row$cpms_id), "?"),
        size = "m",
        easyClose = FALSE,
        div(
          style = "color: #1d2a36; line-height: 1.55;",
          p("This will permanently delete the selected run from the study library."),
          tags$ul(
            tags$li("Upload metadata"),
            tags$li("ICT costing rows"),
            tags$li("Posting lines"),
            tags$li("Custom activities"),
            tags$li("Saved workbook and generated EDGE ZIP, if present")
          ),
          p(
            style = "margin-bottom: 0.5rem; font-weight: 600; color: #a94442;",
            "Type DELETE to confirm."
          ),
          textInput(ns("delete_confirmation_text"), "Confirmation", value = "", placeholder = "DELETE")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_study"), "Delete run", class = "btn-danger")
        )
      ))
    }, ignoreInit = TRUE)

    observe({
      confirm_ready <- identical(trimws(input$delete_confirmation_text %||% ""), "DELETE")
      shinyjs::toggleState("confirm_delete_study", condition = confirm_ready)
    })

    observeEvent(input$confirm_delete_study, {
      row <- pending_delete()
      req(row)
      req(identical(trimws(input$delete_confirmation_text %||% ""), "DELETE"))

      delete_result <- tryCatch({
        delete_study_run(
          cpms_id = as.character(row$cpms_id),
          study_site = as.character(row$study_site),
          scenario_id = as.character(row$scenario_id)
        )
      }, error = function(e) {
        if (handle_fatal_db_error(session, e, "library", list(
          cpms_id = row$cpms_id,
          study_site = row$study_site,
          scenario_id = row$scenario_id,
          stage = "delete_study_run"
        ))) {
          return(NULL)
        }

        app_log_exception("library", "Study deletion failed", e, list(
          cpms_id = row$cpms_id,
          study_site = row$study_site,
          scenario_id = row$scenario_id
        ))
        showNotification("Failed to delete the selected study run", type = "error", duration = 8)
        return(NULL)
      })

      req(delete_result)

      removeModal()
      pending_delete(NULL)

      if (same_study_ref(shared_state$current_study, build_study_ref(row))) {
        shared_state$current_study <- NULL
        shinyjs::runjs('$("a[data-value=\'tab_library\']").trigger("click")')
      }

      current_refresh <- isolate(shared_state$library_refresh)
      shared_state$library_refresh <- current_refresh + 1L

      app_log_info("library", "Study run deleted", list(
        cpms_id = row$cpms_id,
        study_site = row$study_site,
        scenario_id = row$scenario_id,
        rows_deleted = delete_result$total_rows_deleted
      ))

      showNotification(
        paste0(
          "Deleted run for CPMS ",
          row$cpms_id,
          " / ",
          row$study_site,
          " / Scenario ",
          row$scenario_id,
          " (",
          delete_result$total_rows_deleted,
          " rows removed)."
        ),
        type = "message",
        duration = 6
      )

      if (length(delete_result$files$failed) > 0) {
        showNotification(
          "Run data was deleted, but one or more saved files could not be removed from disk.",
          type = "warning",
          duration = 8
        )
      }
    }, ignoreInit = TRUE)
    
    # ── Open the selected study in the workspace ─────────────────────────────
    observeEvent(selected_study(), {
      req(selected_study())
      row <- selected_study()
      
      shared_state$current_study <- build_study_ref(row)
      shinyjs::runjs('$("a[data-value=\'tab_study\']").trigger("click")')
      
      selected_study(NULL)
    }, ignoreNULL = TRUE)
    
    # ── Render cards ──────────────────────────────────────────────────────────
    output$study_cards <- renderUI({
      data <- visible_studies()
      total_matches <- nrow(filtered_studies())

      if (nrow(studies()) == 0) {
        return(
          div(
            class = "rids-empty-state rids-library-empty",
            div(class = "rids-empty-icon", icon("book-open")),
            p("No studies are available in the library yet.")
          )
        )
      }

      if (total_matches == 0) {
        return(
          div(
            class = "rids-empty-state rids-library-empty",
            div(class = "rids-empty-icon", icon("filter")),
            h2("0 studies match the current filters"),
            p("Try broadening your search or clearing one or more filters.")
          )
        )
      }
      
      shown_matches <- nrow(data)

      tagList(
        div(
          class = "rids-library-summary",
          div(
            class = "rids-library-summary-count",
            paste0(
              format(total_matches, big.mark = ","),
              if (total_matches == 1) " study" else " studies",
              " found"
            )
          ),
          div(
            class = "rids-library-summary-visible",
            paste0(
              "Showing ",
              format(shown_matches, big.mark = ","),
              " of ",
              format(total_matches, big.mark = ",")
            )
          )
        ),
        div(
          class = "rids-library-grid",
          lapply(seq_len(nrow(data)), function(i) {
            row <- data[i, ]
            open_key_json <- jsonlite::toJSON(
              as.character(row$study_key),
              auto_unbox = TRUE,
              null = "null"
            )
            delete_key_json <- jsonlite::toJSON(
              as.character(row$study_key),
              auto_unbox = TRUE,
              null = "null"
            )

            div(
              class = "card rids-study-card",
              div(
                class = "card-body rids-study-card-body",
                div(
                  class = "rids-study-card-title",
                  row$study_name %||% paste0("CPMS ", row$cpms_id)
                ),
                div(
                  class = "rids-study-chips",
                  span(
                    class = "rids-study-chip is-scenario",
                    paste0("Scenario ", row$scenario_id %||% "Unknown")
                  ),
                  span(
                    class = "rids-study-chip is-site",
                    row$study_site %||% "Site unknown"
                  ),
                  span(
                    class = "rids-study-chip is-speciality",
                    row$speciality_name %||% "Speciality unknown"
                  ),
                  span(
                    class = "rids-study-chip is-edge",
                    paste0("EDGE: ", row$edge_id %||% "Not set")
                  )
                ),
                div(
                  class = "rids-study-meta",
                  div(
                    class = "rids-study-meta-row",
                    span(class = "rids-study-meta-icon", icon("hashtag")),
                    span(class = "rids-study-meta-value", paste0("CPMS ", row$cpms_id %||% "Unknown"))
                  ),
                  div(
                    class = "rids-study-meta-row",
                    span(class = "rids-study-meta-icon", icon("user")),
                    span(class = "rids-study-meta-value", row$uploaded_by %||% "Unknown uploader")
                  ),
                  div(
                    class = "rids-study-meta-row",
                    span(class = "rids-study-meta-icon", icon("clock")),
                    span(
                      class = "rids-study-meta-value",
                      format(as.POSIXct(row$upload_timestamp), "%d %b %Y %H:%M")
                    )
                  )
                ),
                div(
                  class = "rids-study-actions",
                  actionButton(
                    inputId = ns(paste0("open_study_", i)),
                    label   = tagList(icon("folder-open"), " Open"),
                    class   = "btn btn-sm btn-outline-primary rids-study-action is-primary",
                    onclick = sprintf(
                      "Shiny.setInputValue('%s', %s, {priority: 'event'})",
                      ns("open_study"),
                      open_key_json
                    )
                  ),
                  actionButton(
                    inputId = ns(paste0("delete_study_", i)),
                    label   = tagList(icon("trash"), " Delete"),
                    class   = "btn btn-sm btn-outline-danger rids-study-action",
                    onclick = sprintf(
                      "Shiny.setInputValue('%s', %s, {priority: 'event'})",
                      ns("delete_study"),
                      delete_key_json
                    )
                  )
                )
              )
            )
          })
        ),
        if (shown_matches < total_matches) {
          div(
            class = "rids-library-load-more",
            actionButton(
              ns("load_more"),
              label = paste0("Load more (", total_matches - shown_matches, " remaining)"),
              class = "btn btn-default"
            )
          )
        }
      )
    })
    
  })
}

`%||%` <- function(a, b) {
  if (is.null(a) || is.na(a) || !nzchar(as.character(a))) b else a
}
