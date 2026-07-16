study_workspace_kv <- function(label, value) {
  div(
    class = "rids-study-kv",
    div(class = "rids-study-kv-label", label),
    div(class = "rids-study-kv-value", value)
  )
}

studyWorkspaceUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "rids-page rids-workspace-page",
    div(
      class = "rids-workspace-header",
      actionLink(
        inputId = ns("back_to_library"),
        label   = tagList(icon("arrow-left"), " Back to library"),
        class = "rids-back-link"
      ),
      div(
        class = "rids-workspace-title",
        textOutput(ns("workspace_title"), inline = TRUE)
      )
    ),
    
    uiOutput(ns("body"))
  )
}

studyWorkspaceServer <- function(id, auth_state, shared_state, current_step) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    version_refresh <- reactiveVal(0L)
    pending_archive_id <- reactiveVal(NULL)
    pending_discard_id <- reactiveVal(NULL)

    active_study <- reactive({
      req(shared_state$current_study)
      shared_state$current_study
    })
    
    # ── Helper: render a value, falling back to em-dash if NA/empty ──────────
    fmt_value <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(as.character(x))) {
        return(tags$span(class = "rids-muted-placeholder", "—"))
      }
      as.character(x)
    }
    
    # ── Look up the active study's metadata ──────────────────────────────────
    study_meta <- reactive({
      ref <- active_study()
      
      df <- rids_repos()$studies$find_meta(
        as.character(ref$cpms_id),
        as.character(ref$study_site),
        as.character(ref$scenario_id)
      )
      
      if (nrow(df) == 0) return(NULL)
      df[1, ]
    })

    template_versions <- reactive({
      version_refresh()
      ref <- active_study()
      rids_repos()$template_versions$list_for_study(
        as.character(ref$cpms_id),
        as.character(ref$study_site),
        as.character(ref$scenario_id)
      )
    })

    version_label <- function(version_type, effective_from_date) {
      if (identical(version_type, "baseline")) return("Original template")
      prefix <- if (identical(version_type, "substantial_amendment")) {
        "SUBSTANTIAL AMENDMENT"
      } else {
        "DISTRIBUTION AMENDMENT"
      }
      paste(prefix, format(as.Date(effective_from_date), "%d %b %Y"), sep = " - ")
    }

    current_available_version <- reactive({
      version_refresh()
      local_timezone <- Sys.timezone()
      if (is.na(local_timezone) || !nzchar(local_timezone)) local_timezone <- ""
      next_day <- as.POSIXct(as.character(Sys.Date() + 1), tz = local_timezone)
      milliseconds_to_midnight <- max(
        1000,
        as.numeric(difftime(next_day, Sys.time(), units = "secs")) * 1000 + 1000
      )
      invalidateLater(milliseconds_to_midnight, session)
      ref <- active_study()
      rids_repos()$template_versions$resolve_available_for_activity_date(
        as.character(ref$cpms_id),
        as.character(ref$study_site),
        as.character(ref$scenario_id),
        Sys.Date()
      )
    })
    
    posting_versions <- reactive({
      completed_template_versions(template_versions())
    })

    selected_posting_version <- reactive({
      versions <- posting_versions()
      if (is.null(versions) || nrow(versions) == 0L) return(NULL)

      selected_id <- as.character(input$posting_version_id %||% "")
      if (!selected_id %in% as.character(versions$version_id)) {
        selected_id <- default_template_version_id(versions, current_available_version())
      }
      versions[as.character(versions$version_id) == selected_id, , drop = FALSE][1, , drop = FALSE]
    })

    # ── Posting lines row count for selected template version ────────────────
    posting_count <- reactive({
      ref <- active_study()
      version <- selected_posting_version()
      if (is.null(version)) return(0L)
      
      rids_repos()$posting_lines$count_for_run(
        as.character(ref$cpms_id),
        as.character(ref$study_site),
        as.character(ref$scenario_id),
        version$version_id[[1]]
      )
    })
    
    # ── Header title ─────────────────────────────────────────────────────────
    output$workspace_title <- renderText({
      if (is.null(shared_state$current_study)) return("No study selected")
      
      meta <- study_meta()
      if (is.null(meta)) {
        ref <- active_study()
        return(paste0("CPMS ", ref$cpms_id, " · ", ref$study_site, " · Scenario ", ref$scenario_id))
      }
      
      if (is.na(meta$study_name) || !nzchar(meta$study_name)) {
        paste0("CPMS ", meta$cpms_id, " · ", meta$study_site, " · Scenario ", meta$scenario_id)
      } else {
        paste0(meta$study_name, " · CPMS ", meta$cpms_id, " · ", meta$study_site)
      }
    })
    
    # ── Body: tabs ───────────────────────────────────────────────────────────
    output$body <- renderUI({
      if (is.null(shared_state$current_study)) {
        return(p(
          class = "rids-study-state",
          "No study selected. Open one from the library."
        ))
      }
      
      meta <- study_meta()
      if (is.null(meta)) {
        ref <- active_study()
        return(p(
          class = "rids-study-state is-error",
          paste0(
            "Study not found: CPMS ",
            ref$cpms_id,
            " / ",
            ref$study_site,
            " / Scenario ",
            ref$scenario_id
          )
        ))
      }
      
      # ── Overview tab content ─────────────────────────────────────────────
      uploaded_at <- tryCatch(
        format(as.POSIXct(meta$upload_timestamp), "%d %b %Y %H:%M"),
        error = function(e) "—"
      )
      uploader_blob <- if (is.na(meta$uploaded_by) || !nzchar(meta$uploaded_by)) {
        uploaded_at
      } else {
        paste0(meta$uploaded_by, " · ", uploaded_at)
      }
      
      overview_panel <- div(
        class = "rids-study-panel",
        study_workspace_kv("CPMS ID",        fmt_value(meta$cpms_id)),
        study_workspace_kv("Study Site",     fmt_value(meta$study_site)),
        study_workspace_kv("EDGE ID",        fmt_value(meta$edge_id)),
        study_workspace_kv("Scenario",       fmt_value(meta$scenario_id)),
        study_workspace_kv("Speciality",     fmt_value(meta$speciality_name)),
        study_workspace_kv("Original file",  fmt_value(meta$original_filename)),
        study_workspace_kv("Uploaded by",    uploader_blob),
        
        hr(class = "rids-section-rule"),
        
        div(
          class = "rids-study-kv-label rids-study-notes-label",
          "Notes"
        ),
        div(
          class = "rids-study-notes",
          if (is.na(meta$notes) || !nzchar(meta$notes)) {
            tags$span(class = "rids-muted-placeholder", "No notes")
          } else {
            meta$notes
          }
        )
      )
      
      # ── Posting lines tab content ────────────────────────────────────────
      posting_version_rows <- posting_versions()
      selected_version <- selected_posting_version()
      n_posting <- posting_count()
      
      posting_panel <- div(
        class = "rids-study-panel",
        if (nrow(posting_version_rows) == 0L) {
          p(
            class = "rids-muted-placeholder",
            "No completed template versions are available for this study."
          )
        } else {
          tagList(
            p(
              class = "rids-form-copy rids-study-panel-copy",
              "Select a template version to download its posting lines as a CSV file."
            ),
            selectInput(
              ns("posting_version_id"),
              "Template version",
              choices = template_version_choices(posting_version_rows),
              selected = as.character(selected_version$version_id[[1]]),
              width = "100%"
            ),
            div(
              class = "rids-study-version-summary",
              if (!is.null(selected_version)) {
                effective <- as.Date(selected_version$effective_from_date[[1]])
                effective_label <- if (is.na(effective)) {
                  "Baseline fallback"
                } else {
                  paste("Effective from", format(effective, "%d %b %Y"))
                }
                tagList(
                  effective_label,
                  " · ",
                  toupper(selected_version$status[[1]]),
                  " · ",
                  format(n_posting, big.mark = ","),
                  " posting lines"
                )
              }
            ),
            if (n_posting == 0L) {
              p(
                class = "rids-study-warning",
                "No posting lines are available for this template version."
              )
            } else {
              downloadButton(
                ns("download_posting_lines"),
                label = "Download posting lines (CSV)",
                class = "btn-primary"
              )
            }
          )
        }
      )
      
      # ── EDGE templates tab content ───────────────────────────────────────
      available_version <- current_available_version()
      available_zip_path <- if (is.null(available_version)) NA_character_ else available_version$edge_zip_path[[1]]
      available_workbook_path <- if (is.null(available_version)) NA_character_ else available_version$saved_file_path[[1]]
      edge_zip_exists <- !is.na(available_zip_path) &&
        nzchar(available_zip_path) &&
        file.exists(available_zip_path)
      
      edge_panel <- div(
        class = "rids-study-panel",
        if (edge_zip_exists) {
          tagList(
            p(
              class = "rids-form-copy rids-study-panel-copy",
              "Download the EDGE templates that were generated for this study."
            ),
            div(
              class = "rids-study-version-summary",
              version_label(
                available_version$version_type[[1]],
                available_version$effective_from_date[[1]]
              )
            ),
            downloadButton(
              ns("download_edge_zip"),
              label = "Download EDGE templates (ZIP)",
              class = "btn-primary"
            )
          )
        } else {
          p(
            class = "rids-muted-placeholder",
            "No EDGE templates ZIP found for this study. The file may have been deleted or never generated."
          )
        }
      )
      
      # ── Files tab content ────────────────────────────────────────────────
      file_row <- function(label, path, dl_id, copy_id) {
        exists <- !is.null(path) && !is.na(path) && nzchar(path) && file.exists(path)
        
        div(
          class = "rids-file-row",
          div(
            class = "rids-file-row-header",
            div(
              class = "rids-file-row-title",
              label
            ),
            if (exists) {
              div(
                class = "rids-file-row-actions",
                downloadButton(
                  ns(dl_id),
                  label = "Download",
                  class = "btn-sm btn-outline-primary"
                ),
                actionButton(
                  ns(copy_id),
                  label = tagList(icon("copy"), " Copy path"),
                  class = "btn-sm btn-outline-secondary"
                )
              )
            } else {
              tags$span(class = "rids-muted-placeholder", "File not found")
            }
          ),
          div(
            class = "rids-file-path",
            if (is.null(path) || is.na(path) || !nzchar(path)) "—" else path
          )
        )
      }
      
      files_panel <- div(
        class = "rids-study-panel",
        p(
          class = "rids-form-copy rids-study-panel-copy",
          "Files associated with this study. Use Copy path to grab the location for use in your file explorer."
        ),
        file_row("Current ICT workbook", available_workbook_path, "download_ict", "copy_ict"),
        file_row("Current EDGE templates (ZIP)", available_zip_path, "download_zip_v2", "copy_zip")
      )

      versions <- template_versions()
      version_rows <- lapply(seq_len(nrow(versions)), function(i) {
        version <- versions[i, ]
        version_id <- version$version_id[[1]]
        effective <- if (is.na(version$effective_from_date[[1]])) {
          "Used before the first amendment"
        } else {
          paste("Effective from", format(as.Date(version$effective_from_date[[1]]), "%d %b %Y"))
        }
        uploaded <- tryCatch(
          format(as.POSIXct(version$upload_timestamp[[1]]), "%d %b %Y %H:%M"),
          error = function(e) "Unknown"
        )
        uploader <- version$uploaded_by[[1]]
        if (is.na(uploader) || !nzchar(uploader)) uploader <- "Unknown"
        active_amendments <- versions[
          versions$status == "active" &
            versions$version_type != "baseline" &
            !is.na(versions$effective_from_date) &
            as.Date(versions$effective_from_date) <= Sys.Date(),
          ,
          drop = FALSE
        ]
        can_archive <- FALSE
        if (identical(version$status[[1]], "active") && nrow(active_amendments) > 0) {
          if (identical(version$version_type[[1]], "baseline")) {
            can_archive <- TRUE
          } else {
            target_date <- as.Date(version$effective_from_date[[1]])
            can_archive <- any(
              as.Date(active_amendments$effective_from_date) > target_date |
                (as.Date(active_amendments$effective_from_date) == target_date &
                   active_amendments$version_number > version$version_number[[1]])
            )
          }
        }

        div(
          class = paste(
            c(
              "rids-version-card",
              if (identical(version$status[[1]], "archived")) "is-archived"
            ),
            collapse = " "
          ),
          div(
            class = "rids-version-card-header",
            div(
              div(
                class = "rids-version-card-title",
                version_label(version$version_type[[1]], version$effective_from_date[[1]])
              ),
              div(
                class = "rids-version-card-meta",
                paste0("Version ", version$version_number[[1]], " · ", effective,
                       " · ", toupper(version$status[[1]]))
              ),
              div(
                class = "rids-version-upload-meta",
                paste("Uploaded by", uploader, "on", uploaded)
              )
            ),
            div(
              class = "rids-version-card-actions",
              downloadButton(
                ns(paste0("download_version_workbook_", version_id)),
                "ICT workbook",
                class = "btn-sm btn-outline-primary"
              ),
              if (!is.na(version$edge_zip_path[[1]]) && nzchar(version$edge_zip_path[[1]]) &&
                  file.exists(version$edge_zip_path[[1]])) {
                downloadButton(
                  ns(paste0("download_version_zip_", version_id)),
                  "EDGE ZIP",
                  class = "btn-sm btn-outline-primary"
                )
              },
              if (isTRUE(can_archive)) {
                actionButton(
                  ns(paste0("archive_version_", version_id)),
                  "Archive",
                  class = "btn-sm btn-outline-secondary",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', %d, {priority: 'event'});",
                    ns("request_archive"), version_id
                  )
                )
              },
              if (identical(version$status[[1]], "processing")) {
                actionButton(
                  ns(paste0("discard_version_", version_id)),
                  "Discard incomplete upload",
                  class = "btn-sm btn-outline-danger",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', %d, {priority: 'event'});",
                    ns("request_discard"), version_id
                  )
                )
              }
            )
          ),
          if (!is.na(version$notes[[1]]) && nzchar(version$notes[[1]])) {
            div(class = "rids-version-card-notes", version$notes[[1]])
          }
        )
      })

      versions_panel <- div(
        class = "rids-version-panel",
        div(
          class = "rids-version-panel-header",
          p(
            class = "rids-form-copy rids-version-panel-copy",
            "Each amendment is a complete template. Activity performed before its effective date remains on the earlier applicable version."
          ),
          if (any(versions$status == "processing")) {
            span(
              class = "rids-version-processing",
              "Complete or discard the in-progress upload before adding another amendment."
            )
          } else {
            actionButton(
              ns("upload_amendment"),
              "Upload amendment",
              icon = icon("upload"),
              class = "btn-primary"
            )
          }
        ),
        tagList(version_rows)
      )
      
      # ── Tab structure ────────────────────────────────────────────────────
      bs4Card(
        title       = NULL,
        width       = 12,
        status      = "primary",
        solidHeader = FALSE,
        collapsible = FALSE,
        
        tabsetPanel(
          id = ns("workspace_tabs"),
          selected = isolate(input$workspace_tabs %||% "Overview"),
          tabPanel("Overview",        overview_panel),
          tabPanel("Posting lines",   posting_panel),
          tabPanel("EDGE templates",  edge_panel),
          tabPanel("Files",           files_panel),
          tabPanel("Template versions", versions_panel)
        )
      )
    })
    
    # ── Posting lines download ───────────────────────────────────────────────
    output$download_posting_lines <- downloadHandler(
      filename = function() {
        req(shared_state$current_study)
        meta <- study_meta()
        req(meta)
        
        cpms <- meta$cpms_id
        site <- if (!is.na(meta$study_site) && nzchar(meta$study_site)) {
          meta$study_site
        } else {
          "site"
        }
        name <- if (!is.na(meta$study_name) && nzchar(meta$study_name)) {
          gsub("[^A-Za-z0-9_-]+", "_", meta$study_name)
        } else {
          "study"
        }
        version <- selected_posting_version()
        req(version)
        version_token <- template_version_filename_token(version)
        ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0(cpms, "_", site, "_", name, "_", version_token, "_posting_lines_", ts, ".csv")
      },
      content = function(file) {
        ref <- active_study()
        version <- selected_posting_version()
        req(version)
        
        df <- rids_repos()$posting_lines$find_by_run(
          as.character(ref$cpms_id),
          as.character(ref$study_site),
          as.character(ref$scenario_id),
          version$version_id[[1]]
        )
        
        if (nrow(df) == 0) {
          showNotification("No posting lines found for this study", type = "warning")
        }
        
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # ── Original ICT download (Files tab) ────────────────────────────────────
    output$download_ict <- downloadHandler(
      filename = function() {
        version <- current_available_version()
        req(version)
        if (!is.na(version$original_filename[[1]]) && nzchar(version$original_filename[[1]])) {
          version$original_filename[[1]]
        } else {
          "ict_workbook.xlsx"
        }
      },
      content = function(file) {
        version <- current_available_version()
        req(version, !is.na(version$saved_file_path[[1]]), file.exists(version$saved_file_path[[1]]))
        file.copy(version$saved_file_path[[1]], file)
      }
    )
    
    # ── EDGE ZIP download (Files tab) ────────────────────────────────────────
    output$download_zip_v2 <- downloadHandler(
      filename = function() {
        meta <- study_meta()
        version <- current_available_version()
        req(meta, version)
        paste0(
          meta$cpms_id, "_", meta$study_site, "_", meta$scenario_id,
          "_v", version$version_number[[1]], "_edge_templates.zip"
        )
      },
      content = function(file) {
        version <- current_available_version()
        req(version, !is.na(version$edge_zip_path[[1]]), file.exists(version$edge_zip_path[[1]]))
        file.copy(version$edge_zip_path[[1]], file)
      }
    )
    
    # ── EDGE ZIP download (EDGE templates tab) ───────────────────────────────
    output$download_edge_zip <- downloadHandler(
      filename = function() {
        meta <- study_meta()
        version <- current_available_version()
        req(meta, version)
        paste0(
          meta$cpms_id, "_", meta$study_site, "_", meta$scenario_id,
          "_v", version$version_number[[1]], "_edge_templates.zip"
        )
      },
      content = function(file) {
        version <- current_available_version()
        req(version, !is.na(version$edge_zip_path[[1]]), file.exists(version$edge_zip_path[[1]]))
        file.copy(version$edge_zip_path[[1]], file)
      }
    )
    
    # ── Copy ICT path to clipboard ───────────────────────────────────────────
    observeEvent(input$copy_ict, {
      version <- current_available_version()
      req(version, !is.na(version$saved_file_path[[1]]))
      
      shinyjs::runjs(sprintf(
        "navigator.clipboard.writeText(%s);",
        jsonlite::toJSON(version$saved_file_path[[1]], auto_unbox = TRUE)
      ))
      showNotification("Path copied to clipboard", type = "message", duration = 2)
    })
    
    # ── Copy EDGE ZIP path to clipboard ──────────────────────────────────────
    observeEvent(input$copy_zip, {
      version <- current_available_version()
      req(version, !is.na(version$edge_zip_path[[1]]))
      
      shinyjs::runjs(sprintf(
        "navigator.clipboard.writeText(%s);",
        jsonlite::toJSON(version$edge_zip_path[[1]], auto_unbox = TRUE)
      ))
      showNotification("Path copied to clipboard", type = "message", duration = 2)
    })

    observe({
      versions <- template_versions()
      if (nrow(versions) == 0) return()

      lapply(seq_len(nrow(versions)), function(i) {
        local({
          version <- versions[i, ]
          version_id <- version$version_id[[1]]

          output[[paste0("download_version_workbook_", version_id)]] <- downloadHandler(
            filename = function() {
              name <- version$original_filename[[1]]
              if (is.na(name) || !nzchar(name)) "ict_workbook.xlsx" else name
            },
            content = function(file) {
              req(!is.na(version$saved_file_path[[1]]), file.exists(version$saved_file_path[[1]]))
              file.copy(version$saved_file_path[[1]], file)
            }
          )

          output[[paste0("download_version_zip_", version_id)]] <- downloadHandler(
            filename = function() {
              paste0("template_version_", version$version_number[[1]], "_edge_templates.zip")
            },
            content = function(file) {
              req(!is.na(version$edge_zip_path[[1]]), file.exists(version$edge_zip_path[[1]]))
              file.copy(version$edge_zip_path[[1]], file)
            }
          )
        })
      })
    })

    observeEvent(input$request_archive, {
      pending_archive_id(as.integer(input$request_archive))
      showModal(modalDialog(
        title = "Archive template version?",
        p("The version will be hidden from routine team use. Historical activity-date routing will remain unchanged."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_archive"), "Archive version", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_archive, {
      req(pending_archive_id())
      tryCatch({
        meta <- study_meta()
        req(meta)
        rids_repos()$template_versions$archive(
          pending_archive_id(),
          expected_study_id = meta$id[[1]]
        )
        version_refresh(version_refresh() + 1L)
        pending_archive_id(NULL)
        removeModal()
        showNotification("Template version archived", type = "message")
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 8)
      })
    })

    observeEvent(input$request_discard, {
      pending_discard_id(as.integer(input$request_discard))
      showModal(modalDialog(
        title = "Discard incomplete upload?",
        p("This removes the incomplete template version and its saved files. Completed versions are not affected."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_discard"), "Discard upload", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_discard, {
      req(pending_discard_id())
      meta <- study_meta()
      ref <- active_study()
      req(meta)

      tryCatch({
        version <- rids_repos()$template_versions$find(pending_discard_id())
        req(nrow(version) == 1L)
        if (!identical(as.integer(version$study_id[[1]]), as.integer(meta$id[[1]]))) {
          stop("Template version does not belong to the selected study.")
        }
        if (!identical(version$status[[1]], "processing")) {
          stop("Only an incomplete template version can be discarded.")
        }

        if (identical(version$version_type[[1]], "baseline")) {
          delete_study_run(
            cpms_id = as.character(ref$cpms_id),
            study_site = as.character(ref$study_site),
            scenario_id = as.character(ref$scenario_id),
            con = CON,
            delete_files = TRUE
          )
          current_refresh <- isolate(shared_state$library_refresh)
          if (is.null(current_refresh) || is.na(current_refresh)) current_refresh <- 0L
          shared_state$library_refresh <- current_refresh + 1L
          if (is.function(session$userData$reset_app_state)) {
            session$userData$reset_app_state(reset_library_refresh = FALSE)
          } else {
            shared_state$current_study <- NULL
            current_step(NULL)
          }
          removeModal()
          shinyjs::runjs('$("a[data-value=\'tab_library\']").trigger("click")')
        } else {
          rids_repos()$template_versions$discard(
            pending_discard_id(),
            expected_study_id = meta$id[[1]]
          )
          paths <- unique(c(version$saved_file_path[[1]], version$edge_zip_path[[1]]))
          paths <- paths[!is.na(paths) & nzchar(paths)]
          for (path in paths) {
            if (file.exists(path)) unlink(path, force = TRUE)
          }
          version_refresh(version_refresh() + 1L)
          removeModal()
        }

        pending_discard_id(NULL)
        showNotification("Incomplete upload discarded", type = "message")
      }, error = function(e) {
        showNotification(paste("Failed to discard upload:", conditionMessage(e)), type = "error", duration = 10)
      })
    })

    observeEvent(input$upload_amendment, {
      showModal(modalDialog(
        title = "Upload template amendment",
        size = "m",
        selectInput(
          ns("amendment_type"),
          "Amendment type",
          choices = c(
            "Substantial amendment" = "substantial_amendment",
            "Distribution amendment" = "distribution_amendment"
          )
        ),
        dateInput(
          ns("amendment_effective_date"),
          "Effective from date",
          value = Sys.Date(),
          format = "dd M yyyy"
        ),
        fileInput(
          ns("amendment_upload"),
          "Amended ICT workbook",
          multiple = FALSE,
          accept = ".xlsx"
        ),
        textAreaInput(ns("amendment_notes"), "Notes", rows = 3),
        div(
          class = "rids-form-help",
          "The workbook must contain the same CPMS ID as this study. It will be processed through the normal RIDS review flow."
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_amendment_upload"), "Upload and review", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$confirm_amendment_upload, {
      req(input$amendment_type, input$amendment_effective_date, input$amendment_upload)
      ref <- active_study()
      meta <- study_meta()
      req(meta)

      validation <- tryCatch(
        validate_ict_workbook(input$amendment_upload$datapath),
        error = function(e) list(valid = FALSE, findings = conditionMessage(e))
      )
      if (!isTRUE(validation$valid)) {
        showNotification(
          paste("Workbook validation failed:", paste(head(validation$findings, 3), collapse = "; ")),
          type = "error",
          duration = 10
        )
        return()
      }

      extracted_cpms <- tryCatch(
        as.character(extract_cpms_id(input$amendment_upload$datapath)),
        error = function(e) NA_character_
      )
      if (is.na(extracted_cpms) || !identical(
        sanitize_text_value(extracted_cpms),
        sanitize_text_value(as.character(ref$cpms_id))
      )) {
        showNotification("The amended workbook CPMS ID does not match this study", type = "error", duration = 10)
        return()
      }

      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      original_name <- gsub(
        "[/\\\\]",
        "_",
        basename(as.character(input$amendment_upload$name))
      )
      saved_path <- tempfile(
        pattern = paste0(timestamp, "_"),
        tmpdir = ICT_UPLOAD_DIR,
        fileext = paste0("_", original_name)
      )
      if (!isTRUE(file.copy(input$amendment_upload$datapath, saved_path, overwrite = FALSE))) {
        showNotification("Failed to save the amended workbook", type = "error")
        return()
      }

      version_id <- tryCatch(
        rids_repos()$template_versions$create(
          study_id = meta$id[[1]],
          version_type = input$amendment_type,
          effective_from_date = as.Date(input$amendment_effective_date),
          uploaded_by = sanitize_text_value(auth_state$username %||% auth_state$name %||% ""),
          notes = sanitize_text_value(input$amendment_notes),
          original_filename = sanitize_text_value(original_name),
          saved_file_path = sanitize_text_value(saved_path)
        ),
        error = function(e) e
      )

      if (inherits(version_id, "error")) {
        unlink(saved_path)
        showNotification(paste("Failed to create amendment:", conditionMessage(version_id)), type = "error", duration = 10)
        return()
      }

      processed <- tryCatch(
        process_workbook(
          input_path = saved_path,
          db_path = DB_DIR,
          study_site = as.character(ref$study_site),
          scenario_id = as.character(ref$scenario_id),
          version_id = version_id
        ),
        error = function(e) e
      )

      if (inherits(processed, "error")) {
        cleanup_error <- tryCatch({
          rids_repos()$template_versions$discard(
            version_id,
            expected_study_id = meta$id[[1]]
          )
          if (file.exists(saved_path)) unlink(saved_path, force = TRUE)
          NULL
        }, error = function(e) e)
        message <- paste("Failed to process amendment:", conditionMessage(processed))
        if (inherits(cleanup_error, "error")) {
          message <- paste(message, "Cleanup also failed; contact an administrator.")
        }
        showNotification(message, type = "error", duration = 10)
        return()
      }

      shared_state$cpms_id <- as.character(ref$cpms_id)
      shared_state$upload_id <- meta$id[[1]]
      shared_state$template_version_id <- version_id
      shared_state$template_version_number <- rids_repos()$template_versions$find(version_id)$version_number[[1]]
      shared_state$template_version_type <- input$amendment_type
      shared_state$template_version_effective_date <- as.Date(input$amendment_effective_date)
      shared_state$scenario_id <- as.character(ref$scenario_id)
      shared_state$study_site <- as.character(ref$study_site)
      shared_state$study_name <- meta$study_name[[1]]
      shared_state$speciality_id <- meta$speciality_id[[1]]
      shared_state$speciality_name <- meta$speciality_name[[1]]
      shared_state$mff_split_enabled <- isTRUE(meta$mff_split_enabled[[1]])
      shared_state$mff_split_pct <- as.numeric(meta$mff_split_pct[[1]] %||% 0)
      shared_state$include_screening_failure <- FALSE
      shared_state$screening_failure_arm <- NULL
      shared_state$processed_ict <- processed
      shared_state$upload_meta <- list(
        scenario_id = as.character(ref$scenario_id),
        study_site = as.character(ref$study_site),
        edge_id = meta$edge_id[[1]],
        study_name = meta$study_name[[1]],
        filename = original_name,
        raw_ict = saved_path,
        timestamp = timestamp,
        upload_id = meta$id[[1]],
        version_id = version_id,
        speciality_id = meta$speciality_id[[1]],
        speciality_name = meta$speciality_name[[1]],
        mff_split_enabled = isTRUE(meta$mff_split_enabled[[1]]),
        mff_split_pct = as.numeric(meta$mff_split_pct[[1]] %||% 0),
        include_screening_failure = FALSE,
        screening_failure_arm = NULL
      )

      version_refresh(version_refresh() + 1L)
      removeModal()
      current_step("step2")
      shinyjs::runjs('$("[data-value=\'tab_step2\']").tab("show")')
      shinyjs::runjs("$('body').addClass('sidebar-collapse')")
      showNotification("Amendment uploaded. Review the costs before generating templates.", type = "message", duration = 7)
    })
    
    # ── Back button ──────────────────────────────────────────────────────────
    observeEvent(input$back_to_library, {
      shared_state$current_study <- NULL
      shinyjs::runjs('$("a[data-value=\'tab_library\']").trigger("click")')
    })
    
  })
}
