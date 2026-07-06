step4_templates_for_export <- function(edited_templates, original_templates) {
  if (!is.null(edited_templates) && length(edited_templates) > 0) {
    return(edited_templates)
  }

  original_templates
}

step4_display_mode <- function(current_step = NULL,
                               templates = NULL,
                               validation_failed = FALSE,
                               validation_failure_latched = FALSE) {
  if (isTRUE(validation_failed) || isTRUE(validation_failure_latched)) {
    return("validation_failed")
  }

  if (!identical(current_step, "step4")) {
    return("idle")
  }

  if (is.null(templates) || length(templates) == 0) {
    return("pending")
  }

  "ready"
}

step4_effective_preview_arm <- function(selected_arm = NULL, templates = NULL) {
  if (is.null(templates) || length(templates) == 0) {
    return(NULL)
  }

  template_names <- names(templates)
  if (is.null(template_names) || length(template_names) == 0) {
    return(NULL)
  }

  if (!is.null(selected_arm) && length(selected_arm) == 1L && !is.na(selected_arm) && selected_arm %in% template_names) {
    return(selected_arm)
  }

  template_names[[1]]
}

step4_available_preview_arms <- function(templates = NULL) {
  if (is.null(templates) || length(templates) == 0) {
    return(character(0))
  }

  template_names <- names(templates)
  if (is.null(template_names) || length(template_names) == 0) {
    return(character(0))
  }

  template_names
}

# step4_UI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     bs4Card(
#       title       = "Generate EDGE Templates",
#       width       = 12,
#       status      = "primary",
#       solidHeader = FALSE,
#       footer = tagList(
#         downloadButton(ns("download_zip"), "Download ZIP", class = "btn-success"),
#         actionButton(ns("complete"), "Complete and return to library", class = "btn-primary")
#       ),
#       div(
#         style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 1rem;",
#         selectInput(ns("arm_select"), label = "Study Arm", choices = NULL, width = "200px"),
#         uiOutput(ns("save_status"))
#       ),
#       reactableOutput(ns("preview_table")),
#       
#       hr(),
#       h4("Template builder (preview)"),
#       edgeBuilderUI(ns("edge_builder"))
#     )
#   )
# }
# 
# # step4_Server <- function(id, auth_state, shared_state, current_step) {
# #   moduleServer(id, function(input, output, session) {
# #     
# #     templates <- reactiveVal(NULL)
# #     zip_path  <- reactiveVal(NULL)
# #     
# #     edited_templates <- edgeBuilderServer(
# #       id             = "edge_builder",
# #       edge_templates = reactive(shared_state$edge_templates)
# #     )
# #     
# #     w <- Waiter$new(
# #       html = tagList(
# #         div(
# #           style = "display: flex; flex-direction: column; align-items: center; gap: 1.5rem;",
# #           div(class = "green-ring"),
# #           div(
# #             style = "color: #ffffff; font-size: 1rem; font-weight: 600;",
# #             "Generating EDGE templates"
# #           ),
# #           div(
# #             style = "color: rgba(255,255,255,0.5); font-size: 0.8rem;",
# #             "This may take a moment..."
# #           )
# #         )
# #       ),
# #       color = "rgba(31, 95, 139, 0.55)"
# #     )
# #     
# #     # ── Generate templates on load ────────────────────────────────────────────
# #     observe({
# #       req(shared_state$current_step == "step4")
# #       req(shared_state$evaluated_plan)
# #       
# #       w$show()
# #       
# #       # Step 1 — adjust posting lines
# #       adjusted <- tryCatch({
# #         adjust_posting_lines(shared_state$evaluated_plan)
# #       }, error = function(e) {
# #         message("adjust_posting_lines error: ", e$message)
# #         showNotification("Failed to adjust posting lines", type = "error")
# #         w$hide()
# #         return(NULL)
# #       })
# #       
# #       req(adjusted)
# #       
# #       # Note: Fix this - needs to be renamed earlier in the pipeline. This is not clean.
# #       adjusted <- adjusted %>% 
# #         rename(Staff_Role = Staff.Role)
# #       
# #       # Step 2 — save posting lines to DB
# #       tryCatch({
# #         dbExecute(CON,
# #                   "DELETE FROM posting_lines WHERE cpms_id = ?",
# #                   params = list(as.character(shared_state$cpms_id))
# #         )
# #         dbAppendTable(CON, "posting_lines", adjusted)
# #         message("Posting lines saved to DB: ", nrow(adjusted), " rows")
# #       }, error = function(e) {
# #         message("Posting lines DB error: ", e$message)
# #         showNotification("Failed to save posting lines", type = "error")
# #       })
# #       
# #       # Step 3 — build templates
# #       tmpl <- tryCatch({
# #         build_all_edge_templates(adjusted)
# #       }, error = function(e) {
# #         message("build_all_edge_templates error: ", e$message)
# #         showNotification("Failed to build templates", type = "error")
# #         w$hide()
# #         return(NULL)
# #       })
# #       
# #       req(tmpl)
# #       templates(tmpl)
# #       shared_state$edge_templates <- tmpl
# #       
# #       # Step 4 — write CSVs and zip
# #       tryCatch({
# #         tmp_dir <- tempdir()
# #         
# #         for (arm in names(tmpl)) {
# #           csv_path <- file.path(tmp_dir, paste0(arm, ".csv"))
# #           write.csv(tmpl[[arm]], csv_path, row.names = FALSE)
# #         }
# #         
# #         timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
# #         zip_name  <- paste0(shared_state$cpms_id, "_", timestamp, ".zip")
# #         zp        <- file.path(EDGE_OUTPUT_DIR, zip_name)
# #         
# #         if (!dir.exists(EDGE_OUTPUT_DIR)) dir.create(EDGE_OUTPUT_DIR, recursive = TRUE)
# #         
# #         zip(zp, files = file.path(tmp_dir, paste0(names(tmpl), ".csv")), flags = "-j")
# #         zip_path(zp)
# #         
# #       }, error = function(e) {
# #         message("Zip error: ", e$message)
# #         showNotification("Failed to save ZIP", type = "error")
# #       })
# #       
# #       updateSelectInput(session, "arm_select", choices = names(tmpl))
# #       
# #       w$hide()
# #       showNotification("Templates generated successfully", type = "message", duration = 5)
# #     })
# #     
# #     # ── Preview selected arm ──────────────────────────────────────────────────
# #     output$preview_table <- renderReactable({
# #       req(templates())
# #       req(input$arm_select)
# #       req(input$arm_select %in% names(templates()))
# #       
# #       df <- templates()[[input$arm_select]]
# #       
# #       reactable(
# #         df,
# #         columns = list(
# #           Department = colDef(show = FALSE)
# #         ),
# #         striped       = TRUE,
# #         highlight     = TRUE,
# #         compact       = TRUE,
# #         rownames      = FALSE,
# #         pagination    = FALSE,
# #         height        = 500,
# #         resizable     = TRUE,
# #         wrap          = FALSE,
# #         defaultColDef = colDef(minWidth = 120)
# #       )
# #     })
# #     
# #     # ── Save status ───────────────────────────────────────────────────────────
# #     output$save_status <- renderUI({
# #       req(zip_path())
# #       div(
# #         style = "display: flex; align-items: center; gap: 0.5rem;",
# #         span(style = "color: #28a745; font-size: 1.2rem;", "✓"),
# #         span(
# #           style = "font-size: 0.85rem; color: #697786;",
# #           paste0("Saved to: ", zip_path())
# #         )
# #       )
# #     })
# #     
# #     # ── Download ZIP ──────────────────────────────────────────────────────────
# #     output$download_zip <- downloadHandler(
# #       filename = function() {
# #         paste0(shared_state$cpms_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
# #       },
# #       content = function(file) {
# #         req(zip_path())
# #         file.copy(zip_path(), file)
# #       }
# #     )
# #     
# #   })
# # }
# 
# step4_Server <- function(id, auth_state, shared_state, current_step) {
#   moduleServer(id, function(input, output, session) {
#     
#     templates <- reactiveVal(NULL)
#     zip_path  <- reactiveVal(NULL)
#     
#     # ── Reset helper for shared_state ──────────────────────────────────────────
#     reset_shared_state <- function() {
#       shared_state$scenario_id      <- NULL
#       shared_state$edge_id          <- NULL
#       shared_state$cpms_id          <- NULL
#       shared_state$filename         <- NULL
#       shared_state$upload_meta      <- NULL
#       shared_state$raw_ict          <- NULL
#       shared_state$posting_plan     <- NULL
#       shared_state$processed_ict    <- NULL
#       shared_state$evaluated_plan   <- NULL
#       shared_state$edge_templates   <- NULL
#       shared_state$speciality_id    <- NULL
#       shared_state$speciality_name  <- NULL
#       shared_state$current_step     <- NULL
#       shared_state$timestamp        <- NULL
#     }
#     
#     templates <- reactiveVal(NULL)
#     zip_path  <- reactiveVal(NULL)
#     
#     # ── Edge template builder module ─────────────────────────────────────────
#     edited_templates <- edgeBuilderServer(
#       id             = "edge_builder",
#       edge_templates = reactive(shared_state$edge_templates)
#     )
#     
#     w <- Waiter$new(
#       html = tagList(
#         div(
#           style = "display: flex; flex-direction: column; align-items: center; gap: 1.5rem;",
#           div(class = "green-ring"),
#           div(
#             style = "color: #ffffff; font-size: 1rem; font-weight: 600;",
#             "Generating EDGE templates"
#           ),
#           div(
#             style = "color: rgba(255,255,255,0.5); font-size: 0.8rem;",
#             "This may take a moment..."
#           )
#         )
#       ),
#       color = "rgba(31, 95, 139, 0.55)"
#     )
#     
#     # ── Helpers ──────────────────────────────────────────────────────────────
#     
#     prepare_for_export <- function(tpls) {
#       Filter(function(d) !is.null(d) && nrow(d) > 0, tpls)
#     }
#     
#     # Department is internal-only — drives builder read-only logic.
#     # EDGE expects it blank on import, and the top preview represents the export.
#     blank_department <- function(tpls) {
#       lapply(tpls, function(d) {
#         if ("Department" %in% names(d)) d$Department <- NA
#         d
#       })
#     }
#     
#     # prepare_for_export <- function(tpls) {
#     #   Filter(function(d) !is.null(d) && nrow(d) > 0, tpls)
#     # }
#     
#     write_zip <- function(tpls, zp) {
#       
#       tpls <- prepare_for_export(tpls)
#       if (length(tpls) == 0) {
#         stop("No templates with rows to export.")
#       }
#       tpls <- blank_department(tpls)
#       
#       
#       if (length(tpls) == 0) {
#         stop("No templates with rows to export.")
#       }
#       # Write CSVs locally first
#       tmp_dir <- tempfile("edge_export_")
#       dir.create(tmp_dir, recursive = TRUE)
#       on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
#       csv_files <- character(length(tpls))
#       for (i in seq_along(tpls)) {
#         nm <- names(tpls)[i]
#         # Make filename safe
#         safe_nm <- gsub("[^A-Za-z0-9_-]", "_", nm)
#         csv <- file.path(tmp_dir, paste0(safe_nm, ".csv"))
#         write.csv(
#           tpls[[i]],
#           file = csv,
#           row.names = FALSE,
#           na = ""
#         )
#         csv_files[i] <- csv
#       }
#       if (!all(file.exists(csv_files))) {
#         stop("One or more CSV files were not created before zipping.")
#       }
#       # Create ZIP locally first
#       local_zip <- tempfile("edge_zip_", fileext = ".zip")
#       zip::zipr(
#         zipfile = local_zip,
#         files   = csv_files,
#         root    = tmp_dir
#       )
#       if (!file.exists(local_zip) || file.info(local_zip)$size == 0) {
#         stop("ZIP archive was not created locally.")
#       }
#       # Copy final ZIP to network/shared output path
#       out_dir <- dirname(zp)
#       if (!dir.exists(out_dir)) {
#         dir.create(out_dir, recursive = TRUE)
#       }
#       ok <- file.copy(local_zip, zp, overwrite = TRUE)
#       if (!ok || !file.exists(zp) || file.info(zp)$size == 0) {
#         stop("ZIP was created locally but could not be copied to: ", zp)
#       }
#       invisible(zp)
#     }
#     
#     # ── Generate templates on load ────────────────────────────────────────────
#     observe({
#       req(shared_state$current_step == "step4")
#       req(shared_state$evaluated_plan)
#       
#       w$show()
#       
#       adjusted <- tryCatch({
#         adjust_posting_lines(shared_state$evaluated_plan)
#       }, error = function(e) {
#         message("adjust_posting_lines error: ", e$message)
#         showNotification("Failed to adjust posting lines", type = "error")
#         w$hide()
#         return(NULL)
#       })
#       
#       req(adjusted)
#       
#       adjusted <- adjusted %>% rename(Staff_Role = Staff.Role)
#       
#       # ── Attach cost centres ──────────────────────────────────────────────────
#       adjusted <- tryCatch({
#         add_cost_centres(adjusted, isolate(shared_state$speciality_name))
#       }, error = function(e) {
#         message("add_cost_centres error: ", conditionMessage(e))
#         showNotification(
#           paste("Failed to assign cost centres:", conditionMessage(e)),
#           type = "error",
#           duration = 10
#         )
#         return(adjusted)
#       })
#       
#       adjusted <- tryCatch({
#         assign_edge_keys(adjusted)
#       }, error = function(e) {
#         message("assign_edge_keys error: ", conditionMessage(e))
#         showNotification(
#           paste("Failed to assign EDGE keys:", conditionMessage(e)),
#           type = "error",
#           duration = 10
#         )
#         return(adjusted)
#       })
#       
#       tryCatch({
#         dbExecute(CON,
#                   "DELETE FROM posting_lines WHERE cpms_id = ?",
#                   params = list(as.character(shared_state$cpms_id))
#         )
#         dbAppendTable(CON, "posting_lines", adjusted)
#         message("Posting lines saved to DB: ", nrow(adjusted), " rows")
#       }, error = function(e) {
#         message("Posting lines DB error: ", e$message)
#         showNotification("Failed to save posting lines", type = "error")
#       })
#       
#       tmpl <- tryCatch({
#       
#         visit_lookup <- dbGetQuery(CON, "
#                         SELECT DISTINCT Study, Study_Arm, Visit_Label, Visit_Number
#                         FROM ict_costing_tbl
#                         WHERE Visit_Label IS NOT NULL
#                       ")
#         
#         templates <- build_all_edge_templates(adjusted, visit_lookup, shared_state$upload_meta$edge_id)
#         
#       }, error = function(e) {
#         message("build_all_edge_templates error: ", e$message)
#         showNotification("Failed to build templates", type = "error")
#         w$hide()
#         return(NULL)
#       })
#       
#       req(tmpl)
#       templates(tmpl)
#       shared_state$edge_templates <- tmpl
#       
#       # Initial ZIP write — uses original templates (user hasn't touched yet).
#       # The download handler regenerates from edited_templates() on click.
#       tryCatch({
#         timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
#         zip_name  <- paste0(shared_state$cpms_id, "_", timestamp, ".zip")
#         zp        <- file.path(EDGE_OUTPUT_DIR, zip_name)
#         
#         if (!dir.exists(EDGE_OUTPUT_DIR)) dir.create(EDGE_OUTPUT_DIR, recursive = TRUE)
#         
#         write_zip(tmpl, zp)
#         zip_path(zp)
#         
#         # Persist the ZIP path to meta_data for this study
#         dbExecute(CON,
#                   "UPDATE meta_data SET edge_zip_path = ? WHERE cpms_id = ?",
#                   params = list(zp, as.character(shared_state$cpms_id))
#         )
#         
#       }, error = function(e) {
#         message("Zip error: ", e$message)
#         showNotification("Failed to save ZIP", type = "error")
#       })
#       
#       updateSelectInput(session, "arm_select", choices = names(tmpl))
#       
#       w$hide()
#       showNotification("Templates generated successfully", type = "message", duration = 5)
#     })
#     
#     # ── Preview selected arm ──────────────────────────────────────────────────
#     # output$preview_table <- renderReactable({
#     #   req(templates())
#     #   req(input$arm_select)
#     #   req(input$arm_select %in% names(templates()))
#     #   
#     #   df <- templates()[[input$arm_select]]
#     #   
#     #   reactable(
#     #     df,
#     #     columns = list(
#     #       Department = colDef(show = FALSE)
#     #     ),
#     #     striped       = TRUE,
#     #     highlight     = TRUE,
#     #     compact       = TRUE,
#     #     rownames      = FALSE,
#     #     pagination    = FALSE,
#     #     height        = 500,
#     #     resizable     = TRUE,
#     #     wrap          = FALSE,
#     #     defaultColDef = colDef(minWidth = 120)
#     #   )
#     # })
#     output$preview_table <- renderReactable({
#       req(input$arm_select)
#       
#       tpls <- edited_templates()
#       if (is.null(tpls) || length(tpls) == 0) tpls <- templates()
#       
#       req(tpls, input$arm_select %in% names(tpls))
#       
#       df <- blank_department(tpls)[[input$arm_select]]
#       
#       reactable(
#         df,
#         striped       = TRUE,
#         highlight     = TRUE,
#         compact       = TRUE,
#         rownames      = FALSE,
#         pagination    = FALSE,
#         height        = 500,
#         resizable     = TRUE,
#         wrap          = FALSE,
#         defaultColDef = colDef(minWidth = 120)
#       )
#     })
#     
#     # ── Save status ───────────────────────────────────────────────────────────
#     output$save_status <- renderUI({
#       req(zip_path())
#       div(
#         style = "display: flex; align-items: center; gap: 0.5rem;",
#         span(style = "color: #28a745; font-size: 1.2rem;", "✓"),
#         span(
#           style = "font-size: 0.85rem; color: #697786;",
#           paste0("Saved to: ", zip_path())
#         )
#       )
#     })
#     
#     # ── Download ZIP (rebuilds from edited templates on click) ───────────────
#     output$download_zip <- downloadHandler(
#       filename = function() {
#         paste0(shared_state$cpms_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
#       },
#       contentType = "application/zip",
#       content = function(file) {
#         tpls <- edited_templates()
#         if (is.null(tpls) || length(tpls) == 0) {
#           tpls <- templates()
#         }
#         
#         req(tpls)
#         
#         tmp_zip <- tempfile("edge_download_", fileext = ".zip")
#         on.exit(unlink(tmp_zip), add = TRUE)
#         
#         write_zip(tpls, tmp_zip)
#         
#         ok <- file.copy(tmp_zip, file, overwrite = TRUE)
#         if (!ok || !file.exists(file)) {
#           stop("Failed to copy ZIP to the download target.")
#         }
#       }
#     )
#     
#     # ── Complete: success modal + navigate + reset ──────────────────────────
#     observeEvent(input$complete, {
#       
#       current_session <- session
#       
#       showModal(modalDialog(
#         title     = NULL,
#         footer    = NULL,
#         easyClose = FALSE,
#         size      = "s",
#         div(
#           style = "text-align: center; padding: 1.5rem 1rem;",
#           div(
#             style = paste(
#               "width: 64px;",
#               "height: 64px;",
#               "border-radius: 50%;",
#               "background: #e6f4ea;",
#               "display: flex;",
#               "align-items: center;",
#               "justify-content: center;",
#               "margin: 0 auto 1rem auto;"
#             ),
#             tags$span(
#               style = "color: #28a745; font-size: 2rem; font-weight: 700;",
#               HTML("&check;")
#             )
#           ),
#           h4(
#             style = "margin-bottom: 0.5rem; color: #1d2a36;",
#             "Study processed successfully"
#           ),
#           p(
#             style = "color: #697786; margin-bottom: 0;",
#             "Opening the study library..."
#           )
#         )
#       ))
#       
#       later::later(function() {
#         shiny::withReactiveDomain(current_session, {
#           removeModal()
#           templates(NULL)
#           zip_path(NULL)
#           reset_shared_state()
#           current_step(NULL)
#           shinyjs::runjs('$("a[data-value=\'tab_library\']").trigger("click")')
#           shinyjs::runjs("$('body').addClass('sidebar-collapse')")
#         })
#       }, delay = 2)
#     })
#     
#     # ── Disable Complete until templates exist ──────────────────────────────
#     observe({
#       shinyjs::toggleState("complete", condition = !is.null(templates()))
#     })
#     
#   })
# }

step4_UI <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Card(
      title       = "Generate EDGE Templates",
      width       = 12,
      status      = "primary",
      solidHeader = FALSE,
      footer = uiOutput(ns("step4_footer")),
      uiOutput(ns("step4_body"))
    ),
    shinyjs::hidden(
      div(
        id = ns("complete_overlay"),
        build_loading_state_overlay(
          title = "Study processed successfully",
          subtitle = "Opening the study library...",
          status = "success"
        )
      )
    )
  )
}

step4_Server <- function(id, auth_state, shared_state, current_step) {
  moduleServer(id, function(input, output, session) {
    study_identity_params <- function() {
      list(
        as.character(shared_state$cpms_id),
        as.character(shared_state$study_site),
        as.character(shared_state$scenario_id)
      )
    }
    
    templates <- reactiveVal(NULL)
    zip_path  <- reactiveVal(NULL)
    unmatched_cost_centres <- reactiveVal(NULL)
    unmatched_cost_centres_summary <- reactiveVal(tibble::tibble())
    validation_failed <- reactiveVal(FALSE)
    rollback_failed_message <- reactiveVal(NULL)
    validation_failure_latched <- reactiveVal(FALSE)
    current_display_mode <- reactive({
      step4_display_mode(
        current_step = shared_state$current_step,
        templates = templates(),
        validation_failed = validation_failed(),
        validation_failure_latched = validation_failure_latched()
      )
    })
    current_preview_templates <- reactive({
      tpls <- edited_templates()
      if (is.null(tpls) || length(tpls) == 0) {
        tpls <- templates()
      }
      tpls
    })
    
    # ── Edge template builder module ─────────────────────────────────────────
    edited_templates <- edgeBuilderServer(
      id             = "edge_builder",
      edge_templates = reactive(shared_state$edge_templates)
    )
    
    # ── ADDON ── custom activities module ─────────────────────────────────
    custom_activity_handles <- customActivityServer(
      id                = "custom_activities",
      auth_state        = auth_state,
      shared_state      = shared_state,
      study_arm_choices = reactive({
        tpl <- templates()
        if (is.null(tpl)) character(0) else names(tpl)
      })
    )
    # ──────────────────────────────────────────────────────────────────────
    
    w <- Waiter$new(
      html = build_loading_state_overlay("Generating EDGE templates"),
      color = "transparent"
    )
    
    # ── Helpers ──────────────────────────────────────────────────────────────
    
    prepare_for_export <- function(tpls) {
      Filter(function(d) !is.null(d) && nrow(d) > 0, tpls)
    }

    clear_cost_centre_failure <- function() {
      unmatched_cost_centres(NULL)
      unmatched_cost_centres_summary(tibble::tibble())
      validation_failed(FALSE)
      rollback_failed_message(NULL)
      validation_failure_latched(FALSE)
    }

    set_cost_centre_failure <- function(unmatched_report, rollback_message = NULL) {
      unmatched_cost_centres(unmatched_report)
      unmatched_cost_centres_summary(summarize_unmatched_cost_centres(unmatched_report))
      validation_failed(TRUE)
      rollback_failed_message(rollback_message)
      validation_failure_latched(TRUE)
    }

    build_cost_centre_error_report <- function(message_text) {
      tibble::tibble(
        Department = "Configuration error",
        activity_type = trimws(as.character(message_text %||% "Unknown cost centre error")),
        Staff_Role = NA_character_,
        posting_line_type_id = NA_character_
      )
    }

    summarize_unmatched_cost_centres <- function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(tibble::tibble())
      }

      df %>%
        count(
          .data$Department,
          .data$activity_type,
          .data$Staff_Role,
          .data$posting_line_type_id,
          name = "occurrence_count"
        ) %>%
        arrange(
          .data$Department,
          .data$activity_type,
          .data$Staff_Role,
          .data$posting_line_type_id
        )
    }
    
    # Department is internal-only — drives builder read-only logic.
    # EDGE expects it blank on import, and the top preview represents the export.
    blank_department <- function(tpls) {
      lapply(tpls, function(d) {
        if ("Department" %in% names(d)) d$Department <- NA
        d
      })
    }
    
    write_zip <- function(tpls, zp) {
      
      tpls <- prepare_for_export(tpls)
      if (length(tpls) == 0) {
        stop("No templates with rows to export.")
      }
      tpls <- blank_department(tpls)
      
      
      if (length(tpls) == 0) {
        stop("No templates with rows to export.")
      }
      # Write CSVs locally first
      tmp_dir <- tempfile("edge_export_")
      dir.create(tmp_dir, recursive = TRUE)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
      csv_files <- character(length(tpls))
      for (i in seq_along(tpls)) {
        nm <- names(tpls)[i]
        # Make filename safe
        safe_nm <- gsub("[^A-Za-z0-9_-]", "_", nm)
        csv <- file.path(tmp_dir, paste0(safe_nm, ".csv"))
        write.csv(
          tpls[[i]],
          file = csv,
          row.names = FALSE,
          na = ""
        )
        csv_files[i] <- csv
      }
      if (!all(file.exists(csv_files))) {
        stop("One or more CSV files were not created before zipping.")
      }
      # Create ZIP locally first
      local_zip <- tempfile("edge_zip_", fileext = ".zip")
      zip::zipr(
        zipfile = local_zip,
        files   = csv_files,
        root    = tmp_dir
      )
      if (!file.exists(local_zip) || file.info(local_zip)$size == 0) {
        stop("ZIP archive was not created locally.")
      }
      # Copy final ZIP to network/shared output path
      out_dir <- dirname(zp)
      if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE)
      }
      ok <- file.copy(local_zip, zp, overwrite = TRUE)
      if (!ok || !file.exists(zp) || file.info(zp)$size == 0) {
        stop("ZIP was created locally but could not be copied to: ", zp)
      }
      invisible(zp)
    }

    log_step4_event <- function(level, message, details = list()) {
      if (identical(level, "INFO")) {
        app_log_info("step4", message)
      }
    }

    output$step4_footer <- renderUI({
      if (!identical(current_display_mode(), "ready")) {
        return(NULL)
      }

      tagList(
        downloadButton(session$ns("download_zip"), "Download ZIP", class = "btn-success"),
        actionButton(session$ns("complete"), "Complete and return to library", class = "btn-primary")
      )
    })

    output$step4_body <- renderUI({
      display_mode <- current_display_mode()

      if (identical(display_mode, "validation_failed")) {
        return(tagList(
          uiOutput(session$ns("cost_centre_validation_panel"))
        ))
      }

      if (!identical(display_mode, "ready")) {
        return(
          div(
            style = paste(
              "padding: 1rem;",
              "border-radius: 6px;",
              "background: #f7f9fb;",
              "color: #697786;"
            ),
            div(
              style = "font-weight: 600; color: #1d2a36; margin-bottom: 0.35rem;",
              "Preparing template output"
            ),
            div(
              "RIDS is still preparing this study for template generation.",
              "If cost centre validation fails, the failure report will appear here."
            )
          )
        )
      }

      tagList(
        div(
          style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 1rem;",
          selectInput(
            session$ns("arm_select"),
            label = "Study Arm",
            choices = step4_available_preview_arms(current_preview_templates()),
            selected = step4_effective_preview_arm(input$arm_select, current_preview_templates()),
            width = "200px"
          ),
          uiOutput(session$ns("save_status"))
        ),
        reactableOutput(session$ns("preview_table")),
        hr(),
        h4("Template builder (preview)"),
        edgeBuilderUI(session$ns("edge_builder")),
        hr(),
        customActivityUI(session$ns("custom_activities"))
      )
    })

    output$cost_centre_validation_panel <- renderUI({
      unmatched <- unmatched_cost_centres()
      unmatched_summary <- unmatched_cost_centres_summary()

      if (!isTRUE(validation_failed()) &&
          (is.null(unmatched) || !is.data.frame(unmatched) || nrow(unmatched) == 0)) {
        return(NULL)
      }

      rollback_message <- rollback_failed_message()

      tagList(
        div(
          style = paste(
            "margin-bottom: 1rem;",
            "padding: 0.9rem 1rem;",
            "border-radius: 6px;",
            "background: #fff4f2;"
          ),
          div(
            style = "font-weight: 600; color: #8e2f23; margin-bottom: 0.35rem;",
            "Cost centre matrix validation failed"
          ),
          div(
            style = "color: #8e2f23;",
            "The study was not processed. Review the unmatched conditions below, download the report if needed, then start again from step 1."
          ),
          if (!is.null(rollback_message)) {
            div(
              style = "margin-top: 0.5rem; font-weight: 600; color: #8e2f23;",
              rollback_message
            )
          },
          div(
            style = "margin-top: 0.75rem;",
            if (is.data.frame(unmatched) && nrow(unmatched) > 0) {
              downloadButton(
                session$ns("download_unmatched_cost_centres"),
                "Download unmatched conditions CSV",
                class = "btn-outline-danger btn-sm"
              )
            }
          )
        ),
        if (is.data.frame(unmatched_summary) && nrow(unmatched_summary) > 0) {
          reactableOutput(session$ns("unmatched_cost_centres_table"))
        }
      )
    })

    observeEvent(shared_state$current_step, {
      if (!identical(shared_state$current_step, "step4")) {
        clear_cost_centre_failure()
      }
    }, ignoreInit = TRUE)
    
    # ── Generate templates on load ────────────────────────────────────────────
    observe({
      req(shared_state$current_step == "step4")
      req(shared_state$evaluated_plan)
      
      # Re-trigger when customs change so templates rebuild with/without them.
      # First entry: signal is 0; addon wipes; signal bumps to 1 → this observer
      # runs once more with customs cleared (no-op effectively).
      custom_activity_handles$invalidation_signal()
      if (isTRUE(validation_failure_latched())) {
        return(invisible(NULL))
      }

      clear_cost_centre_failure()
      templates(NULL)
      zip_path(NULL)
      shared_state$edge_templates <- NULL
      
      w$show()

      log_step4_event(
        level = "INFO",
        message = "Template generation started",
        details = list(scenario_id = shared_state$scenario_id)
      )
      
      adjusted <- tryCatch({
        adjust_posting_lines(shared_state$evaluated_plan)
      }, error = function(e) {
        if (handle_fatal_db_error(session, e, "step4", list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          stage = "adjust_posting_lines"
        ))) {
          w$hide()
          return(NULL)
        }

        app_log_exception("step4", "Adjust posting lines failed", e, list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id
        ))
        log_step4_event(
          level = "ERROR",
          message = "Posting line generation failed",
          details = list(
            stage = "adjust_posting_lines",
            error = conditionMessage(e)
          )
        )
        showNotification("Failed to adjust posting lines", type = "error")
        w$hide()
        return(NULL)
      })
      
      req(adjusted)
      adjusted$study_site <- shared_state$study_site
      
      adjusted <- adjusted %>% rename(Staff_Role = Staff.Role)
      
      # ── Attach cost centres ──────────────────────────────────────────────────
      adjusted <- tryCatch({
        add_cost_centres(adjusted, isolate(shared_state$speciality_name))
      }, error = function(e) {
        error_message <- conditionMessage(e)
        set_cost_centre_failure(
          build_cost_centre_error_report(error_message),
          rollback_message = paste(
            "Cost centre assignment could not run.",
            "Review the report below, then start again from step 1."
          )
        )
        templates(NULL)
        zip_path(NULL)
        shared_state$edge_templates <- NULL
        app_log_exception("step4", "Cost centre assignment failed", e, list(
          cpms_id = shared_state$cpms_id,
          speciality = isolate(shared_state$speciality_name)
        ))
        w$hide()
        return(NULL)
      })

      req(adjusted)

      cc_assignment_summary <- attr(adjusted, "cost_centre_assignment_summary")
      cc_unmatched_report <- attr(adjusted, "cost_centre_unmatched_report")
      log_step4_event(
        level = "INFO",
        message = "Cost centre assignment completed",
        details = list(
          matched_rows = cc_assignment_summary$matched_rows %||% NA_integer_,
          unmatched_rows = cc_assignment_summary$unmatched_rows %||% NA_integer_
        )
      )

      if (!is.null(cc_unmatched_report) && nrow(cc_unmatched_report) > 0) {
        unmatched_summary <- summarize_unmatched_cost_centres(cc_unmatched_report)
        set_cost_centre_failure(cc_unmatched_report)
        app_log_exception(
          "step4",
          "Cost centre matrix validation failed",
          simpleError("Unmatched cost centre conditions detected"),
          list(
            cpms_id = shared_state$cpms_id,
            study_site = shared_state$study_site,
            scenario_id = shared_state$scenario_id,
            unmatched_rows = nrow(cc_unmatched_report),
            unmatched_conditions = nrow(unmatched_summary)
          )
        )

        rollback_result <- tryCatch({
          delete_study_run(
            cpms_id = as.character(shared_state$cpms_id),
            study_site = as.character(shared_state$study_site),
            scenario_id = as.character(shared_state$scenario_id),
            con = CON,
            delete_files = TRUE
          )
        }, error = function(e) {
          e
        })

        templates(NULL)
        zip_path(NULL)
        shared_state$edge_templates <- NULL

        if (inherits(rollback_result, "error")) {
          rollback_message <- paste(
            "Cleanup also failed, so some study data may still exist.",
            "Please contact an administrator."
          )
          set_cost_centre_failure(cc_unmatched_report, rollback_message)
          app_log_exception(
            "step4",
            "Study rollback failed after cost centre validation failure",
            rollback_result,
            list(
              cpms_id = shared_state$cpms_id,
              study_site = shared_state$study_site,
              scenario_id = shared_state$scenario_id,
              unmatched_rows = nrow(cc_unmatched_report),
              unmatched_conditions = nrow(unmatched_summary)
            )
          )
          showNotification(
            "Cost centre matrix validation failed and cleanup did not complete. Please contact an administrator.",
            type = "error",
            duration = 12
          )
        } else {
          showNotification(
            paste(
              "Cost centre matrix validation failed.",
              nrow(unmatched_summary),
              "unsupported condition(s) were found. The study was deleted and must be started again."
            ),
            type = "error",
            duration = 12
          )
        }

        w$hide()
        return(NULL)
      }
      
      adjusted <- tryCatch({
        assign_edge_keys(adjusted)
      }, error = function(e) {
        app_log_exception("step4", "EDGE key assignment failed", e, list(
          cpms_id = shared_state$cpms_id
        ))
        showNotification(
          paste("Failed to assign EDGE keys:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        return(adjusted)
      })
      
      # ── ADDON ── merge custom activities before persist + template build ──
      adjusted <- tryCatch({
        apply_custom_activities(adjusted, shared_state)
      }, error = function(e) {
        if (handle_fatal_db_error(session, e, "step4", list(
          cpms_id = shared_state$cpms_id,
          stage = "custom_activity_merge"
        ))) {
          w$hide()
          return(NULL)
        }

        app_log_exception("step4", "Custom activity merge failed", e, list(
          cpms_id = shared_state$cpms_id
        ))
        showNotification(
          paste("Failed to merge custom activities:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        return(adjusted)   # fall back to pipeline-only output
      })
      # ──────────────────────────────────────────────────────────────────────

      req(adjusted)
      
      persisted_ok <- tryCatch({
        identity_params <- study_identity_params()
        rids_repos()$posting_lines$replace_for_run(
          adjusted,
          identity_params[[1]],
          identity_params[[2]],
          identity_params[[3]]
        )
        log_step4_event(
          level = "INFO",
          message = "Posting lines saved",
          details = list(rows = nrow(adjusted))
        )
        TRUE
      }, error = function(e) {
        if (handle_fatal_db_error(session, e, "step4", list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(adjusted),
          stage = "posting_lines_persist"
        ))) {
          w$hide()
          return(FALSE)
        }

        app_log_exception("step4", "Posting lines persistence failed", e, list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(adjusted)
        ))
        log_step4_event(
          level = "ERROR",
          message = "Persistence failed",
          details = list(
            stage = "posting_lines_persist",
            rows = nrow(adjusted),
            error = conditionMessage(e)
          )
        )
        showNotification("Failed to save posting lines", type = "error")
        FALSE
      })

      if (!isTRUE(persisted_ok)) {
        w$hide()
        return(NULL)
      }
      
      tmpl <- tryCatch({
        
        visit_lookup <- rids_repos()$ict_costing$visit_lookup(
          as.character(shared_state$cpms_id),
          as.character(shared_state$study_site),
          as.character(shared_state$scenario_id)
        )
        
        templates <- build_all_edge_templates(adjusted, visit_lookup, shared_state$upload_meta$edge_id)
        
      }, error = function(e) {
        if (handle_fatal_db_error(session, e, "step4", list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(adjusted),
          stage = "template_build"
        ))) {
          w$hide()
          return(NULL)
        }

        app_log_exception("step4", "EDGE template build failed", e, list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(adjusted)
        ))
        log_step4_event(
          level = "ERROR",
          message = "Posting line generation failed",
          details = list(
            stage = "template_build",
            rows = nrow(adjusted),
            error = conditionMessage(e)
          )
        )
        showNotification("Failed to build templates", type = "error")
        w$hide()
        return(NULL)
      })
      
      req(tmpl)
      templates(tmpl)
      shared_state$edge_templates <- tmpl
      
      # Initial ZIP write — uses original templates (user hasn't touched yet).
      # The download handler regenerates from edited_templates() on click.
      zip_saved_ok <- tryCatch({
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        zip_name  <- paste0(
          shared_state$cpms_id,
          "_",
          shared_state$study_site,
          "_",
          shared_state$scenario_id,
          "_",
          timestamp,
          ".zip"
        )
        zp        <- file.path(EDGE_OUTPUT_DIR, zip_name)

        if (!dir.exists(EDGE_OUTPUT_DIR)) dir.create(EDGE_OUTPUT_DIR, recursive = TRUE)
        
        write_zip(tmpl, zp)
        zip_path(zp)
        
        # Persist the ZIP path to meta_data for this study
        zip_identity <- study_identity_params()
        rids_repos()$studies$set_edge_zip_path(
          zp,
          zip_identity[[1]],
          zip_identity[[2]],
          zip_identity[[3]]
        )

        log_step4_event(
          level = "INFO",
          message = "ZIP save completed",
          details = list(
            template_count = length(tmpl),
            zip_name = zip_name
          )
        )
        TRUE
        
      }, error = function(e) {
        if (handle_fatal_db_error(session, e, "step4", list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          template_count = length(tmpl),
          stage = "zip_generation"
        ))) {
          w$hide()
          return(FALSE)
        }

        app_log_exception("step4", "ZIP generation failed", e, list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          template_count = length(tmpl)
        ))
        log_step4_event(
          level = "ERROR",
          message = "Persistence failed",
          details = list(
            stage = "zip_generation",
            template_count = length(tmpl),
            error = conditionMessage(e)
          )
        )
        showNotification("Failed to save ZIP", type = "error")
        FALSE
      })

      if (!isTRUE(zip_saved_ok)) {
        w$hide()
        return(NULL)
      }

      app_log_info("step4", "Template generation completed")
      
      w$hide()
      showNotification("Templates generated successfully", type = "message", duration = 5)
    })
    
    # ── Preview selected arm ──────────────────────────────────────────────────
    output$preview_table <- renderReactable({
      req(identical(current_display_mode(), "ready"))
      
      tpls <- current_preview_templates()

      preview_arm <- step4_effective_preview_arm(input$arm_select, tpls)
      req(preview_arm)

      df <- blank_department(tpls)[[preview_arm]]
      
      reactable(
        df,
        striped       = TRUE,
        highlight     = TRUE,
        compact       = TRUE,
        rownames      = FALSE,
        pagination    = FALSE,
        height        = 500,
        resizable     = TRUE,
        wrap          = FALSE,
        defaultColDef = colDef(minWidth = 120)
      )
    })

    output$unmatched_cost_centres_table <- renderReactable({
      unmatched_summary <- unmatched_cost_centres_summary()
      req(nrow(unmatched_summary) > 0)

      reactable(
        unmatched_summary,
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        rownames = FALSE,
        defaultPageSize = 10,
        pagination = TRUE,
        columns = list(
          Department = colDef(name = "Department"),
          activity_type = colDef(name = "Activity Type"),
          Staff_Role = colDef(name = "Staff Role"),
          posting_line_type_id = colDef(name = "Split Type"),
          occurrence_count = colDef(name = "Count")
        )
      )
    })
    
    # ── Save status ───────────────────────────────────────────────────────────
    output$save_status <- renderUI({
      req(identical(current_display_mode(), "ready"))
      req(zip_path())
      div(
        style = "display: flex; align-items: center; gap: 0.5rem;",
        span(style = "color: #28a745; font-size: 1.2rem;", "✓"),
        span(
          style = "font-size: 0.85rem; color: #697786;",
          paste0("Saved to: ", zip_path())
        )
      )
    })
    
    # ── Download ZIP (rebuilds from edited templates on click) ───────────────
    output$download_zip <- downloadHandler(
      filename = function() {
        paste0(shared_state$cpms_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      contentType = "application/zip",
      content = function(file) {
        tpls <- step4_templates_for_export(edited_templates(), templates())
        
        req(tpls)
        
        tmp_zip <- tempfile("edge_download_", fileext = ".zip")
        on.exit(unlink(tmp_zip), add = TRUE)
        
        write_zip(tpls, tmp_zip)
        
        ok <- file.copy(tmp_zip, file, overwrite = TRUE)
        if (!ok || !file.exists(file)) {
          stop("Failed to copy ZIP to the download target.")
        }
      }
    )

    output$download_unmatched_cost_centres <- downloadHandler(
      filename = function() {
        paste0(
          shared_state$cpms_id,
          "_",
          shared_state$study_site,
          "_",
          shared_state$scenario_id,
          "_unmatched_cost_centres.csv"
        )
      },
      contentType = "text/csv",
      content = function(file) {
        unmatched <- unmatched_cost_centres()
        req(identical(current_display_mode(), "validation_failed"))
        req(is.data.frame(unmatched), nrow(unmatched) > 0)
        write.csv(unmatched, file = file, row.names = FALSE, na = "")
      }
    )
    
    # ── Complete: success modal + navigate + reset ──────────────────────────
    observeEvent(input$complete, {
      req(identical(current_display_mode(), "ready"))

      final_templates <- step4_templates_for_export(edited_templates(), templates())
      req(final_templates)

      current_zip_path <- zip_path()
      req(current_zip_path)

      shinyjs::show("complete_overlay")

      zip_saved_ok <- tryCatch({
        write_zip(final_templates, current_zip_path)
        log_step4_event(
          level = "INFO",
          message = "Final amended ZIP save completed",
          details = list(
            template_count = length(final_templates),
            zip_path = current_zip_path
          )
        )
        TRUE
      }, error = function(e) {
        if (handle_fatal_db_error(session, e, "step4", list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          template_count = length(final_templates),
          stage = "final_zip_persist"
        ))) {
          shinyjs::hide("complete_overlay")
          return(FALSE)
        }

        app_log_exception("step4", "Final amended ZIP save failed", e, list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          template_count = length(final_templates),
          zip_path = current_zip_path
        ))
        showNotification(
          "Failed to save the amended EDGE templates. Please try again before returning to the library.",
          type = "error",
          duration = 10
        )
        shinyjs::hide("complete_overlay")
        FALSE
      })

      if (!isTRUE(zip_saved_ok)) {
        return()
      }

      current_session <- session
      later::later(function() {
        shiny::withReactiveDomain(current_session, {
          shinyjs::hide("complete_overlay")
          templates(NULL)
          zip_path(NULL)
          current_refresh <- isolate(shared_state$library_refresh)
          if (is.null(current_refresh) || is.na(current_refresh)) current_refresh <- 0L
          shared_state$library_refresh <- current_refresh + 1L
          if (is.function(session$userData$reset_app_state)) {
            invoke_reset_app_state(
              session$userData$reset_app_state,
              reset_library_refresh = FALSE
            )
          } else {
            current_step(NULL)
          }
          shinyjs::runjs('$("a[data-value=\'tab_library\']").trigger("click")')
          shinyjs::runjs("$('body').addClass('sidebar-collapse')")
        })
      }, delay = 2)
    })
    
    # ── Disable Complete until templates exist ──────────────────────────────
    observe({
      shinyjs::toggleState("complete", condition = identical(current_display_mode(), "ready"))
    })
    
  })
}
