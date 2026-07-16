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

step4_filter_export_templates <- function(templates) {
  Filter(function(template) !is.null(template) && nrow(template) > 0, templates)
}

# Department is internal-only — drives builder read-only logic.
# EDGE expects it blank on import, and the top preview represents the export.
step4_blank_export_departments <- function(templates) {
  lapply(templates, function(template) {
    if ("Department" %in% names(template)) template$Department <- NA
    template
  })
}

step4_apply_amendment_export_rules <- function(templates,
                                                version_type,
                                                effective_from_date,
                                                version_number) {
  templates <- suffix_amendment_template_names(
    templates,
    version_type = version_type,
    effective_from_date = effective_from_date
  )

  qualify_amendment_analysis_codes(
    templates,
    version_type = version_type,
    version_number = version_number
  )
}

step4_write_export_zip <- function(templates,
                                   zip_path,
                                   version_type,
                                   effective_from_date,
                                   version_number) {
  templates <- step4_filter_export_templates(templates)
  if (length(templates) == 0) {
    stop("No templates with rows to export.")
  }

  templates <- step4_apply_amendment_export_rules(
    templates,
    version_type = version_type,
    effective_from_date = effective_from_date,
    version_number = version_number
  )
  templates <- step4_blank_export_departments(templates)

  if (length(templates) == 0) {
    stop("No templates with rows to export.")
  }

  tmp_dir <- tempfile("edge_export_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  csv_files <- character(length(templates))
  for (i in seq_along(templates)) {
    template_name <- names(templates)[i]
    safe_name <- edge_template_export_stem(
      template_name,
      version_type = version_type
    )
    csv_path <- file.path(tmp_dir, paste0(safe_name, ".csv"))
    write.csv(
      templates[[i]],
      file = csv_path,
      row.names = FALSE,
      na = ""
    )
    csv_files[i] <- csv_path
  }

  if (!all(file.exists(csv_files))) {
    stop("One or more CSV files were not created before zipping.")
  }

  local_zip <- tempfile("edge_zip_", fileext = ".zip")
  zip::zipr(
    zipfile = local_zip,
    files = csv_files,
    root = tmp_dir
  )
  if (!file.exists(local_zip) || file.info(local_zip)$size == 0) {
    stop("ZIP archive was not created locally.")
  }

  output_dir <- dirname(zip_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  copied <- file.copy(local_zip, zip_path, overwrite = TRUE)
  if (!copied || !file.exists(zip_path) || file.info(zip_path)$size == 0) {
    stop("ZIP was created locally but could not be copied to: ", zip_path)
  }

  invisible(zip_path)
}

step4_UI <- function(id) {
  ns <- NS(id)
  div(
    class = "rids-page rids-workflow-page",
    div(class = "rids-page-header rids-workflow-header", div(div(class = "rids-page-eyebrow", "ICT processing · Step 4 of 4"), h1("Generate outputs"), p("Review, refine and export EDGE-ready templates.")), div(class = "rids-page-mark", icon("file-export"))),
    uiOutput(ns("amendment_banner")),
    bs4Card(
      title       = "EDGE templates",
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
    output$amendment_banner <- render_amendment_workflow_banner(shared_state)

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
      edge_templates = reactive(shared_state$edge_templates),
      version_type = reactive(shared_state$template_version_type),
      version_number = reactive(shared_state$template_version_number)
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
    
    write_zip <- function(tpls, zp) {
      step4_write_export_zip(
        tpls,
        zip_path = zp,
        version_type = shared_state$template_version_type,
        effective_from_date = shared_state$template_version_effective_date,
        version_number = shared_state$template_version_number
      )
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
            class = "rids-step4-pending",
            div(
              class = "rids-step4-pending-title",
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
          class = "rids-step4-arm-row",
          div(
            class = "rids-step4-arm-field",
            selectInput(
              session$ns("arm_select"),
              label = "Study Arm",
              choices = step4_available_preview_arms(current_preview_templates()),
              selected = step4_effective_preview_arm(input$arm_select, current_preview_templates()),
              width = "100%"
            )
          ),
          uiOutput(session$ns("save_status"))
        ),
        div(
          class = "rids-table-region",
          role = "region",
          `aria-label` = "EDGE template preview table",
          reactableOutput(session$ns("preview_table"))
        ),
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
          class = "rids-step4-validation-error",
          div(
            class = "rids-step4-validation-title",
            "Cost centre matrix validation failed"
          ),
          div(
            class = "rids-step4-validation-copy",
            "The study was not processed. Review the unmatched conditions below, download the report if needed, then start again from step 1."
          ),
          if (!is.null(rollback_message)) {
            div(
              class = "rids-step4-validation-rollback",
              rollback_message
            )
          },
          div(
            class = "rids-step4-validation-actions",
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
          div(
            class = "rids-table-region",
            role = "region",
            `aria-label` = "Unmatched cost centre conditions table",
            reactableOutput(session$ns("unmatched_cost_centres_table"))
          )
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
        showNotification(
          paste("Failed to adjust posting lines:", conditionMessage(e)),
          type = "error",
          duration = 12
        )
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
          if (!identical(shared_state$template_version_type, "baseline")) {
            version <- rids_repos()$template_versions$find(shared_state$template_version_id)
            result <- rids_repos()$template_versions$discard(
              shared_state$template_version_id,
              expected_study_id = shared_state$upload_id
            )
            if (nrow(version) > 0 && !is.na(version$saved_file_path[[1]]) &&
                nzchar(version$saved_file_path[[1]]) && file.exists(version$saved_file_path[[1]])) {
              unlink(version$saved_file_path[[1]])
            }
            result
          } else {
            delete_study_run(
              cpms_id = as.character(shared_state$cpms_id),
              study_site = as.character(shared_state$study_site),
              scenario_id = as.character(shared_state$scenario_id),
              con = CON,
              delete_files = TRUE
            )
          }
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
              "unsupported condition(s) were found.",
              if (identical(shared_state$template_version_type, "baseline")) {
                "The study was deleted and must be started again."
              } else {
                "The new amendment was removed; existing template versions were not changed."
              }
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
          identity_params[[3]],
          shared_state$template_version_id
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
          as.character(shared_state$scenario_id),
          shared_state$template_version_id
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
        zip_name <- paste0(
          "template_version_",
          as.integer(shared_state$template_version_id),
          "_edge_templates.zip"
        )
        zp <- file.path(EDGE_OUTPUT_DIR, zip_name)

        if (!dir.exists(EDGE_OUTPUT_DIR)) dir.create(EDGE_OUTPUT_DIR, recursive = TRUE)
        
        write_zip(tmpl, zp)
        zip_path(zp)
        
        rids_repos()$template_versions$set_edge_zip_path(
          shared_state$template_version_id,
          zp,
          expected_study_id = shared_state$upload_id
        )
        if (identical(shared_state$template_version_type, "baseline")) {
          zip_identity <- study_identity_params()
          rids_repos()$studies$set_edge_zip_path(
            zp,
            zip_identity[[1]],
            zip_identity[[2]],
            zip_identity[[3]]
          )
        }

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

      preview_template <- stats::setNames(list(tpls[[preview_arm]]), preview_arm)
      preview_template <- step4_apply_amendment_export_rules(
        preview_template,
        version_type = shared_state$template_version_type,
        effective_from_date = shared_state$template_version_effective_date,
        version_number = shared_state$template_version_number
      )
      df <- step4_blank_export_departments(preview_template)[[1]]
      
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
        class = "rids-step4-save-status",
        span(class = "rids-step4-save-icon", `aria-hidden` = "true", "✓"),
        span(
          class = "rids-step4-save-path",
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
        rids_repos()$template_versions$activate(
          shared_state$template_version_id,
          expected_study_id = shared_state$upload_id
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
