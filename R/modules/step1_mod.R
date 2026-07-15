step1_UI <- function(id) {
  ns <- NS(id)
  div(
    class = "step1-page",
    div(
      class = "step1-page-header",
      div(
        div(class = "step1-eyebrow", "ICT processing · Step 1 of 4"),
        h1("Set up your study"),
        p("Add the study details and source workbook needed to begin costing.")
      ),
      div(
        class = "step1-header-mark",
        icon("file-excel")
      )
    ),
    div(
      class = "step1-layout",
      div(
        class = "step1-main",
        div(
          class = "step1-surface",
          div(
            class = "step1-section-heading",
            div(class = "step1-section-icon", icon("clipboard-list")),
            div(h2("Study details"), p("Core identifiers used throughout the workflow."))
          ),
          div(
            class = "step1-form-grid",
            div(class = "step1-field", selectInput(ns('scenario'), 'Scenario', choices = c("A", "B"))),
            div(
              class = "step1-field",
              selectInput(
                ns("study_site"),
                "Study site",
                choices = c("Select site..." = "", "RDUHT", "NDDHT"),
                selected = ""
              )
            ),
            div(class = "step1-field step1-field-wide", selectInput(ns('speciality_id'), 'Clinical speciality', choices = NULL)),
            div(class = "step1-field", textInput(ns('edge_id'), 'EDGE ID')),
            div(class = "step1-field", textInput(ns('study_name'), 'Study name'))
          )
        ),
        div(
          class = "step1-surface",
          div(
            class = "step1-section-heading",
            div(class = "step1-section-icon", icon("sliders-h")),
            div(h2("Costing options"), p("Configure optional workbook outputs before processing."))
          ),
          div(
            class = "step1-option-list",
            div(
              class = "step1-option-row",
              div(
                class = "step1-option-copy",
                strong("MFF split"),
                span("Apply the new split percentage to generated costs.")
              ),
              div(
                class = "step1-option-controls",
                checkboxInput(ns("mff_split_enabled"), "Enabled", value = TRUE),
                numericInput(
                  ns("mff_split_pct"),
                  "Percentage",
                  value = 0.25,
                  min = 0,
                  max = 1,
                  step = 0.01
                )
              )
            ),
            div(
              class = "step1-option-row",
              div(
                class = "step1-option-copy",
                strong("Screening failure templates"),
                span("Generate additional EDGE templates when an eligible arm is found.")
              ),
              checkboxInput(
                ns("include_screening_failure"),
                "Generate templates",
                value = FALSE
              )
            ),
            shinyjs::hidden(
              div(
                id = ns("screening_failure_arm_wrap"),
                class = "step1-reveal-field",
                selectInput(
                  ns("screening_failure_arm"),
                  "Screening failure arm",
                  choices = c("No eligible study arm found" = ""),
                  selected = ""
                )
              )
            )
          )
        )
      ),
      div(
        class = "step1-side",
        div(
          class = "step1-surface step1-upload-surface",
          div(
            class = "step1-upload-mark",
            icon("cloud-upload-alt")
          ),
          h2("Upload workbook"),
          p("Use the completed ICT costing workbook in Excel format."),
          fileInput(
            ns("upload"),
            "Choose Excel file",
            multiple = FALSE,
            accept = c(".xlsx")
          ),
          div(class = "step1-file-hint", icon("check-circle"), " .xlsx files only")
        ),
        div(
          class = "step1-surface step1-notes-surface",
          textAreaInput(
            ns("notes"),
            "Upload notes",
            placeholder = "Add context for reviewers (optional)"
          )
        )
      )
    ),
    div(
      class = "step1-action-bar",
      div(
        class = "step1-action-copy",
        icon("shield-alt"),
        span("Your workbook will be validated before any study data is created.")
      ),
      div(
        class = "step1-actions",
        helpUI(ns("help")),
        actionButton(
          ns('next_step'),
          label = tagList("Review costs", icon("arrow-right")),
          class = "pipeline-next-btn step1-next-btn"
        )
      )
    )
  )
}

step1_Server <- function(id, auth_state, shared_state, current_step) {
  moduleServer(id, function(input, output, session) {
    screening_arm_choices <- reactiveVal(character(0))

    reset_step1_form <- function() {
      updateSelectInput(session, "scenario", selected = "A")
      updateSelectInput(session, "study_site", selected = "")
      updateSelectInput(session, "speciality_id", selected = "")
      updateTextInput(session, "edge_id", value = "")
      updateTextInput(session, "study_name", value = "")
      updateCheckboxInput(session, "mff_split_enabled", value = TRUE)
      updateNumericInput(session, "mff_split_pct", value = 0.25)
      updateCheckboxInput(session, "include_screening_failure", value = FALSE)
      screening_arm_choices(character(0))
      updateSelectInput(
        session,
        "screening_failure_arm",
        choices = c("No eligible study arm found" = ""),
        selected = ""
      )
      updateTextAreaInput(session, "notes", value = "")
      session$sendCustomMessage("resetFileInput", list(id = session$ns("upload")))

      feedbackDanger("edge_id", show = FALSE)
      feedbackDanger("upload", show = FALSE)
      feedbackDanger("study_site", show = FALSE)
      feedbackDanger("speciality_id", show = FALSE)
      feedbackDanger("mff_split_pct", show = FALSE)
    }

    session$userData$reset_step1_form <- reset_step1_form
    
    # ── Specialities lookup (loaded once at module init) ─────────────────────
    specialities <- reactive({
      rids_repos()$specialities$list_active()
    })
    
    observe({
      sp <- specialities()
      req(nrow(sp) > 0)
      
      choices        <- sp$id
      names(choices) <- sp$name
      
      updateSelectInput(
        session,
        "speciality_id",
        choices  = c("Select speciality..." = "", choices),
        selected = ""
      )
    })

    observe({
      shinyjs::toggle(
        id = "mff_split_pct",
        condition = isTRUE(input$mff_split_enabled),
        anim = TRUE
      )
    })

    observe({
      shinyjs::toggle(
        id = "screening_failure_arm_wrap",
        condition = isTRUE(input$include_screening_failure) &&
          length(screening_arm_choices()) > 0,
        anim = TRUE
      )
    })

    observeEvent(input$upload, {
      if (is.null(input$upload)) {
        screening_arm_choices(character(0))
        updateSelectInput(
          session,
          "screening_failure_arm",
          choices = c("No eligible study arm found" = ""),
          selected = ""
        )
        return()
      }

      sheet_names <- tryCatch(
        openxlsx::getSheetNames(input$upload$datapath),
        error = function(e) character(0)
      )

      candidates <- screening_failure_candidate_sheets(sheet_names)
      screening_arm_choices(candidates)

      if (length(candidates) == 0) {
        updateSelectInput(
          session,
          "screening_failure_arm",
          choices = c("No eligible study arm found" = ""),
          selected = ""
        )
        return()
      }

      choice_labels <- candidates
      choice_labels[[1]] <- paste0(choice_labels[[1]], " (automatic)")

      updateSelectInput(
        session,
        "screening_failure_arm",
        choices = stats::setNames(candidates, choice_labels),
        selected = candidates[[1]]
      )
    }, ignoreNULL = FALSE)
    
    # ── Help ─────────────────────────────────────────────────────────────────
    helpServer("help", content = list(
      title = "Upload Help",
      sections = list(
        list(
          heading = "What is this step?",
          body    = "This step allows you to upload an ICT costing workbook and begin the RIDS pipeline."
        ),
        list(
          heading = "What file should I upload?",
          body    = "Upload the Excel (.xlsx) ICT workbook provided by your study team."
        ),
        list(
          heading = "What is a Scenario?",
          body    = "The scenario determines how costs are distributed across posting lines. Select the scenario that matches your study's commercial arrangement."
        ),
        list(
          heading = "What is Clinical Speciality?",
          body    = "The clinical area the study sits within. This is required and used for grouping studies in reporting."
        ),
        list(
          heading = "What is Study Site?",
          body    = "Select the study site that this upload belongs to. This is used to keep studies with the same CPMS ID distinct."
        ),
        list(
          heading = "FAQ",
          body    = "If you are unsure which scenario to select, contact your R&D finance lead."
        )
      )
    ))
    
    # ── Next step ─────────────────────────────────────────────────────────────
    observeEvent(input$next_step, {
      
      # ── Validation ───────────────────────────────────────────────────────
      feedbackDanger("edge_id",       show = input$edge_id == "", text = "Required")
      feedbackDanger("upload",        show = is.null(input$upload), text = "Required")
      feedbackDanger("study_site",    show = is.null(input$study_site) || input$study_site == "",
                     text = "Required")
      feedbackDanger("speciality_id", show = is.null(input$speciality_id) || input$speciality_id == "",
                     text = "Required")
      feedbackDanger(
        "mff_split_pct",
        show = isTRUE(input$mff_split_enabled) &&
          (is.null(input$mff_split_pct) || is.na(input$mff_split_pct) ||
             input$mff_split_pct < 0 || input$mff_split_pct > 1),
        text = "Enter a value between 0 and 1"
      )
      
      req(
        input$edge_id != "",
        input$scenario,
        !is.null(input$study_site),
        input$study_site != "",
        !is.null(input$upload),
        !is.null(input$speciality_id),
        input$speciality_id != "",
        !isTRUE(input$mff_split_enabled) ||
          (!is.null(input$mff_split_pct) && !is.na(input$mff_split_pct) &&
             input$mff_split_pct >= 0 && input$mff_split_pct <= 1)
      )

      log_event(
        level = "INFO",
        area = "upload",
        message = "Upload started",
        user_id = auth_state$user_id,
        username = auth_state$username,
        session_id = auth_state$session_id,
        details = list(
          scenario_id = input$scenario,
          study_site = input$study_site,
          edge_id = input$edge_id,
          original_filename = input$upload$name,
          speciality_id = as.integer(input$speciality_id),
          mff_split_enabled = isTRUE(input$mff_split_enabled),
          mff_split_pct = if (isTRUE(input$mff_split_enabled)) as.numeric(input$mff_split_pct) else 0
        )
      )
      app_log_info("step1", "Upload started")
      
      ###
      # ── Validation ────────────────────────────────────────────────────────
      validation <- tryCatch(
        validate_ict_workbook(input$upload$datapath),
        error = function(e) {
          list(valid = FALSE, findings = paste("Validation error:", conditionMessage(e)))
        }
      )
      
      if (!isTRUE(validation$valid)) {
        log_event(
          level = "WARN",
          area = "upload",
          message = "Upload validation failed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          session_id = auth_state$session_id,
          details = list(
            scenario_id = input$scenario,
            study_site = input$study_site,
            edge_id = input$edge_id,
            original_filename = input$upload$name,
            validation_findings = head(as.character(validation$findings %||% character()), 10)
          )
        )
        showModal(modalDialog(
          title = "ICT workbook validation failed",
          size  = "m",
          easyClose = FALSE,
          footer = modalButton("Close"),
          div(
            style = "padding: 0.5rem 0;",
            p(
              style = "color: #697786; margin-bottom: 1rem;",
              "The uploaded workbook contains data outside the expected structure. ",
              "Please remove the highlighted content and re-upload."
            ),
            tags$ul(
              style = "padding-left: 1.25rem; color: #1d2a36;",
              lapply(validation$findings, tags$li)
            )
          )
        ))
        return()  # block — do not proceed to file.copy / DB insert
      }
      ###
      # ── Process ──────────────────────────────────────────────────────────
      timestamp     <- format(Sys.time(), "%Y%m%d_%H%M%S")
      original_name <- gsub("[/\\\\]", "_", basename(as.character(input$upload$name)))
      saved_path    <- tempfile(
        pattern = paste0(timestamp, "_"),
        tmpdir = ICT_UPLOAD_DIR,
        fileext = paste0("_", original_name)
      )

      log_event(
        level = "INFO",
        area = "upload",
        message = "Upload processing started",
        user_id = auth_state$user_id,
        username = auth_state$username,
        session_id = auth_state$session_id,
        details = list(
          scenario_id = input$scenario,
          study_site = input$study_site,
          edge_id = input$edge_id,
          original_filename = original_name
        )
      )
      app_log_info("step1", "Workbook processing started")

      copy_ok <- tryCatch({
        file.copy(input$upload$datapath, saved_path, overwrite = FALSE)
      }, error = function(e) {
        log_event(
          level = "ERROR",
          area = "upload",
          message = "Upload processing failed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          session_id = auth_state$session_id,
          details = list(
            scenario_id = input$scenario,
            edge_id = input$edge_id,
            original_filename = original_name,
            error = conditionMessage(e),
            stage = "file_copy"
          )
        )
        app_log_exception("step1", "Workbook file copy failed", e, list(
          edge_id = input$edge_id,
          filename = original_name
        ))
        FALSE
      })

      if (!isTRUE(copy_ok)) {
        showNotification("Failed to save uploaded workbook", type = "error")
        return()
      }
      
      extracted_cpms <- tryCatch({
        extract_cpms_id(saved_path)
      }, error = function(e) {
        log_event(
          level = "ERROR",
          area = "upload",
          message = "Upload processing failed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          session_id = auth_state$session_id,
          details = list(
            scenario_id = input$scenario,
            edge_id = input$edge_id,
            original_filename = original_name,
            error = conditionMessage(e),
            stage = "cpms_extract"
          )
        )
        app_log_exception("step1", "CPMS extraction failed", e, list(
          edge_id = input$edge_id,
          filename = original_name
        ))
        showNotification("Failed to extract CPMS ID", type = "error")
        return(NULL)
      })
      
      if (is.null(extracted_cpms)) {
        if (file.exists(saved_path)) unlink(saved_path, force = TRUE)
        return()
      }

      duplicate_exists <- tryCatch({
        rids_repos()$studies$exists_run(
          sanitize_text_value(as.character(extracted_cpms)),
          sanitize_text_value(input$study_site),
          sanitize_text_value(input$scenario)
        )
      }, error = function(e) {
        if (handle_fatal_db_error(session, e, "step1", list(
          cpms_id = extracted_cpms,
          study_site = input$study_site,
          scenario_id = input$scenario,
          stage = "duplicate_check"
        ))) {
          return(NA)
        }

        app_log_exception("step1", "Duplicate study check failed", e, list(
          cpms_id = extracted_cpms,
          study_site = input$study_site,
          scenario_id = input$scenario
        ))
        showNotification("Failed to validate whether this study already exists", type = "error")
        return(NA)
      })

      if (is.na(duplicate_exists)) {
        if (file.exists(saved_path)) unlink(saved_path, force = TRUE)
        return()
      }

      if (isTRUE(duplicate_exists)) {
        if (file.exists(saved_path)) unlink(saved_path, force = TRUE)
        showNotification(
          paste(
            "This study already exists for CPMS",
            extracted_cpms,
            ", site",
            input$study_site,
            "and scenario",
            input$scenario
          ),
          type = "warning",
          duration = 8
        )
        return()
      }
      
      meta_saved <- tryCatch({
        rids_repos()$studies$insert_meta(
          cpms_id = sanitize_text_value(as.character(extracted_cpms)),
          study_site = sanitize_text_value(input$study_site),
          scenario_id = sanitize_text_value(input$scenario),
          edge_id = sanitize_text_value(input$edge_id),
          study_name = sanitize_text_value(input$study_name),
          notes = sanitize_text_value(input$notes),
          uploaded_by = sanitize_text_value(auth_state$username %||% auth_state$name %||% ""),
          original_filename = sanitize_text_value(original_name),
          saved_file_path = sanitize_text_value(saved_path),
          speciality_id = as.integer(input$speciality_id),
          mff_split_enabled = isTRUE(input$mff_split_enabled),
          mff_split_pct = if (isTRUE(input$mff_split_enabled)) as.numeric(input$mff_split_pct) else 0
        )
        TRUE
      }, error = function(e) {
        if (handle_fatal_db_error(session, e, "step1", list(
          cpms_id = extracted_cpms,
          edge_id = input$edge_id,
          study_site = input$study_site,
          scenario_id = input$scenario,
          stage = "meta_data_insert"
        ))) {
          return(FALSE)
        }

        log_event(
          level = "ERROR",
          area = "upload",
          message = "Upload processing failed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = extracted_cpms,
          session_id = auth_state$session_id,
          details = list(
            scenario_id = input$scenario,
            study_site = input$study_site,
            edge_id = input$edge_id,
            original_filename = original_name,
            error = conditionMessage(e),
            stage = "meta_data_insert"
          )
        )
        app_log_exception("step1", "Upload metadata save failed", e, list(
          cpms_id = extracted_cpms,
          edge_id = input$edge_id
        ))
        showNotification("Failed to save upload metadata", type = "error")
        FALSE
      })

      if (!isTRUE(meta_saved)) {
        if (file.exists(saved_path)) unlink(saved_path, force = TRUE)
        return()
      }

      upload_id <- tryCatch({
        rids_repos()$studies$last_upload_id()
      }, error = function(e) {
        NA_integer_
      })

      version_id <- tryCatch({
        rids_repos()$template_versions$create(
          study_id = upload_id,
          version_type = "baseline",
          uploaded_by = sanitize_text_value(auth_state$username %||% auth_state$name %||% ""),
          notes = sanitize_text_value(input$notes),
          original_filename = sanitize_text_value(original_name),
          saved_file_path = sanitize_text_value(saved_path)
        )
      }, error = function(e) {
        app_log_exception("step1", "Baseline template version save failed", e, list(
          cpms_id = extracted_cpms,
          upload_id = upload_id
        ))
        cleanup <- tryCatch(
          delete_study_run(
            cpms_id = sanitize_text_value(as.character(extracted_cpms)),
            study_site = sanitize_text_value(input$study_site),
            scenario_id = sanitize_text_value(input$scenario),
            con = CON,
            delete_files = TRUE
          ),
          error = function(cleanup_error) cleanup_error
        )
        message <- "Failed to create the baseline template version. The upload was rolled back."
        if (inherits(cleanup, "error")) {
          message <- "Failed to create the baseline template version and cleanup did not complete. Contact an administrator."
        }
        showNotification(message, type = "error", duration = 10)
        NA_integer_
      })

      if (is.na(version_id)) return()

      version_number <- rids_repos()$template_versions$find(version_id)$version_number[[1]]
      
      shared_state$processed_ict <- tryCatch({
        process_workbook(
          input_path = saved_path,
          db_path = DB_DIR,
          study_site = input$study_site,
          scenario_id = input$scenario,
          version_id = version_id
        )
      }, error = function(e) {
        if (handle_fatal_db_error(session, e, "step1", list(
          cpms_id = extracted_cpms,
          upload_id = upload_id,
          scenario_id = input$scenario,
          stage = "workbook_process"
        ))) {
          return(NULL)
        }

        log_event(
          level = "ERROR",
          area = "upload",
          message = "Upload processing failed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = extracted_cpms,
          upload_id = upload_id,
          session_id = auth_state$session_id,
          details = list(
            scenario_id = input$scenario,
            edge_id = input$edge_id,
            original_filename = original_name,
            error = conditionMessage(e),
            stage = "workbook_process"
          )
        )
        app_log_exception("step1", "Workbook processing failed", e, list(
          cpms_id = extracted_cpms,
          upload_id = upload_id
        ))
        showNotification("Failed to process workbook", type = "error")
        return(NULL)
      })
      
      if (is.null(shared_state$processed_ict)) {
        cleanup <- tryCatch(
          delete_study_run(
            cpms_id = sanitize_text_value(as.character(extracted_cpms)),
            study_site = sanitize_text_value(input$study_site),
            scenario_id = sanitize_text_value(input$scenario),
            con = CON,
            delete_files = TRUE
          ),
          error = function(cleanup_error) cleanup_error
        )
        if (inherits(cleanup, "error")) {
          showNotification(
            "Workbook processing failed and cleanup did not complete. Contact an administrator.",
            type = "error",
            duration = 10
          )
        }
        return()
      }

      processed_candidates <- screening_failure_candidate_sheets(shared_state$processed_ict)
      selected_screening_arm <- resolve_screening_failure_arm(
        shared_state$processed_ict,
        input$screening_failure_arm
      )

      screening_arm_choices(processed_candidates)
      if (length(processed_candidates) > 0) {
        processed_labels <- processed_candidates
        processed_labels[[1]] <- paste0(processed_labels[[1]], " (automatic)")
        updateSelectInput(
          session,
          "screening_failure_arm",
          choices = stats::setNames(processed_candidates, processed_labels),
          selected = if (!is.na(selected_screening_arm)) selected_screening_arm else processed_candidates[[1]]
        )
      } else {
        updateSelectInput(
          session,
          "screening_failure_arm",
          choices = c("No eligible study arm found" = ""),
          selected = ""
        )
      }
      
      # ── Resolve speciality name for shared_state ─────────────────────────
      sp_id   <- as.integer(input$speciality_id)
      sp_name <- specialities()$name[specialities()$id == sp_id]
      
      shared_state$cpms_id          <- extracted_cpms
      shared_state$upload_id        <- upload_id
      shared_state$template_version_id <- version_id
      shared_state$template_version_number <- version_number
      shared_state$template_version_type <- "baseline"
      shared_state$template_version_effective_date <- NULL
      shared_state$scenario_id      <- input$scenario
      shared_state$study_site       <- input$study_site
      shared_state$include_screening_failure <- isTRUE(input$include_screening_failure)
      shared_state$screening_failure_arm <- if (!is.na(selected_screening_arm)) selected_screening_arm else NULL
      shared_state$mff_split_enabled <- isTRUE(input$mff_split_enabled)
      shared_state$mff_split_pct <- if (isTRUE(input$mff_split_enabled)) as.numeric(input$mff_split_pct) else 0
      shared_state$speciality_id    <- sp_id
      shared_state$speciality_name  <- sp_name
      shared_state$study_name       <- input$study_name
      shared_state$upload_meta      <- list(
        scenario_id     = input$scenario,
        study_site      = input$study_site,
        mff_split_enabled = isTRUE(input$mff_split_enabled),
        mff_split_pct   = if (isTRUE(input$mff_split_enabled)) as.numeric(input$mff_split_pct) else 0,
        include_screening_failure = isTRUE(input$include_screening_failure),
        screening_failure_arm = if (!is.na(selected_screening_arm)) selected_screening_arm else NULL,
        edge_id         = input$edge_id,
        study_name      = input$study_name,
        filename        = original_name,
        raw_ict         = saved_path,
        timestamp       = timestamp,
        upload_id       = upload_id,
        version_id      = version_id,
        speciality_id   = sp_id,
        speciality_name = sp_name
      )

      log_event(
        level = "INFO",
        area = "upload",
        message = "Upload completed",
        user_id = auth_state$user_id,
        username = auth_state$username,
        cpms_id = extracted_cpms,
        upload_id = upload_id,
        session_id = auth_state$session_id,
        details = list(
          scenario_id = input$scenario,
          study_site = input$study_site,
          edge_id = input$edge_id,
          original_filename = original_name,
          speciality_id = sp_id
        )
      )
      app_log_info("step1", "Upload completed")
      
      # ── Navigate ─────────────────────────────────────────────────────────
      current_step("step2")
      shinyjs::runjs('$("[data-value=\'tab_step2\']").tab("show")')
      shinyjs::runjs("$('body').addClass('sidebar-collapse')")
    })
    
  })
}
