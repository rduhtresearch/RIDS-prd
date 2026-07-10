step3_UI <- function(id) {
  ns <- NS(id)
  bs4Card(
    title  = "Apply Tags",
    width  = 12,
    status = "primary",
    solidHeader = FALSE,
    footer = tagList(
      selectInput(ns("tag_select"), label = NULL, choices = c("TRAINING_FEE"), width = "200px"),
      actionButton(ns("apply_tag"), "Apply Tag", class = "btn-primary"),
      actionButton(ns("save"), "Save", class = "btn-success"),
      actionButton(ns("next_step"), "Next: Generate Templates", class = "pipeline-next-btn")
    ),
    reactableOutput(ns("table"))
  )
}

step3_Server <- function(id, auth_state, shared_state, current_step) {
  moduleServer(
    id,
    function(input, output, session) {
      
      w <- Waiter$new(
        html = build_loading_state_overlay("Running cost adjustment engine"),
        color = "transparent"
      )
      is_saved <- reactiveVal(FALSE)
      
      observe({
        shinyjs::toggleState("next_step", condition = isTRUE(is_saved()))
      })
      
      # declare reactive val for data
      working_data = reactiveValues(df = NULL)
      
      # run pipeline step on load
      observe({
        req(shared_state$current_step == "step3")
        req(shared_state$processed_ict)

        posting_ict <- tryCatch({
          duplicate_screening_failure_sheets(
            shared_state$processed_ict,
            include_screening_failure = isTRUE(shared_state$include_screening_failure),
            screening_failure_arm = shared_state$screening_failure_arm
          )
        }, error = function(e) {
          app_log_exception("step3", "Screening failure source duplication failed", e, list(
            cpms_id = shared_state$cpms_id,
            include_screening_failure = isTRUE(shared_state$include_screening_failure),
            screening_failure_arm = shared_state$screening_failure_arm
          ))
          showNotification("Failed to prepare Screening Failure rows", type = "error")
          return(NULL)
        })

        req(posting_ict)
        
        df <- tryCatch({
          prepare_posting_input(
            ict           = posting_ict,
            ict_db_path   = DB_DIR,
            scenario_id   = shared_state$scenario_id,
          )
    
        }, error = function(e){
          if (handle_fatal_db_error(session, e, "step3", list(
            cpms_id = shared_state$cpms_id,
            scenario_id = shared_state$scenario_id,
            stage = "prepare_posting_input"
          ))) {
            return(NULL)
          }

          app_log_exception("step3", "Posting input preparation failed", e, list(
            cpms_id = shared_state$cpms_id,
            scenario_id = shared_state$scenario_id
          ))
          showNotification("Failed to generate posting plan", type = "error")
          return(NULL)
        })

        df <- prepare_screening_failure_posting_input(df)
        
        # assign processed data to sys shared state
        req(df)
        df$study_site <- shared_state$study_site
        working_data$df <- df
        shared_state$posting_plan <- df
        is_saved(FALSE)
      })
      
      # render table
      output$table <- renderReactable({
        req(working_data$df)

        column_defs <- list(
          .selection               = colDef(name = "Select", sortable = FALSE, filterable = FALSE, width = 50, align = "center", headerStyle = list(fontWeight = "bold"), header = JS("function() { return '' }")),
          Visit                    = colDef(show = TRUE),
          Activity                 = colDef(show = TRUE),
          Activity.Type            = colDef(name = "Type", show = TRUE),
          Department               = colDef(show = TRUE),
          calc_tag                 = colDef(name = "Tag", show = TRUE),
          Activity.Code            = colDef(show = FALSE),
          Staff.Role               = colDef(show = FALSE),
          Time.Required            = colDef(show = FALSE),
          Activity.Cost            = colDef(show = FALSE),
          Total.Activity.Cost      = colDef(show = FALSE),
          Indirect.Costs           = colDef(show = FALSE),
          Capacity.Building        = colDef(show = FALSE),
          MFF                      = colDef(show = FALSE),
          Total                    = colDef(show = FALSE),
          study_name               = colDef(show = FALSE),
          cpms_id                  = colDef(show = FALSE),
          Flag                     = colDef(show = FALSE),
          SheetName                = colDef(show = FALSE),
          staff_group              = colDef(show = FALSE),
          Study_Arm                = colDef(show = FALSE),
          activity_occurrence_id.x = colDef(show = FALSE),
          sheet_name               = colDef(show = FALSE),
          row_id                   = colDef(show = FALSE),
          provider_org             = colDef(show = FALSE),
          pi_org                   = colDef(show = FALSE),
          Visit_Label              = colDef(show = FALSE),
          activity_type_norm       = colDef(show = FALSE),
          staff_role_norm          = colDef(show = FALSE),
          row_category_auto        = colDef(show = FALSE),
          row_category             = colDef(show = FALSE),
          is_medic                 = colDef(show = FALSE),
          scenario_id              = colDef(show = FALSE),
          ruleset_id               = colDef(show = FALSE),
          activity_occurrence_id.y = colDef(show = FALSE),
          contract_cost            = colDef(show = FALSE)
        )
        
        reactable(
          working_data$df,
          selection  = "multiple",
          onClick    = "select",
          rownames   = FALSE,
          striped    = TRUE,
          highlight  = TRUE,
          compact    = TRUE,
          rowStyle   = JS("function(rowInfo) {
    if (rowInfo.row['calc_tag'] !== null && rowInfo.row['calc_tag'] !== '') {
      return { background: '#e8f4fd' }
    }
  }"),
          columns = column_defs[intersect(names(column_defs), c(".selection", names(working_data$df)))]
        )
      })
      
      # Apply custom tag
      observeEvent(input$apply_tag, {
        selected_rows <- getReactableState("table", "selected")
        req(selected_rows)
        
        working_data$df[selected_rows, "calc_tag"] <- input$tag_select

        log_event(
          level = "INFO",
          area = "step3",
          message = "Scenario applied",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          session_id = auth_state$session_id,
          details = list(
            tag = input$tag_select,
            row_count = length(selected_rows)
          )
        )
        updateReactable("table", data = working_data$df)
        is_saved(FALSE)
      })
      
      # save updates
      observeEvent(input$save, {
        req(working_data$df)
        
        w$show()
        
        shared_state$posting_plan <- working_data$df

        log_event(
          level = "INFO",
          area = "step3",
          message = "Posting evaluation started",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          session_id = auth_state$session_id,
          details = list(rows = nrow(working_data$df))
        )
        app_log_info("step3", "Posting evaluation started")

        study_mff_config <- tryCatch({
          cfg <- rids_repos()$studies$mff_config(
            as.character(shared_state$cpms_id),
            as.character(shared_state$study_site),
            as.character(shared_state$scenario_id)
          )

          if (nrow(cfg) == 0) {
            list(mff_split_enabled = FALSE, mff_split_pct = 0)
          } else {
            list(
              mff_split_enabled = isTRUE(cfg$mff_split_enabled[[1]]),
              mff_split_pct = as.numeric(cfg$mff_split_pct[[1]] %||% 0)
            )
          }
        }, error = function(e) {
          if (handle_fatal_db_error(session, e, "step3", list(
            cpms_id = shared_state$cpms_id,
            upload_id = shared_state$upload_id,
            stage = "load_mff_split_config"
          ))) {
            is_saved(FALSE)
            w$hide()
            return(NULL)
          }

          app_log_exception("step3", "Study MFF split config lookup failed", e, list(
            cpms_id = shared_state$cpms_id,
            upload_id = shared_state$upload_id
          ))
          showNotification("Failed to load study finance configuration", type = "error")
          is_saved(FALSE)
          w$hide()
          return(NULL)
        })

        req(study_mff_config)
        
        evaluated <- tryCatch({
          evaluate_posting_plan(
            prepared_df = working_data$df,
            rules_db_path = DB_DIR,
            scenario_id = shared_state$scenario_id,
            mff_split_enabled = study_mff_config$mff_split_enabled,
            mff_split_pct = study_mff_config$mff_split_pct
          )
        }, error = function(e) {
          if (handle_fatal_db_error(session, e, "step3", list(
            cpms_id = shared_state$cpms_id,
            upload_id = shared_state$upload_id,
            rows = nrow(working_data$df),
            stage = "evaluate_posting_plan"
          ))) {
            is_saved(FALSE)
            w$hide()
            return(NULL)
          }

          app_log_exception("step3", "Posting evaluation failed", e, list(
            cpms_id = shared_state$cpms_id,
            upload_id = shared_state$upload_id,
            rows = nrow(working_data$df)
          ))
          log_event(
            level = "ERROR",
            area = "step3",
            message = "Evaluation failed",
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
          showNotification("Failed to evaluate posting plan", type = "error")
          is_saved(FALSE)
          w$hide()
          return(NULL)
        })
        
        w$hide()
        req(evaluated)
        evaluated$study_site <- shared_state$study_site
        shared_state$evaluated_plan <- evaluated

        log_event(
          level = "INFO",
          area = "step3",
          message = "Posting evaluation completed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          session_id = auth_state$session_id,
          details = list(rows = nrow(evaluated))
        )
        app_log_info("step3", "Posting evaluation completed")
        
        is_saved(TRUE)
        showNotification("Tags saved", type = "message", duration = 5)
      })
      
      # next step
      observeEvent(input$next_step, {
        req(shared_state$evaluated_plan)
        shared_state$current_step <- "step4"
        current_step("step4")
        shinyjs::runjs('$("[data-value=\'tab_step4\']").tab("show")')
        shinyjs::runjs("$('body').addClass('sidebar-collapse')")
      })
      
    }
  )
}
