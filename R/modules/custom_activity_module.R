# ==============================================================================
# R/addons/custom_activities/custom_activity_module.R
#
# Shiny module for the "Custom activities" panel in step 4.
#
# Interaction model:
#   - Add activity      → modal flow with switch (single CC | baseline)
#   - View detail       → click row to expand inline; click again to collapse
#   - Delete activity   → button inside the expanded row's detail panel
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(reactable)
  library(htmltools)
  library(shinyjs)
  library(dplyr)
  library(tibble)
  library(scales)
})

# ── Constants ────────────────────────────────────────────────────────────────

.CA_BASELINE_SLOTS <- 5L

.CA_BASELINE_SLOT_LABELS <- c(
  "DIRECT — Direct Cost",
  "CAPACITY_RD — Capacity (R&D)",
  "INDIRECT_50_DELIVERY — Indirect 50% (Delivery)",
  "INDIRECT_25_TRUST — Indirect 25% (Trust Overhead)",
  "INDIRECT_25_PI — Indirect 25% (PI)"
)

.CA_MODE_LEFT_VALUE  <- "single_cc"
.CA_MODE_RIGHT_VALUE <- "baseline"

# ── Helpers ──────────────────────────────────────────────────────────────────

.format_gbp <- function(x) {
  if (is.null(x) || length(x) == 0 || any(is.na(x))) return("—")
  scales::label_currency(prefix = "£", accuracy = 0.01)(x)
}

.field_hint <- function(touched, error_msg, hint = NULL) {
  if (isTRUE(touched) && !is.null(error_msg)) {
    span(
      class = "ca-field-hint is-error",
      error_msg
    )
  } else if (!is.null(hint)) {
    span(
      class = "ca-field-hint",
      hint
    )
  } else {
    NULL
  }
}

custom_activity_mode_control <- function(ns) {
  div(
    class = "ca-mode-row",
    span(
      id = ns("mode_left_label"),
      class = "ca-mode-label is-active",
      "Single cost centre"
    ),
    tags$label(
      class = "ca-mode-switch",
      tags$input(
        id = ns("modal_mode_switch"),
        class = "ca-mode-switch-input",
        type = "checkbox",
        `aria-label` = "Use baseline five-row cost mode",
        `aria-describedby` = paste(
          ns("mode_left_label"),
          ns("mode_right_label")
        ),
        onclick = sprintf(
          "Shiny.setInputValue('%s', this.checked ? '%s' : '%s')",
          ns("modal_mode"), .CA_MODE_RIGHT_VALUE, .CA_MODE_LEFT_VALUE
        )
      ),
      tags$span(
        class = "ca-mode-switch-track",
        `aria-hidden` = "true",
        tags$span(class = "ca-switch-knob")
      )
    ),
    span(
      id = ns("mode_right_label"),
      class = "ca-mode-label",
      "Baseline (5 rows)"
    ),
    tags$script(HTML(sprintf("\n      (function(){\n        var cb = document.getElementById('%s');\n        var leftLbl = document.getElementById('%s');\n        var rightLbl = document.getElementById('%s');\n        function refresh(){\n          leftLbl.classList.toggle('is-active', !cb.checked);\n          rightLbl.classList.toggle('is-active', cb.checked);\n        }\n        cb.addEventListener('change', refresh);\n        refresh();\n        Shiny.setInputValue('%s', '%s');\n      })();\n    ",
      ns("modal_mode_switch"),
      ns("mode_left_label"),
      ns("mode_right_label"),
      ns("modal_mode"), .CA_MODE_LEFT_VALUE
    )))
  )
}

.ca_as_scalar_num <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_real_)
  if (is.character(x)) {
    x <- trimws(x[[1L]])
    if (!nzchar(x)) return(NA_real_)
  }
  v <- suppressWarnings(as.numeric(x[[1L]]))
  if (length(v) != 1L) return(NA_real_)
  v
}

.ca_is_blank <- function(x) {
  is.null(x) || length(x) == 0L || !nzchar(trimws(as.character(x[[1L]]) %||% ""))
}

.ca_validate_modal_inputs <- function(input_list) {
  errs <- list()

  if (.ca_is_blank(input_list$modal_arm))      errs$modal_arm      <- "Required"
  if (.ca_is_blank(input_list$modal_activity)) errs$modal_activity <- "Required"

  mode <- input_list$modal_mode %||% .CA_MODE_LEFT_VALUE

  if (identical(mode, .CA_MODE_LEFT_VALUE)) {
    if (.ca_is_blank(input_list$single_cc))
      errs$single_cc <- "Required"

    amt <- .ca_as_scalar_num(input_list$single_amt)
    if (is.na(amt)) {
      errs$single_amt <- "Required"
    } else if (amt < 0) {
      errs$single_amt <- "Must be >= 0"
    }

  } else if (identical(mode, .CA_MODE_RIGHT_VALUE)) {
    for (i in seq_len(.CA_BASELINE_SLOTS)) {
      cc_id  <- paste0("base_cc_",  i)
      amt_id <- paste0("base_amt_", i)

      if (.ca_is_blank(input_list[[cc_id]]))
        errs[[cc_id]] <- "Required"

      amt <- .ca_as_scalar_num(input_list[[amt_id]])
      if (is.na(amt)) {
        errs[[amt_id]] <- "Required"
      } else if (amt < 0) {
        errs[[amt_id]] <- "Must be >= 0"
      }
    }
  }

  errs
}

.ca_summarise_for_panel <- function(customs) {
  if (nrow(customs) == 0) {
    return(tibble(
      custom_activity_id = character(0),
      Study_Arm          = character(0),
      Activity           = character(0),
      mode_label         = character(0),
      total              = numeric(0),
      slots              = integer(0)
    ))
  }
  
  customs |>
    group_by(custom_activity_id, Study_Arm, Activity, mode) |>
    summarise(
      total = sum(amount, na.rm = TRUE),
      slots = n(),
      .groups = "drop"
    ) |>
    mutate(
      mode_label = if_else(mode == "single_cc",
                           "Single cost centre",
                           "Baseline (5 rows)")
    ) |>
    select(custom_activity_id, Study_Arm, Activity, mode_label, total, slots) |>
    arrange(custom_activity_id)
}

# ── UI ───────────────────────────────────────────────────────────────────────

customActivityUI <- function(id) {
  ns <- NS(id)
  
  bs4Card(
    title       = "Custom activities",
    width       = 12,
    status      = "secondary",
    solidHeader = FALSE,
    collapsible = TRUE,
    collapsed   = TRUE,
    closable    = FALSE,
    
    useShinyjs(),
    
    tags$head(
      # Scoped CSS for the modal (uses ns()-prefixed id)
      tags$style(HTML(paste0(
        "#", ns("modal_root"), " .ca-modal-section { margin-bottom: 1.25rem; }\n",
        "#", ns("modal_root"), " .ca-section-label {\n",
        "  font-size: 0.78rem; font-weight: 600; color: #5b6772;\n",
        "  text-transform: uppercase; letter-spacing: 0.04em;\n",
        "  margin-bottom: 0.5rem;\n",
        "}\n",
        "#", ns("modal_root"), " .ca-slot-block {\n",
        "  padding: 0.65rem 0.85rem; margin-bottom: 0.65rem;\n",
        "  background: #fafbfc; border-left: 3px solid #cfd8dc;\n",
        "  border-radius: 2px;\n",
        "}\n",
        "#", ns("modal_root"), " .ca-slot-label {\n",
        "  font-size: 0.78rem; font-weight: 600; color: #1f5f8b;\n",
        "  margin-bottom: 0.35rem;\n",
        "}\n",
        "#", ns("modal_root"), " .ca-running-total {\n",
        "  margin-top: 0.5rem; padding: 0.5rem 0.75rem;\n",
        "  background: #eef4fa; border-radius: 3px;\n",
        "  font-weight: 600; color: #1f5f8b;\n",
        "}\n",
        "#", ns("modal_root"), " .form-group { margin-bottom: 0.35rem; }\n"
      ))),
      # Unscoped CSS for the row-expand detail panel (no ns() needed)
      tags$style(HTML("
        .ca-detail-panel {
          padding: 0.85rem 1rem;
          background: #f7f9fb;
          border-top: 1px solid #e1e5eb;
        }
        .ca-detail-header {
          font-size: 0.85rem; color: #5b6772; margin-bottom: 0.5rem;
        }
        .ca-detail-table {
          width: 100%; border-collapse: collapse; margin-bottom: 0.75rem;
          background: #ffffff;
        }
        .ca-detail-table th {
          font-size: 0.78rem; text-align: left;
          padding: 0.4rem 0.6rem; background: #eef2f5; color: #5b6772;
          font-weight: 600; text-transform: uppercase; letter-spacing: 0.03em;
        }
        .ca-detail-table td {
          padding: 0.4rem 0.6rem; font-size: 0.85rem;
          border-top: 1px solid #eef2f5;
        }
        .ca-detail-table td.num { text-align: right; font-variant-numeric: tabular-nums; }
        .ca-detail-footer {
          display: flex; align-items: center; justify-content: space-between;
        }
        .ca-detail-total {
          font-size: 0.9rem; font-weight: 600; color: #1f5f8b;
        }
      "))
    ),
    
    div(
      class = "rids-form-copy rids-custom-activity-intro",
      "Add activities not captured in the ICT workbook. ",
      "Custom activities are appended at export."
    ),
    
    div(
      class = "rids-custom-activity-actions",
      actionButton(ns("open_add"), "Add activity",
                   class = "btn-primary", icon = icon("plus")),
      uiOutput(ns("count_status"))
    ),
    
    div(
      class = "rids-table-region rids-interactive-table",
      role = "region",
      `aria-label` = "Custom activities summary table",
      reactableOutput(ns("summary_table"))
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────────

customActivityServer <- function(id, auth_state, shared_state, study_arm_choices) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    invalidation_tick <- reactiveVal(0L)
    .bump <- function() invalidation_tick(invalidation_tick() + 1L)
    
    customs <- reactive({
      invalidation_tick()
      req(shared_state$cpms_id)
      req(shared_state$study_site, shared_state$scenario_id)
      ca_load(
        cpms_id = as.character(shared_state$cpms_id),
        study_site = as.character(shared_state$study_site),
        scenario_id = as.character(shared_state$scenario_id),
        version_id = shared_state$template_version_id
      )
    })
    
    # Currently expanded row id, tracked server-side so the Delete button
    # observer knows which activity to remove.
    expanded_id <- reactiveVal(NULL)
    
    # ── Wipe on step 4 entry ───────────────────────────────────────────────
    observeEvent(shared_state$current_step, {
      if (isTRUE(shared_state$current_step == "step4") &&
          !is.null(shared_state$cpms_id)) {
        ca_clear_run(
          cpms_id = as.character(shared_state$cpms_id),
          study_site = as.character(shared_state$study_site %||% NA_character_),
          scenario_id = as.character(shared_state$scenario_id %||% NA_character_),
          version_id = shared_state$template_version_id
        )
        .bump()
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # ── Summary table with native row expansion ────────────────────────────
    output$summary_table <- renderReactable({
      df <- .ca_summarise_for_panel(customs())
      
      if (nrow(df) == 0) {
        return(reactable(
          tibble(` ` = "No custom activities added yet."),
          sortable = FALSE, pagination = FALSE,
          defaultColDef = colDef(align = "center"),
          compact = TRUE
        ))
      }
      
      # Capture data for the details() function closure
      customs_snapshot <- customs()
      
      reactable(
        df,
        columns = list(
          custom_activity_id = colDef(name = "ID",         minWidth = 100),
          Study_Arm          = colDef(name = "Study Arm",  minWidth = 120),
          Activity           = colDef(name = "Activity",   minWidth = 180),
          mode_label         = colDef(name = "Mode",       minWidth = 140),
          total              = colDef(name = "Total",      minWidth = 100,
                                      format = colFormat(prefix = "£", separators = TRUE, digits = 2)),
          slots              = colDef(name = "Slots",      minWidth = 70)
        ),
        # Native reactable detail panel — returns a tagList per row.
        # Detail panels are part of the regular Shiny UI lifecycle, so an
        # actionButton() inside fires a real Shiny observer (unlike cell
        # renderers which produce raw HTML strings).
        details = function(index) {
          activity_id <- df$custom_activity_id[index]
          
          # When the panel opens, remember which row is expanded so the
          # Delete observer knows what to remove.
          expanded_id(activity_id)
          
          slot_rows <- customs_snapshot |>
            filter(custom_activity_id == activity_id) |>
            arrange(slot_num)
          
          # Build a small HTML table of the slots
          rows_html <- lapply(seq_len(nrow(slot_rows)), function(i) {
            tags$tr(
              tags$td(slot_rows$slot_num[i]),
              tags$td(slot_rows$cost_centre[i]),
              tags$td(class = "num", .format_gbp(slot_rows$amount[i]))
            )
          })
          
          div(class = "ca-detail-panel",
              div(class = "ca-detail-header",
                  tags$strong("Activity: "), unique(slot_rows$Activity), " · ",
                  tags$strong("Mode: "),
                  if (unique(slot_rows$mode) == "single_cc")
                    "Single cost centre" else "Baseline (5 rows)"),
              tags$table(class = "ca-detail-table",
                         tags$thead(
                           tags$tr(tags$th("Slot"), tags$th("Cost centre"), tags$th("Amount"))
                         ),
                         tags$tbody(rows_html)
              ),
              div(class = "ca-detail-footer",
                  div(class = "ca-detail-total",
                      "Total: ", .format_gbp(sum(slot_rows$amount))),
                  actionButton(
                    ns(paste0("delete_", activity_id)),
                    "Delete activity",
                    class = "btn-sm btn-outline-danger",
                    icon  = icon("trash")
                  )
              )
          )
        },
        onClick    = "expand",   # clicking a row toggles its detail panel
        defaultColDef = colDef(vAlign = "center"),
        striped    = TRUE,
        highlight  = TRUE,
        compact    = TRUE,
        pagination = FALSE
      )
    })
    
    output$count_status <- renderUI({
      n <- nrow(customs())
      if (n == 0) {
        span(class = "rids-custom-activity-count", "No custom activities added.")
      } else {
        total <- sum(customs()$amount, na.rm = TRUE)
        span(class = "rids-custom-activity-count has-activities",
             sprintf("%d activit%s · total %s",
                     n_distinct(customs()$custom_activity_id),
                     if (n_distinct(customs()$custom_activity_id) == 1) "y" else "ies",
                     .format_gbp(total)))
      }
    })
    
    # ── Delete observer ────────────────────────────────────────────────────
    # A single observer watches input changes for any input whose name
    # starts with "delete_". When one fires (i.e. the button inside an
    # expanded panel was clicked), pull the id out of the input name and
    # delete that activity. No bridges, no JS — proper Shiny input.
    observe({
      # React to any change in any delete_* input
      delete_inputs <- grep("^delete_", names(input), value = TRUE)
      lapply(delete_inputs, function(input_name) {
        input[[input_name]]  # take dependency
      })
    })
    
    # Use observe + isolate pattern: detect which delete button was just
    # clicked by inspecting which counter changed.
    delete_counters <- reactiveVal(list())
    
    observe({
      delete_inputs <- grep("^delete_", names(input), value = TRUE)
      current <- isolate(delete_counters())
      latest_clicked <- NULL
      latest_count   <- 0L
      
      for (input_name in delete_inputs) {
        v <- input[[input_name]]
        if (is.null(v)) next
        prev <- current[[input_name]] %||% 0L
        if (v > prev) {
          # This button was just clicked
          activity_id <- sub("^delete_", "", input_name)
          latest_clicked <- activity_id
          latest_count   <- v
        }
        current[[input_name]] <- v
      }
      
      delete_counters(current)
      
      if (!is.null(latest_clicked)) {
        # Perform the delete
        tryCatch({
          ca_delete(
            latest_clicked,
            cpms_id = as.character(shared_state$cpms_id),
            study_site = as.character(shared_state$study_site),
            scenario_id = as.character(shared_state$scenario_id),
            version_id = shared_state$template_version_id
          )
          .bump()
          expanded_id(NULL)
          showNotification(paste0("Deleted ", latest_clicked),
                           type = "message", duration = 3)
        }, error = function(e) {
          showNotification(paste("Failed to delete:", conditionMessage(e)),
                           type = "error", duration = 10)
        })
      }
    })
    
    # ── Touch tracking for the add modal ────────────────────────────────────
    touched <- reactiveValues()
    
    .all_field_ids <- function() {
      c("modal_arm", "modal_activity",
        "single_cc", "single_amt",
        paste0("base_cc_",  seq_len(.CA_BASELINE_SLOTS)),
        paste0("base_amt_", seq_len(.CA_BASELINE_SLOTS)))
    }
    
    observeEvent(input$open_add, {
      for (fid in .all_field_ids()) touched[[fid]] <- FALSE
    })
    
    lapply(.all_field_ids(), function(fid) {
      observeEvent(input[[fid]], {
        touched[[fid]] <- TRUE
      }, ignoreInit = TRUE, ignoreNULL = FALSE)
    })
    
    # ── Add modal ──────────────────────────────────────────────────────────
    observeEvent(input$open_add, {
      
      activity_choices <- tryCatch(ca_load_ref_activities(), error = function(e) character(0))
      arm_choices      <- tryCatch(study_arm_choices(),       error = function(e) character(0))
      
      showModal(modalDialog(
        title = "Add custom activity",
        size  = "l",
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("modal_cancel"), "Cancel"),
          actionButton(ns("modal_submit"), "Add activity",
                       class = "btn-primary", icon = icon("check"))
        ),
        
        div(
          id = ns("modal_root"),
          
          # ── Section 1: identification ─────────────────────────────────
          div(class = "ca-modal-section",
              div(class = "ca-section-label", "Activity details"),
              fluidRow(
                column(6,
                       selectInput(ns("modal_arm"), label = NULL,
                                   choices = c("Choose study arm…" = "", arm_choices),
                                   width = "100%"),
                       uiOutput(ns("hint_modal_arm"))),
                column(6,
                       selectInput(ns("modal_activity"), label = NULL,
                                   choices = c("Choose activity…" = "", activity_choices),
                                   width = "100%"),
                       uiOutput(ns("hint_modal_activity")))
              )
          ),
          
          # ── Section 2: mode switch ────────────────────────────────────
          div(class = "ca-modal-section",
              div(class = "ca-section-label", "Cost mode"),
              custom_activity_mode_control(ns)
          ),
          
          # ── Section 3a: single CC ──────────────────────────────────────
          conditionalPanel(
            condition = sprintf("input['%s'] == '%s'", ns("modal_mode"), .CA_MODE_LEFT_VALUE),
            div(class = "ca-modal-section",
                div(class = "ca-section-label", "Cost destination"),
                fluidRow(
                  column(7,
                         textInput(ns("single_cc"), label = NULL,
                                   value = "", placeholder = "Cost centre", width = "100%"),
                         uiOutput(ns("hint_single_cc"))),
                  column(5,
                         numericInput(ns("single_amt"), label = NULL,
                                      value = NA_real_, min = 0, step = 0.01, width = "100%"),
                         uiOutput(ns("hint_single_amt")))
                )
            )
          ),
          
          # ── Section 3b: baseline ───────────────────────────────────────
          conditionalPanel(
            condition = sprintf("input['%s'] == '%s'", ns("modal_mode"), .CA_MODE_RIGHT_VALUE),
            div(class = "ca-modal-section",
                div(class = "ca-section-label", "Posting line allocations"),
                lapply(seq_len(.CA_BASELINE_SLOTS), function(i) {
                  div(class = "ca-slot-block",
                      div(class = "ca-slot-label", .CA_BASELINE_SLOT_LABELS[i]),
                      fluidRow(
                        column(7,
                               textInput(ns(paste0("base_cc_", i)), label = NULL,
                                         value = "", placeholder = "Cost centre", width = "100%"),
                               uiOutput(ns(paste0("hint_base_cc_", i)))),
                        column(5,
                               numericInput(ns(paste0("base_amt_", i)), label = NULL,
                                            value = NA_real_, min = 0, step = 0.01, width = "100%"),
                               uiOutput(ns(paste0("hint_base_amt_", i))))
                      )
                  )
                }),
                div(class = "ca-running-total",
                    textOutput(ns("baseline_total"), inline = TRUE))
            )
          )
        )
      ))
    })
    
    # ── Live baseline total ─────────────────────────────────────────────────
    output$baseline_total <- renderText({
      vals <- vapply(
        seq_len(.CA_BASELINE_SLOTS),
        function(i) {
          x <- input[[paste0("base_amt_", i)]]
          if (is.null(x) || length(x) == 0L) return(NA_real_)
          v <- suppressWarnings(as.numeric(x[[1L]]))
          if (length(v) != 1L) return(NA_real_)
          v
        },
        numeric(1)
      )
      total <- sum(vals, na.rm = TRUE)
      paste0("Running total: ", .format_gbp(total))
    })
    
    # ── Validation ──────────────────────────────────────────────────────────
    .validate_modal <- reactive({
      .ca_validate_modal_inputs(reactiveValuesToList(input))
    })
    
    .render_hint <- function(id_name, default_hint = NULL) {
      output[[paste0("hint_", id_name)]] <- renderUI({
        errs <- .validate_modal()
        .field_hint(touched[[id_name]], errs[[id_name]], default_hint)
      })
    }
    
    .render_hint("modal_arm",      "Choose a study arm")
    .render_hint("modal_activity", "Choose an activity")
    .render_hint("single_cc",      "Enter the cost centre")
    .render_hint("single_amt",     "Amount in GBP")
    for (i in seq_len(.CA_BASELINE_SLOTS)) {
      .render_hint(paste0("base_cc_",  i), "Cost centre")
      .render_hint(paste0("base_amt_", i), "Amount (£)")
    }
    
    observe({
      errs <- .validate_modal()
      shinyjs::toggleState("modal_submit", condition = length(errs) == 0)
    })
    
    observeEvent(input$modal_cancel, removeModal())
    
    # ── Submit ──────────────────────────────────────────────────────────────
    observeEvent(input$modal_submit, {
      errs <- .validate_modal()
      if (length(errs) > 0) return(invisible(NULL))
      
      mode <- input$modal_mode %||% .CA_MODE_LEFT_VALUE
      
      rows_df <- if (identical(mode, .CA_MODE_LEFT_VALUE)) {
        tibble(
          cost_centre = trimws(as.character(input$single_cc)),
          amount      = .ca_as_scalar_num(input$single_amt)
        )
      } else {
        tibble(
          cost_centre = vapply(seq_len(.CA_BASELINE_SLOTS),
                               function(i) trimws(as.character(input[[paste0("base_cc_", i)]] %||% "")),
                               character(1)),
          amount      = vapply(seq_len(.CA_BASELINE_SLOTS),
                               function(i) .ca_as_scalar_num(input[[paste0("base_amt_", i)]]),
                               numeric(1))
        )
      }
      
      activity <- list(
        cpms_id     = as.character(shared_state$cpms_id),
        study_site  = as.character(shared_state$study_site %||% NA_character_),
        study_name  = as.character(shared_state$study_name  %||% NA_character_),
        scenario_id = as.character(shared_state$scenario_id %||% NA_character_),
        version_id  = shared_state$template_version_id,
        Study_Arm   = input$modal_arm,
        Activity    = input$modal_activity,
        mode        = mode,
        rows        = rows_df,
        created_by  = if (is.null(auth_state$user_id)) NA_integer_
        else as.integer(auth_state$user_id)
      )
      
      new_id <- tryCatch({
        ca_insert(activity)
      }, error = function(e) {
        showNotification(paste("Failed to add custom activity:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      })
      
      if (!is.null(new_id)) {
        removeModal()
        .bump()
        showNotification(paste0("Added custom activity ", new_id),
                         type = "message", duration = 4)
      }
    })
    
    list(
      invalidation_signal = reactive(invalidation_tick())
    )
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
