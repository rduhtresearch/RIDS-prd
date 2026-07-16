matrix_format_count <- function(value) {
  format(as.integer(value %||% 0L), big.mark = ",", scientific = FALSE)
}

matrix_format_file_size <- function(bytes) {
  if (is.null(bytes) || length(bytes) == 0 || is.na(bytes)) {
    return("Size unavailable")
  }

  units <- c("B", "KB", "MB", "GB")
  unit_index <- min(floor(log(max(bytes, 1), base = 1024)) + 1L, length(units))
  scaled <- bytes / (1024 ^ (unit_index - 1L))
  digits <- if (unit_index == 1L || scaled >= 10) 0L else 1L
  paste0(format(round(scaled, digits), nsmall = digits, trim = TRUE), " ", units[[unit_index]])
}

matrix_format_timestamp <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value[[1]]) || !nzchar(as.character(value[[1]]))) {
    return("Not recorded")
  }

  timestamp <- if (inherits(value, "POSIXt")) {
    value[[1]]
  } else {
    parsed <- as.POSIXct(
      as.character(value[[1]]),
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
    if (is.na(parsed)) as.POSIXct(as.character(value[[1]]), tz = "UTC") else parsed
  }

  if (is.na(timestamp)) as.character(value[[1]]) else format(timestamp, "%d %b %Y, %H:%M")
}

matrix_canonical_column_name <- function(column_name) {
  alias <- unname(COST_CENTRE_MATRIX_COLUMN_ALIASES[column_name])
  if (length(alias) == 0 || is.na(alias)) column_name else alias
}

matrix_split_badge <- function(column_name, populated_count = NULL, compact = FALSE) {
  div(
    class = paste("rids-matrix-split", if (isTRUE(compact)) "is-compact" else ""),
    span(class = "rids-matrix-split-icon", icon("check")),
    span(
      class = "rids-matrix-split-copy",
      strong(column_name),
      if (!is.null(populated_count) && !isTRUE(compact)) {
        tags$small(paste(matrix_format_count(populated_count), "mapped rows"))
      }
    )
  )
}

matrix_metric <- function(label, value, icon_name) {
  div(
    class = "rids-matrix-metric",
    span(class = "rids-matrix-metric-icon", icon(icon_name)),
    div(
      span(class = "rids-matrix-metric-label", label),
      strong(value)
    )
  )
}

matrix_table_columns <- function(data, split_columns) {
  stats::setNames(lapply(names(data), function(column_name) {
    canonical_name <- matrix_canonical_column_name(column_name)
    is_split <- canonical_name %in% split_columns
    is_key <- canonical_name %in% COST_CENTRE_MATRIX_REQUIRED_COLUMNS
    class_names <- paste(
      if (is_split) "rids-matrix-table-split" else "",
      if (is_key) "rids-matrix-table-key" else ""
    )

    reactable::colDef(
      name = column_name,
      minWidth = if (is_key) 175 else 145,
      sticky = if (identical(canonical_name, "Department")) "left" else NULL,
      class = class_names,
      headerClass = class_names
    )
  }), names(data))
}

costCentreMatrixUI <- function(id) {
  ns <- NS(id)

  div(
    class = "rids-page rids-matrix-page",
    div(
      class = "rids-page-header rids-matrix-page-header",
      div(
        div(class = "rids-page-eyebrow", "Operations"),
        h1("Cost Centre Matrix"),
        p("Review the active routing matrix and its live split columns.")
      ),
      div(
        class = "rids-matrix-header-tools",
        uiOutput(ns("header_action")),
        div(class = "rids-page-mark", icon("table"))
      )
    ),
    tags$section(
      class = "rids-matrix-surface rids-matrix-overview",
      uiOutput(ns("matrix_overview"))
    ),
    tags$section(
      class = "rids-matrix-surface",
      div(
        class = "rids-matrix-section-heading",
        div(
          span(class = "rids-matrix-section-icon", icon("columns")),
          div(
            h2("Active split columns"),
            p("Recognised posting splits currently used for cost-centre routing.")
          )
        ),
        uiOutput(ns("split_count_badge"))
      ),
      uiOutput(ns("active_split_columns"))
    ),
    tags$section(
      class = "rids-matrix-surface rids-matrix-viewer",
      div(
        class = "rids-matrix-section-heading",
        div(
          span(class = "rids-matrix-section-icon", icon("th-list")),
          div(
            h2("Matrix viewer"),
            p("Sort and inspect the current matrix without changing it.")
          )
        ),
        uiOutput(ns("viewer_action"))
      ),
      uiOutput(ns("matrix_table_state"))
    )
  )
}

costCentreMatrixServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
    refresh <- reactiveVal(0L)
    upload_candidate <- reactiveVal(NULL)

    allowed_split_columns <- reactive({
      req(auth_state$logged_in)
      cc_allowed_posting_line_types()
    })

    current_matrix <- reactive({
      refresh()
      req(auth_state$logged_in)

      file_path <- cc_get_setting("cost_centre_matrix_file", "")
      if (!nzchar(file_path)) {
        return(list(configured = FALSE, valid = FALSE, message = "No active matrix is configured."))
      }

      file_info <- if (file.exists(file_path)) file.info(file_path) else NULL
      original_name <- cc_get_setting("cost_centre_matrix_original_name", basename(file_path))
      uploaded_by <- cc_get_setting("cost_centre_matrix_uploaded_by", "System configuration")
      uploaded_at <- cc_get_setting("cost_centre_matrix_uploaded_at", "")
      if (!nzchar(uploaded_at) && !is.null(file_info)) uploaded_at <- file_info$mtime[[1]]

      inspection <- tryCatch(
        cc_inspect_cost_centre_matrix(
          file_path,
          allowed_posting_line_types = allowed_split_columns()
        ),
        error = function(e) e
      )

      if (inherits(inspection, "error")) {
        return(list(
          configured = TRUE,
          valid = FALSE,
          message = conditionMessage(inspection),
          file_path = file_path,
          file_name = original_name,
          uploaded_by = uploaded_by,
          uploaded_at = uploaded_at,
          file_size = if (is.null(file_info)) NA_real_ else file_info$size[[1]]
        ))
      }

      list(
        configured = TRUE,
        valid = TRUE,
        message = sprintf("Valid with %s active split columns", length(inspection$split_columns)),
        file_path = file_path,
        file_name = original_name,
        uploaded_by = uploaded_by,
        uploaded_at = uploaded_at,
        file_size = if (is.null(file_info)) NA_real_ else file_info$size[[1]],
        inspection = inspection
      )
    })

    output$header_action <- renderUI({
      req(auth_state$logged_in)

      if (isTRUE(is_admin(auth_state$role))) {
        actionButton(
          session$ns("open_matrix_upload"),
          label = tagList(icon("upload"), span("Upload new matrix")),
          class = "btn-primary rids-matrix-upload-button",
          title = "Upload a new cost centre matrix"
        )
      } else {
        span(class = "rids-matrix-read-only", icon("lock"), " View only")
      }
    })

    output$matrix_overview <- renderUI({
      current <- current_matrix()

      if (!isTRUE(current$configured)) {
        return(
          div(
            class = "rids-matrix-overview-empty",
            span(class = "rids-matrix-file-mark", icon("file-upload")),
            div(
              h2("No active matrix"),
              p("Upload a validated CSV or XLSX matrix to enable cost-centre routing.")
            )
          )
        )
      }

      row_count <- if (isTRUE(current$valid)) current$inspection$row_count else 0L
      column_count <- if (isTRUE(current$valid)) current$inspection$column_count else 0L
      split_count <- if (isTRUE(current$valid)) length(current$inspection$split_columns) else 0L
      extension <- toupper(tools::file_ext(current$file_name))
      if (!nzchar(extension)) extension <- "FILE"

      tagList(
        div(
          class = "rids-matrix-file-summary",
          span(class = "rids-matrix-file-mark", icon(if (identical(extension, "XLSX")) "file-excel" else "file-csv")),
          div(
            class = "rids-matrix-file-copy",
            span(class = "rids-matrix-overline", "Current matrix"),
            h2(current$file_name),
            p(paste(extension, matrix_format_file_size(current$file_size), sep = " · ")),
            tags$small(paste("Uploaded by", current$uploaded_by))
          ),
          span(
            class = paste("rids-matrix-validity", if (isTRUE(current$valid)) "is-valid" else "is-invalid"),
            icon(if (isTRUE(current$valid)) "check-circle" else "exclamation-triangle"),
            if (isTRUE(current$valid)) " Active" else " Needs attention"
          )
        ),
        if (!isTRUE(current$valid)) {
          div(class = "rids-matrix-alert is-error", icon("exclamation-circle"), span(current$message))
        },
        div(
          class = "rids-matrix-metrics",
          matrix_metric("Rows", matrix_format_count(row_count), "list-ol"),
          matrix_metric("Columns", matrix_format_count(column_count), "columns"),
          matrix_metric("Active splits", matrix_format_count(split_count), "project-diagram"),
          matrix_metric("Last replaced", matrix_format_timestamp(current$uploaded_at), "clock")
        )
      )
    })

    output$split_count_badge <- renderUI({
      current <- current_matrix()
      if (!isTRUE(current$valid)) return(NULL)

      span(
        class = "rids-matrix-count-badge",
        paste(length(current$inspection$split_columns), "active")
      )
    })

    output$active_split_columns <- renderUI({
      current <- current_matrix()

      if (!isTRUE(current$valid)) {
        return(
          div(
            class = "rids-matrix-inline-empty",
            icon(if (isTRUE(current$configured)) "exclamation-circle" else "info-circle"),
            span(if (isTRUE(current$configured)) current$message else "No split columns are active yet.")
          )
        )
      }

      split_columns <- current$inspection$split_columns
      populated_counts <- current$inspection$populated_counts
      inactive_columns <- setdiff(allowed_split_columns(), split_columns)

      tagList(
        div(
          class = "rids-matrix-split-grid",
          lapply(split_columns, function(column_name) {
            matrix_split_badge(column_name, unname(populated_counts[column_name] %||% 0L))
          })
        ),
        if (length(inactive_columns) > 0) {
          div(
            class = "rids-matrix-inactive-row",
            span(class = "rids-matrix-inactive-label", "Available but inactive"),
            div(
              class = "rids-matrix-inactive-list",
              lapply(inactive_columns, function(column_name) span(column_name))
            )
          )
        }
      )
    })

    output$viewer_action <- renderUI({
      current <- current_matrix()
      if (!isTRUE(current$valid)) {
        return(
          tags$button(
            type = "button",
            class = "btn btn-secondary rids-matrix-icon-button",
            disabled = "disabled",
            title = "Download active matrix",
            icon("download")
          )
        )
      }

      downloadButton(
        session$ns("download_matrix"),
        label = "Download",
        class = "btn-secondary"
      )
    })

    output$matrix_table_state <- renderUI({
      current <- current_matrix()
      if (!isTRUE(current$valid)) {
        return(
          div(
            class = "rids-matrix-table-empty",
            span(class = "rids-matrix-table-empty-icon", icon("table")),
            h3("Matrix unavailable"),
            p(if (isTRUE(current$configured)) "Resolve the matrix validation issue to view its rows." else "Upload a matrix to populate this viewer.")
          )
        )
      }

      reactableOutput(session$ns("matrix_table"))
    })

    output$matrix_table <- renderReactable({
      current <- current_matrix()
      req(isTRUE(current$valid))

      reactable(
        current$inspection$data,
        columns = matrix_table_columns(current$inspection$data, current$inspection$split_columns),
        searchable = FALSE,
        sortable = TRUE,
        resizable = TRUE,
        pagination = TRUE,
        defaultPageSize = 20,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 20, 50, 100),
        highlight = TRUE,
        compact = TRUE,
        wrap = FALSE,
        fullWidth = TRUE,
        class = "rids-matrix-table",
        language = reactableLang(noData = "No matrix rows found")
      )
    })

    output$download_matrix <- downloadHandler(
      filename = function() {
        current <- current_matrix()
        req(isTRUE(current$valid))
        gsub("[^A-Za-z0-9._ -]", "_", basename(current$file_name))
      },
      content = function(file) {
        current <- current_matrix()
        req(isTRUE(current$valid), file.exists(current$file_path))
        if (!file.copy(current$file_path, file, overwrite = TRUE)) {
          stop("Unable to copy the active matrix for download.")
        }
      }
    )

    show_upload_modal <- function() {
      showModal(modalDialog(
        title = tagList(icon("upload"), " Upload new matrix"),
        div(
          class = "rids-matrix-upload-modal",
          div(
            class = "rids-matrix-upload-intro",
            span(class = "rids-matrix-upload-step is-current", "1", tags$small("Select")),
            span(class = "rids-matrix-upload-line"),
            span(class = "rids-matrix-upload-step", "2", tags$small("Validate")),
            span(class = "rids-matrix-upload-line"),
            span(class = "rids-matrix-upload-step", "3", tags$small("Activate"))
          ),
          fileInput(
            session$ns("matrix_upload"),
            "Matrix file",
            accept = c(".csv", ".xlsx"),
            buttonLabel = "Choose file",
            placeholder = "CSV or XLSX"
          ),
          uiOutput(session$ns("upload_preview"))
        ),
        footer = tagList(
          modalButton("Cancel"),
          div(class = "rids-matrix-upload-footer-action", uiOutput(session$ns("upload_primary_action")))
        ),
        size = "l",
        easyClose = FALSE
      ))
    }

    observeEvent(input$open_matrix_upload, {
      req(auth_state$logged_in, isTRUE(is_admin(auth_state$role)))
      upload_candidate(NULL)
      show_upload_modal()
    })

    observeEvent(input$matrix_upload, {
      req(auth_state$logged_in, isTRUE(is_admin(auth_state$role)))
      upload <- input$matrix_upload
      req(!is.null(upload), file.exists(upload$datapath))

      inspection <- tryCatch(
        cc_inspect_cost_centre_matrix(
          upload$datapath,
          allowed_posting_line_types = allowed_split_columns(),
          file_name = upload$name
        ),
        error = function(e) e
      )
      data <- if (!inherits(inspection, "error")) {
        inspection$data
      } else {
        tryCatch(
          cc_read_cost_centre_matrix(upload$datapath, file_name = upload$name),
          error = function(e) NULL
        )
      }

      upload_candidate(list(
        upload = upload,
        valid = !inherits(inspection, "error"),
        message = if (inherits(inspection, "error")) conditionMessage(inspection) else "Validation passed. This matrix is ready for review.",
        inspection = if (inherits(inspection, "error")) NULL else inspection,
        data = data
      ))
    }, ignoreNULL = TRUE)

    output$upload_preview <- renderUI({
      candidate <- upload_candidate()
      if (is.null(candidate)) {
        return(
          div(
            class = "rids-matrix-upload-placeholder",
            span(class = "rids-matrix-upload-placeholder-icon", icon("file-alt")),
            h3("Choose a matrix to preview"),
            p("CSV and XLSX files are checked before the active matrix changes.")
          )
        )
      }

      row_count <- if (!is.null(candidate$data)) nrow(candidate$data) else 0L
      column_count <- if (!is.null(candidate$data)) ncol(candidate$data) else 0L

      tagList(
        div(
          class = paste("rids-matrix-alert", if (isTRUE(candidate$valid)) "is-success" else "is-error"),
          icon(if (isTRUE(candidate$valid)) "check-circle" else "exclamation-circle"),
          div(strong(if (isTRUE(candidate$valid)) "Validation passed" else "Validation failed"), span(candidate$message))
        ),
        div(
          class = "rids-matrix-upload-file",
          div(
            span(class = "rids-matrix-overline", "Selected file"),
            strong(candidate$upload$name),
            tags$small(paste(matrix_format_count(row_count), "rows ·", matrix_format_count(column_count), "columns ·", matrix_format_file_size(candidate$upload$size)))
          )
        ),
        if (isTRUE(candidate$valid)) {
          div(
            class = "rids-matrix-upload-splits",
            span(class = "rids-matrix-upload-label", "Detected active splits"),
            div(
              class = "rids-matrix-split-grid is-preview",
              lapply(candidate$inspection$split_columns, function(column_name) {
                matrix_split_badge(
                  column_name,
                  unname(candidate$inspection$populated_counts[column_name] %||% 0L),
                  compact = TRUE
                )
              })
            )
          )
        },
        if (!is.null(candidate$data) && nrow(candidate$data) > 0) {
          div(
            class = "rids-matrix-upload-table-wrap",
            div(class = "rids-matrix-upload-label", paste("Preview · first", min(8L, nrow(candidate$data)), "rows")),
            reactableOutput(session$ns("upload_preview_table"))
          )
        }
      )
    })

    output$upload_preview_table <- renderReactable({
      candidate <- upload_candidate()
      req(!is.null(candidate), !is.null(candidate$data), nrow(candidate$data) > 0)

      preview_data <- utils::head(candidate$data, 8L)
      split_columns <- if (isTRUE(candidate$valid)) candidate$inspection$split_columns else character()

      reactable(
        preview_data,
        columns = matrix_table_columns(preview_data, split_columns),
        pagination = FALSE,
        sortable = FALSE,
        compact = TRUE,
        wrap = FALSE,
        fullWidth = TRUE,
        class = "rids-matrix-table rids-matrix-preview-table"
      )
    })

    output$upload_primary_action <- renderUI({
      candidate <- upload_candidate()
      current <- current_matrix()
      is_ready <- !is.null(candidate) && isTRUE(candidate$valid)

      actionButton(
        session$ns("review_matrix_replacement"),
        label = tagList(
          span(if (isTRUE(current$configured)) "Review replacement" else "Review activation"),
          icon("arrow-right")
        ),
        class = "btn-primary",
        disabled = if (!is_ready) "disabled" else NULL
      )
    })

    observeEvent(input$review_matrix_replacement, {
      req(auth_state$logged_in, isTRUE(is_admin(auth_state$role)))
      candidate <- upload_candidate()
      req(!is.null(candidate), isTRUE(candidate$valid))
      current <- current_matrix()

      current_splits <- if (isTRUE(current$valid)) current$inspection$split_columns else character()
      new_splits <- candidate$inspection$split_columns
      added_splits <- setdiff(new_splits, current_splits)
      removed_splits <- setdiff(current_splits, new_splits)
      is_replacement <- isTRUE(current$configured)

      showModal(modalDialog(
        title = if (is_replacement) "Replace active matrix?" else "Activate this matrix?",
        div(
          class = "rids-matrix-confirm",
          div(
            class = "rids-matrix-confirm-files",
            div(
              span("Current"),
              strong(if (is_replacement) current$file_name else "No active matrix")
            ),
            span(class = "rids-matrix-confirm-arrow", icon("arrow-right")),
            div(
              span("New"),
              strong(candidate$upload$name)
            )
          ),
          div(
            class = "rids-matrix-confirm-stats",
            span(strong(matrix_format_count(candidate$inspection$row_count)), " rows"),
            span(strong(matrix_format_count(candidate$inspection$column_count)), " columns"),
            span(strong(length(new_splits)), " active splits")
          ),
          if (length(added_splits) > 0) {
            div(class = "rids-matrix-change is-added", strong("Added splits"), span(paste(added_splits, collapse = ", ")))
          },
          if (length(removed_splits) > 0) {
            div(class = "rids-matrix-change is-removed", strong("Removed splits"), span(paste(removed_splits, collapse = ", ")))
          },
          p(class = "rids-matrix-confirm-note", "New ICT processing will use this matrix immediately after activation.")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            session$ns("confirm_matrix_replacement"),
            if (is_replacement) "Replace active matrix" else "Activate matrix",
            class = "btn-primary"
          )
        ),
        easyClose = FALSE
      ))
    })

    observeEvent(input$confirm_matrix_replacement, {
      req(auth_state$logged_in, isTRUE(is_admin(auth_state$role)))
      candidate <- upload_candidate()
      req(!is.null(candidate), isTRUE(candidate$valid), file.exists(candidate$upload$datapath))

      validation <- validate_cost_centre_matrix_file(
        candidate$upload$datapath,
        allowed_posting_line_types = allowed_split_columns(),
        file_name = candidate$upload$name
      )
      if (!isTRUE(validation$valid)) {
        removeModal()
        showNotification(validation$message, type = "error", duration = 10)
        return()
      }

      tryCatch({
        matrix_dir <- file.path(ICT_UPLOAD_DIR, "cost_centre_matrices")
        if (!dir.exists(matrix_dir)) dir.create(matrix_dir, recursive = TRUE)

        extension <- validation$file_extension
        saved_path <- file.path(matrix_dir, paste0("active_cost_centre_matrix.", extension))
        if (!file.copy(candidate$upload$datapath, saved_path, overwrite = TRUE)) {
          stop("Unable to copy the uploaded matrix into active storage.")
        }
        if (!isTRUE(set_app_setting_value("cost_centre_matrix_file", saved_path))) {
          stop("Unable to update the active matrix setting.")
        }

        uploaded_by <- auth_state$name %||% auth_state$username %||% "Administrator"
        set_app_setting_value("cost_centre_matrix_original_name", basename(candidate$upload$name))
        set_app_setting_value("cost_centre_matrix_uploaded_by", uploaded_by)
        set_app_setting_value(
          "cost_centre_matrix_uploaded_at",
          format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        )

        log_event(
          level = "INFO",
          area = "matrix",
          message = "Cost centre matrix updated",
          user_id = auth_state$user_id,
          username = auth_state$username,
          session_id = auth_state$session_id,
          details = list(
            file_name = basename(candidate$upload$name),
            file_path = saved_path,
            row_count = validation$row_count,
            split_columns = validation$split_columns
          )
        )

        removeModal()
        upload_candidate(NULL)
        refresh(refresh() + 1L)
        showNotification("Cost centre matrix is now active.", type = "message", duration = 5)
      }, error = function(e) {
        removeModal()
        app_log_exception("matrix", "Cost centre matrix save failed", e)
        showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    })
  })
}
