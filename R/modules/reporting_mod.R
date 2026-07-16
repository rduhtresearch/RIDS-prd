EDGE_PROJECT_COST_EVENTS_URL <- "https://www.edge.nhs.uk/api/public/v1/ProjectCostEvents"
EDGE_PROJECT_SITE_COST_EVENTS_URL <- "https://www.edge.nhs.uk/api/public/v1/ProjectSiteCostEvents"

EDGE_COST_EVENT_COLUMNS <- c(
  "event_type", "edgeProjectId", "localProjectReference", "projectTitle",
  "edgeProjectSiteId", "site", "description", "analysisCode", "costCategory",
  "cost", "date", "invoiceNumber", "invoiced", "department", "comments"
)

edge_cost_events_query <- function(year = NULL, month = NULL, day = NULL, period = NULL) {
  values <- list(Year = year, Month = month, Day = day, Period = period)
  values <- lapply(values, function(value) {
    if (is.null(value) || length(value) == 0L || is.na(value[[1]])) {
      return(NULL)
    }

    value <- trimws(as.character(value[[1]]))
    if (!nzchar(value)) NULL else value
  })

  values[!vapply(values, is.null, logical(1))]
}

edge_cost_events_picker_value <- function(value, component) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1]]) || !nzchar(as.character(value[[1]]))) {
    return(NULL)
  }

  date <- tryCatch(suppressWarnings(as.Date(value[[1]])), error = function(e) as.Date(NA))
  if (is.na(date)) {
    stop(sprintf("Choose a valid %s.", component))
  }

  switch(
    component,
    year = format(date, "%Y"),
    month = as.character(as.integer(format(date, "%m"))),
    day = as.character(as.integer(format(date, "%d"))),
    stop("Unsupported date component.")
  )
}

edge_cost_events_picker_filters <- function(year = NULL, month = NULL, day = NULL) {
  list(
    year = edge_cost_events_picker_value(year, "year"),
    month = edge_cost_events_picker_value(month, "month"),
    day = edge_cost_events_picker_value(day, "day")
  )
}

edge_cost_events_date_input <- function(input_id, label, format, startview, minview, width) {
  picker <- dateInput(
    input_id,
    label,
    value = NULL,
    format = format,
    startview = startview,
    weekstart = 1,
    width = width
  )

  htmltools::tagQuery(picker)$
    find("input")$
    addAttrs(`data-date-min-view-mode` = minview)$
    allTags()
}

edge_cost_events_filter_controls <- function(ns, mode) {
  date_fields <- if (identical(mode, "range")) {
    list(
      dateInput(ns("range_from"), "From", value = NULL, format = "dd M yyyy", weekstart = 1, width = "210px"),
      dateInput(ns("range_to"), "To", value = NULL, format = "dd M yyyy", weekstart = 1, width = "210px")
    )
  } else {
    list(
      edge_cost_events_date_input(ns("year"), "Year", "yyyy", "decade", "years", "150px"),
      edge_cost_events_date_input(ns("month"), "Month", "MM", "year", "months", "170px"),
      edge_cost_events_date_input(ns("day"), "Day", "dd", "month", "days", "150px")
    )
  }

  tagList(
    div(
      class = "rids-filter-bar rids-inline-filters rids-reporting-filters",
      date_fields,
      textInput(ns("period"), "Period (optional)", value = "", placeholder = "EDGE period value", width = "260px"),
      div(
        class = "rids-reporting-action",
        actionButton(ns("run_report"), "Run report", icon = icon("play"), class = "btn-primary")
      )
    ),
    tags$small(
      class = "rids-reporting-hint",
      "Period is passed to EDGE exactly as entered. Leave it blank unless you know the period value used in your EDGE configuration."
    )
  )
}

edge_cost_events_empty <- function() {
  result <- as.data.frame(
    stats::setNames(
      replicate(length(EDGE_COST_EVENT_COLUMNS), character(0), simplify = FALSE),
      EDGE_COST_EVENT_COLUMNS
    ),
    stringsAsFactors = FALSE
  )
  result$cost <- numeric(0)
  result$invoiced <- logical(0)
  result
}

edge_cost_events_rows <- function(events, event_type) {
  if (is.null(events) || length(events) == 0L) {
    return(edge_cost_events_empty())
  }

  if (is.data.frame(events)) {
    events <- lapply(seq_len(nrow(events)), function(index) as.list(events[index, , drop = FALSE]))
  }

  rows <- lapply(events, function(event) {
    values <- stats::setNames(vector("list", length(EDGE_COST_EVENT_COLUMNS)), EDGE_COST_EVENT_COLUMNS)
    values$event_type <- event_type

    for (column in setdiff(EDGE_COST_EVENT_COLUMNS, "event_type")) {
      value <- event[[column]]
      values[[column]] <- if (is.null(value) || length(value) == 0L) NA else value[[1]]
    }

    as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
  })

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

flatten_edge_cost_events <- function(payload) {
  if (is.null(payload) || !is.list(payload)) {
    stop("EDGE returned an unexpected Cost Events response.")
  }

  event_fields <- c("projectCostEvents", "projectSiteCostEvents")
  if (!any(event_fields %in% names(payload))) {
    stop("EDGE returned an unexpected Cost Events response.")
  }

  result <- rbind(
    edge_cost_events_rows(payload$projectCostEvents, "Project"),
    edge_cost_events_rows(payload$projectSiteCostEvents, "Project Site")
  )

  result[, EDGE_COST_EVENT_COLUMNS, drop = FALSE]
}

edge_cost_events_error <- function(status_code, fallback = NULL) {
  message <- switch(
    as.character(status_code),
    `400` = "EDGE could not process those filters. Check the values and try again.",
    `401` = "EDGE rejected the saved API key. Update it in Integrations and try again.",
    `429` = "EDGE is temporarily rate limiting requests. Wait a minute and try again.",
    fallback %||% sprintf("EDGE returned an error (HTTP %s). Try again later.", status_code)
  )

  structure(
    list(message = message, call = NULL, status_code = status_code),
    class = c("edge_cost_events_error", "error", "condition")
  )
}

edge_cost_events_url <- function(base_url, query = list()) {
  if (length(query) == 0L) {
    return(base_url)
  }

  encoded <- paste0(
    utils::URLencode(names(query), reserved = TRUE),
    "=",
    vapply(query, utils::URLencode, character(1), reserved = TRUE)
  )
  paste0(base_url, "?", paste(encoded, collapse = "&"))
}

edge_cost_events_http_status <- function(message) {
  match <- regexec("HTTP[^0-9]+([0-9]{3})", message, ignore.case = TRUE)
  parts <- regmatches(message, match)[[1]]
  if (length(parts) < 2L) NA_integer_ else as.integer(parts[[2]])
}

perform_edge_cost_events_request <- function(url, headers, timeout = 30) {
  old_timeout <- getOption("timeout")
  options(timeout = timeout)
  on.exit(options(timeout = old_timeout), add = TRUE)
  warning_message <- ""

  tryCatch(
    withCallingHandlers({
      connection <- base::url(
        url,
        open = "rb",
        method = "libcurl",
        headers = unlist(headers, use.names = TRUE)
      )
      on.exit(close(connection), add = TRUE)

      body <- paste(readLines(connection, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      list(status_code = 200L, body = body)
    }, warning = function(warning) {
      warning_message <<- conditionMessage(warning)
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      status_code <- edge_cost_events_http_status(paste(warning_message, conditionMessage(e)))
      if (!is.na(status_code)) {
        return(list(status_code = status_code, body = ""))
      }

      stop("Unable to connect to EDGE. Check your connection and try again.", call. = FALSE)
    }
  )
}

edge_cost_events_range <- function(date_from = NULL, date_to = NULL) {
  is_blank <- function(value) {
    is.null(value) || length(value) == 0L || is.na(value[[1]]) || !nzchar(as.character(value[[1]]))
  }
  if (is_blank(date_from) && is_blank(date_to)) {
    return(NULL)
  }
  if (is_blank(date_from) || is_blank(date_to)) {
    stop("Choose both From and To dates for a range.")
  }

  date_from <- tryCatch(as.Date(date_from[[1]]), error = function(e) as.Date(NA))
  date_to <- tryCatch(as.Date(date_to[[1]]), error = function(e) as.Date(NA))
  if (is.na(date_from) || is.na(date_to)) {
    stop("Choose valid From and To dates for the range.")
  }
  if (date_from > date_to) {
    stop("The From date must be on or before the To date.")
  }

  list(from = date_from, to = date_to)
}

filter_edge_cost_events_range <- function(data, range) {
  if (is.null(range) || nrow(data) == 0L) {
    return(data)
  }

  event_dates <- suppressWarnings(as.Date(substr(as.character(data$date), 1L, 10L)))
  result <- data[!is.na(event_dates) & event_dates >= range$from & event_dates <= range$to, , drop = FALSE]
  rownames(result) <- NULL
  result
}

edge_cost_events_range_years <- function(range) {
  if (is.null(range)) {
    return(character(0))
  }

  as.character(seq.int(
    as.integer(format(range$from, "%Y")),
    as.integer(format(range$to, "%Y"))
  ))
}

fetch_edge_cost_events_query <- function(api_key, query, perform) {
  endpoints <- list(
    projectCostEvents = EDGE_PROJECT_COST_EVENTS_URL,
    projectSiteCostEvents = EDGE_PROJECT_SITE_COST_EVENTS_URL
  )

  payload <- lapply(names(endpoints), function(response_field) {
    response <- perform(
      url = edge_cost_events_url(endpoints[[response_field]], query),
      headers = list(apikey = api_key, Accept = "application/json"),
      timeout = 30
    )
    status_code <- as.integer(response$status_code %||% NA_integer_)

    if (is.na(status_code) || status_code < 200L || status_code >= 300L) {
      stop(edge_cost_events_error(status_code))
    }

    parsed <- tryCatch(
      jsonlite::fromJSON(response$body %||% "", simplifyVector = FALSE),
      error = function(e) stop("EDGE returned a response that RIDS could not read.")
    )

    if (!is.list(parsed) || !response_field %in% names(parsed)) {
      stop("EDGE returned an unexpected Cost Events response.")
    }
    parsed[[response_field]]
  })
  names(payload) <- names(endpoints)

  flatten_edge_cost_events(payload)
}

fetch_edge_cost_events_range_source <- function(api_key, range, period, perform) {
  results <- lapply(edge_cost_events_range_years(range), function(year) {
    fetch_edge_cost_events_query(
      api_key,
      edge_cost_events_query(year = year, period = period),
      perform
    )
  })

  result <- do.call(rbind, results)
  rownames(result) <- NULL
  result
}

fetch_edge_cost_events <- function(api_key, year = NULL, month = NULL, day = NULL,
                                   period = NULL, range_from = NULL, range_to = NULL,
                                   perform = perform_edge_cost_events_request) {
  api_key <- trimws(as.character(api_key %||% ""))
  if (!nzchar(api_key)) {
    stop("An EDGE API key is required.")
  }

  range <- edge_cost_events_range(range_from, range_to)
  component_query <- if (is.null(range)) {
    edge_cost_events_query(year, month, day, period)
  } else {
    edge_cost_events_query(period = period)
  }

  if (is.null(range)) {
    return(fetch_edge_cost_events_query(api_key, component_query, perform))
  }

  result <- fetch_edge_cost_events_range_source(api_key, range, period, perform)
  filter_edge_cost_events_range(result, range)
}

reportingUI <- function(id) {
  ns <- NS(id)

  div(
    class = "rids-page rids-form-page",
    div(
      class = "rids-page-header",
      div(
        div(class = "rids-page-eyebrow", "Insights"),
        h1("Reporting"),
        p("Pull project and site cost event data directly from EDGE.")
      ),
      div(class = "rids-page-mark", icon("chart-line"))
    ),
    uiOutput(ns("edge_key_status")),
    bs4Card(
      title = tagList(icon("sliders-h"), " Project and site Cost Events filters"),
      width = 12,
      status = "primary",
      solidHeader = FALSE,
      div(
        style = "margin-bottom: 0.5rem;",
        selectInput(
          ns("filter_mode"),
          "Date filter mode",
          choices = c("Year / Month / Day" = "components", "Date range" = "range"),
          selected = "components",
          width = "240px"
        )
      ),
      uiOutput(ns("date_filter_controls"))
    ),
    bs4Card(
      title = tagList(icon("table"), " Project and site Cost Events"),
      width = 12,
      status = "white",
      solidHeader = FALSE,
      uiOutput(ns("report_status")),
      div(
        class = "rids-table-region",
        role = "region",
        `aria-label` = "Project and site cost events table",
        reactableOutput(ns("cost_events_table"))
      )
    )
  )
}

reportingServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
    report_data <- reactiveVal(NULL)
    report_status <- reactiveVal(list(type = "idle", message = "Choose any filters, then run the report."))
    range_cache <- reactiveValues(period = NULL, years = NULL, data = NULL, fetched_at = NULL)
    if (is.null(session$userData$edge_credential_refresh)) {
      session$userData$edge_credential_refresh <- reactiveVal(0L)
    }

    clear_component_pickers <- function() {
      component_ids <- vapply(c("year", "month", "day"), session$ns, character(1))
      selector <- paste0("#", component_ids, collapse = ",")
      shinyjs::runjs(sprintf(
        "$('%s').datepicker('clearDates').val('').trigger('change');",
        selector
      ))
    }

    output$date_filter_controls <- renderUI({
      edge_cost_events_filter_controls(session$ns, input$filter_mode %||% "components")
    })

    observeEvent(input$filter_mode, {
      if (identical(input$filter_mode, "components")) {
        session$onFlushed(clear_component_pickers, once = TRUE)
      }
    }, ignoreInit = FALSE)

    edge_key_status <- reactive({
      req(auth_state$logged_in)
      session$userData$edge_credential_refresh()
      get_user_api_credential_status(auth_state$user_id, "edge")
    })

    output$edge_key_status <- renderUI({
      status <- edge_key_status()
      if (isTRUE(status$configured)) {
        return(NULL)
      }

      div(
        class = "alert alert-warning",
        icon("key"),
        tags$strong(" EDGE API key required. "),
        "Configure your key in Integrations before running this report."
      )
    })

    output$report_status <- renderUI({
      status <- report_status()
      colours <- c(idle = "#697786", loading = "#1769aa", success = "#2e7d32", empty = "#697786", error = "#b42318")
      icons <- c(idle = "info-circle", loading = "spinner", success = "check-circle", empty = "inbox", error = "exclamation-circle")

      div(
        style = sprintf("margin-bottom: 1rem; color: %s;", colours[[status$type]] %||% colours[["idle"]]),
        icon(icons[[status$type]] %||% icons[["idle"]], class = if (identical(status$type, "loading")) "fa-spin" else NULL),
        paste0(" ", status$message)
      )
    })

    observeEvent(input$run_report, {
      req(auth_state$logged_in)

      api_key <- get_user_api_credential(auth_state$user_id, "edge")
      if (is.null(api_key) || !nzchar(api_key)) {
        report_status(list(type = "error", message = "Configure your EDGE API key in Integrations before running this report."))
        showNotification("An EDGE API key is required.", type = "warning", duration = 6)
        return()
      }

      filter_mode <- input$filter_mode %||% "components"
      range <- if (identical(filter_mode, "range")) {
        tryCatch(
          edge_cost_events_range(input$range_from, input$range_to),
          error = function(e) e
        )
      } else {
        NULL
      }
      if (inherits(range, "error")) {
        report_status(list(type = "error", message = conditionMessage(range)))
        showNotification(conditionMessage(range), type = "warning", duration = 6)
        return()
      }
      if (identical(filter_mode, "range") && is.null(range)) {
        message <- "Choose both From and To dates for the range."
        report_status(list(type = "error", message = message))
        showNotification(message, type = "warning", duration = 6)
        return()
      }

      picker_filters <- if (identical(filter_mode, "range")) {
        list(year = NULL, month = NULL, day = NULL)
      } else {
        tryCatch(
          edge_cost_events_picker_filters(input$year, input$month, input$day),
          error = function(e) e
        )
      }
      if (inherits(picker_filters, "error")) {
        report_status(list(type = "error", message = conditionMessage(picker_filters)))
        showNotification(conditionMessage(picker_filters), type = "warning", duration = 6)
        return()
      }

      filters <- edge_cost_events_query(
        picker_filters$year,
        picker_filters$month,
        picker_filters$day,
        input$period
      )
      filter_log <- if (!is.null(range)) {
        sprintf("Range=%s..%s", range$from, range$to)
      } else if (length(filters) == 0L) {
        "none"
      } else {
        paste(names(filters), unlist(filters), sep = "=", collapse = ",")
      }
      period_value <- trimws(as.character(input$period %||% ""))
      range_years <- edge_cost_events_range_years(range)
      cache_valid <- !is.null(range) &&
        !is.null(range_cache$data) &&
        identical(range_cache$period, period_value) &&
        identical(range_cache$years, range_years) &&
        !is.null(range_cache$fetched_at) &&
        as.numeric(difftime(Sys.time(), range_cache$fetched_at, units = "secs")) < 300
      app_log_info("reporting", "EDGE Cost Events request started", list(
        user_id = auth_state$user_id,
        filters = filter_log,
        range_cache = if (is.null(range)) NA else cache_valid
      ))
      report_status(list(type = "loading", message = "Requesting Cost Events from EDGE..."))

      result <- tryCatch(
        {
          if (is.null(range)) {
            fetch_edge_cost_events(
              api_key = api_key,
              year = picker_filters$year,
              month = picker_filters$month,
              day = picker_filters$day,
              period = input$period
            )
          } else {
            source_data <- if (cache_valid) {
              range_cache$data
            } else {
              fetched <- fetch_edge_cost_events_range_source(
                api_key = api_key,
                range = range,
                period = input$period,
                perform = perform_edge_cost_events_request
              )
              range_cache$period <- period_value
              range_cache$years <- range_years
              range_cache$data <- fetched
              range_cache$fetched_at <- Sys.time()
              fetched
            }
            filter_edge_cost_events_range(source_data, range)
          }
        },
        error = function(e) e
      )

      if (inherits(result, "error")) {
        app_log_exception("reporting", "EDGE Cost Events request failed", result, list(user_id = auth_state$user_id))
        report_status(list(type = "error", message = conditionMessage(result)))
        showNotification(conditionMessage(result), type = "error", duration = 8)
        return()
      }

      report_data(result)
      if (nrow(result) == 0L) {
        report_status(list(type = "empty", message = "No Cost Events matched those filters."))
      } else {
        report_status(list(type = "success", message = sprintf("Loaded %s Cost Event%s.", nrow(result), if (nrow(result) == 1L) "" else "s")))
      }
      app_log_info("reporting", "EDGE Cost Events request completed", list(user_id = auth_state$user_id, rows = nrow(result)))
    }, ignoreInit = TRUE)

    output$cost_events_table <- renderReactable({
      data <- report_data()
      req(!is.null(data))

      reactable(
        data,
        rownames = FALSE,
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        pagination = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100),
        defaultColDef = colDef(minWidth = 130),
        columns = list(
          event_type = colDef(name = "Event type", sticky = "left", minWidth = 125),
          edgeProjectId = colDef(name = "EDGE project ID"),
          localProjectReference = colDef(name = "Local project reference"),
          projectTitle = colDef(name = "Project title", minWidth = 220),
          edgeProjectSiteId = colDef(name = "EDGE project site ID"),
          analysisCode = colDef(name = "Analysis code"),
          costCategory = colDef(name = "Cost category"),
          cost = colDef(name = "Cost", format = colFormat(currency = "GBP"), align = "right"),
          date = colDef(name = "Date"),
          invoiceNumber = colDef(name = "Invoice number"),
          invoiced = colDef(name = "Invoiced")
        )
      )
    })
  })
}
