app_run_log_state <- new.env(parent = emptyenv())

format_log_context <- function(context = list()) {
  if (is.null(context) || length(context) == 0) {
    return("")
  }

  parts <- vapply(names(context), function(name) {
    value <- context[[name]]

    if (is.null(value) || length(value) == 0 || all(is.na(value))) {
      return(NA_character_)
    }

    value <- paste(as.character(value), collapse = ",")
    paste0(name, "=", value)
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)

  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) {
    return("")
  }

  paste(parts, collapse = " ")
}

app_log_line <- function(level = "INFO", area = "app", text, context = list()) {
  area <- trimws(as.character(area %||% "app"))
  text <- trimws(as.character(text %||% ""))
  context_text <- format_log_context(context)
  line <- paste0(area, ": ", text)

  if (nzchar(context_text)) {
    line <- paste0(line, " | ", context_text)
  }

  cat(line, "\n", sep = "", file = stdout())
  invisible(line)
}

app_log_info <- function(area, text, context = list()) {
  app_log_line("INFO", area, text, context)
}

app_log_warn <- function(area, text, context = list()) {
  app_log_line("WARN", area, text, context)
}

app_log_error <- function(area, text, context = list()) {
  app_log_line("ERROR", area, text, context)
}

app_log_exception <- function(area, text, error, context = list(), level = "ERROR") {
  app_log_line(level, area, text, c(context, list(error = conditionMessage(error))))
}

initialize_app_run_logging <- function(log_dir = file.path(getwd(), "logs")) {
  tryCatch({
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    prune_app_run_log_files(log_dir = log_dir, retention_hours = 24L)

    log_path <- file.path(
      log_dir,
      sprintf(
        "rids_run_%s_pid%s.txt",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        Sys.getpid()
      )
    )

    con <- file(log_path, open = "at")

    app_run_log_state$log_dir <- log_dir
    app_run_log_state$log_path <- log_path
    app_run_log_state$output_connection <- con
    app_run_log_state$message_connection <- con

    sink(con, split = TRUE)
    app_log_info("startup", "App run started")

    invisible(log_path)
  }, error = function(e) {
    message("initialize_app_run_logging error: ", conditionMessage(e))
    invisible(NULL)
  })
}

close_app_run_logging <- function() {
  tryCatch({
    con <- app_run_log_state$output_connection %||% NULL

    if (!is.null(con) && isOpen(con)) {
      app_log_info("shutdown", "App run ended")
    }

    if (sink.number() > 0) {
      sink()
    }

    if (!is.null(con) && isOpen(con)) {
      writeLines("", con = con, useBytes = TRUE)
      close(con)
    }

    rm(list = ls(app_run_log_state, all.names = TRUE), envir = app_run_log_state)
    invisible(TRUE)
  }, error = function(e) {
    message("close_app_run_logging error: ", conditionMessage(e))
    invisible(FALSE)
  })
}

list_app_run_log_files <- function(log_dir = NULL) {
  log_dir <- log_dir %||% app_run_log_state$log_dir %||% file.path(getwd(), "logs")

  if (!dir.exists(log_dir)) {
    return(data.frame())
  }

  files <- list.files(log_dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(files) == 0) {
    return(data.frame())
  }

  info <- file.info(files)
  logs <- data.frame(
    file_name = basename(files),
    modified_at = format(info$mtime, "%Y-%m-%d %H:%M:%S"),
    size_kb = round(info$size / 1024, 1),
    file_path = normalizePath(files, winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE
  )

  logs[order(info$mtime, decreasing = TRUE), , drop = FALSE]
}

prune_app_run_log_files <- function(log_dir = NULL, retention_hours = 24L) {
  log_dir <- log_dir %||% app_run_log_state$log_dir %||% file.path(getwd(), "logs")
  retention_hours <- suppressWarnings(as.numeric(retention_hours))

  if (!dir.exists(log_dir) || is.na(retention_hours) || retention_hours <= 0) {
    return(invisible(character()))
  }

  files <- list.files(log_dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(files) == 0) {
    return(invisible(character()))
  }

  info <- file.info(files)
  cutoff_time <- Sys.time() - retention_hours * 60 * 60
  stale_files <- rownames(info)[!is.na(info$mtime) & info$mtime < cutoff_time]

  if (length(stale_files) == 0) {
    return(invisible(character()))
  }

  removed <- stale_files[file.remove(stale_files)]
  invisible(removed)
}

normalize_log_level <- function(level) {
  level <- toupper(trimws(as.character(level %||% "INFO")))

  if (!level %in% c("INFO", "WARN", "ERROR")) {
    return("INFO")
  }

  level
}

current_storage_mode <- function() {
  mode <- tryCatch(APP_CONFIG$storage_mode, error = function(e) NULL)
  mode <- tolower(trimws(as.character(mode %||% "duckdb")))

  if (!nzchar(mode)) {
    mode <- "duckdb"
  }

  mode
}

sanitize_log_value <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(NA_character_)
  }

  sanitize_text_value(as.character(x[[1]]))
}

safe_details_json <- function(details = list()) {
  tryCatch({
    if (is.null(details)) {
      details <- list()
    }

    jsonlite::toJSON(
      details,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      POSIXt = "ISO8601",
      digits = NA
    )
  }, error = function(e) {
    jsonlite::toJSON(
      list(details_encoding_error = sanitize_text_value(conditionMessage(e))),
      auto_unbox = TRUE,
      null = "null"
    )
  })
}

log_event <- function(level,
                      area,
                      message,
                      user_id = NULL,
                      username = NULL,
                      cpms_id = NULL,
                      upload_id = NULL,
                      session_id = NULL,
                      details = list()) {
  invisible(FALSE)
}

get_app_setting_value <- function(key, default = NULL) {
  tryCatch({
    row <- DBI::dbGetQuery(
      CON,
      "SELECT value FROM app_settings WHERE key = ?",
      params = list(key)
    )

    if (nrow(row) == 0) {
      return(default)
    }

    row$value[[1]]
  }, error = function(e) {
    message("get_app_setting_value error: ", conditionMessage(e))
    default
  })
}

set_app_setting_value <- function(key, value) {
  tryCatch({
    existing <- DBI::dbGetQuery(
      CON,
      "SELECT COUNT(*) AS n FROM app_settings WHERE key = ?",
      params = list(key)
    )$n[[1]]

    if (existing > 0) {
      DBI::dbExecute(
        CON,
        "UPDATE app_settings SET value = ? WHERE key = ?",
        params = list(as.character(value), key)
      )
    } else {
      DBI::dbExecute(
        CON,
        "INSERT INTO app_settings (key, value) VALUES (?, ?)",
        params = list(key, as.character(value))
      )
    }

    invisible(TRUE)
  }, error = function(e) {
    message("set_app_setting_value error: ", conditionMessage(e))
    invisible(FALSE)
  })
}

get_log_retention_days <- function(default = 90L) {
  raw_value <- suppressWarnings(as.integer(get_app_setting_value("log_retention_days", default)))

  if (is.na(raw_value) || raw_value < 1) {
    return(as.integer(default))
  }

  as.integer(raw_value)
}

query_app_logs <- function(date_from = NULL,
                           date_to = NULL,
                           level = NULL,
                           username = NULL,
                           area = NULL,
                           cpms_id = NULL,
                           upload_id = NULL,
                           limit = 1000L) {
  where <- character()
  params <- list()

  if (!is.null(date_from) && !is.na(date_from) && nzchar(as.character(date_from))) {
    where <- c(where, "timestamp >= ?")
    params <- c(params, list(paste0(as.character(date_from), " 00:00:00")))
  }

  if (!is.null(date_to) && !is.na(date_to) && nzchar(as.character(date_to))) {
    where <- c(where, "timestamp < ?")
    params <- c(params, list(paste0(as.character(as.Date(date_to) + 1), " 00:00:00")))
  }

  if (!is.null(level) && nzchar(level) && !identical(level, "ALL")) {
    where <- c(where, "level = ?")
    params <- c(params, list(normalize_log_level(level)))
  }

  if (!is.null(username) && nzchar(trimws(username))) {
    where <- c(where, "lower(username) LIKE ?")
    params <- c(params, list(paste0("%", tolower(trimws(username)), "%")))
  }

  if (!is.null(area) && nzchar(trimws(area)) && !identical(area, "ALL")) {
    where <- c(where, "area = ?")
    params <- c(params, list(trimws(area)))
  }

  if (!is.null(cpms_id) && nzchar(trimws(cpms_id))) {
    where <- c(where, "cpms_id = ?")
    params <- c(params, list(trimws(cpms_id)))
  }

  if (!is.null(upload_id) && nzchar(trimws(upload_id))) {
    where <- c(where, "upload_id = ?")
    params <- c(params, list(trimws(upload_id)))
  }

  sql <- if (!is.null(limit) &&
             is.finite(limit) &&
             limit > 0 &&
             identical(current_storage_mode(), "sqlserver")) {
    paste(
      "SELECT TOP (", as.integer(limit), ")",
      "log_id, timestamp, level, area, message, user_id, username, session_id,",
      "cpms_id, upload_id, details_json, app_version",
      "FROM app_logs"
    )
  } else {
    paste(
      "SELECT log_id, timestamp, level, area, message, user_id, username, session_id,",
      "cpms_id, upload_id, details_json, app_version",
      "FROM app_logs"
    )
  }

  if (length(where) > 0) {
    sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  }

  sql <- paste(sql, "ORDER BY timestamp DESC, log_id DESC")

  if (!is.null(limit) &&
      is.finite(limit) &&
      limit > 0 &&
      !identical(current_storage_mode(), "sqlserver")) {
    sql <- paste(sql, "LIMIT", as.integer(limit))
  }

  tryCatch({
    DBI::dbGetQuery(CON, sql, params = params)
  }, error = function(e) {
    message("query_app_logs error: ", conditionMessage(e))
    data.frame()
  })
}

get_recent_app_logs <- function(limit = 1000L) {
  query_app_logs(limit = limit)
}

list_app_log_filter_values <- function(column) {
  allowed <- c("level", "area", "username")
  if (!column %in% allowed) {
    return(character())
  }

  sql <- paste0(
    "SELECT DISTINCT ", column, " FROM app_logs ",
    "WHERE ", column, " IS NOT NULL AND trim(", column, ") <> '' ",
    "ORDER BY ", column
  )

  tryCatch({
    vals <- DBI::dbGetQuery(CON, sql)[[1]]
    vals[!is.na(vals) & nzchar(vals)]
  }, error = function(e) {
    message("list_app_log_filter_values error: ", conditionMessage(e))
    character()
  })
}

prune_app_logs <- function(retention_days = get_log_retention_days()) {
  retention_days <- suppressWarnings(as.integer(retention_days))
  if (is.na(retention_days) || retention_days < 1) {
    stop("Retention days must be a positive integer.")
  }

  cutoff <- format(Sys.time() - retention_days * 24 * 60 * 60, "%Y-%m-%d %H:%M:%S")

  tryCatch({
    deleted <- DBI::dbExecute(
      CON,
      "DELETE FROM app_logs WHERE timestamp < ?",
      params = list(cutoff)
    )

    invisible(as.integer(deleted))
  }, error = function(e) {
    message("prune_app_logs error: ", conditionMessage(e))
    stop(e)
  })
}
