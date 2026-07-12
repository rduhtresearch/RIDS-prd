amendment_template_suffix <- function(version_type, effective_from_date) {
  if (!is_amendment_workflow(version_type)) return("")

  parsed_date <- tryCatch(
    as.Date(effective_from_date),
    error = function(e) as.Date(NA)
  )
  if (length(parsed_date) != 1L || is.na(parsed_date)) {
    stop("Amendment EDGE template names require a valid effective-from date.")
  }

  paste0(
    " [",
    toupper(amendment_workflow_label(version_type)),
    " - ",
    format(parsed_date, "%d %b %Y"),
    "]"
  )
}

suffix_amendment_template_names <- function(templates, version_type,
                                             effective_from_date) {
  suffix <- amendment_template_suffix(version_type, effective_from_date)
  if (!nzchar(suffix)) return(templates)

  if (!is.list(templates) || is.null(names(templates)) || any(!nzchar(names(templates)))) {
    stop("Amendment EDGE templates must be a named list.")
  }

  append_once <- function(values) {
    values <- as.character(values)
    eligible <- !is.na(values) & nzchar(trimws(values))
    already_suffixed <- rep(FALSE, length(values))
    already_suffixed[eligible] <- endsWith(values[eligible], suffix)
    values[eligible & !already_suffixed] <- paste0(values[eligible & !already_suffixed], suffix)
    values
  }

  templates <- lapply(templates, function(template) {
    if (is.data.frame(template) && "Template Name" %in% names(template)) {
      template[["Template Name"]] <- append_once(template[["Template Name"]])
    }
    template
  })
  names(templates) <- append_once(names(templates))
  templates
}

qualify_amendment_analysis_codes <- function(templates, version_type, version_number) {
  if (!is_amendment_workflow(version_type)) return(templates)

  if (length(version_number) != 1L || is.null(version_number) || is.na(version_number) ||
      !grepl("^[1-9][0-9]*$", as.character(version_number))) {
    stop("Amendment EDGE analysis codes require a valid template version number.")
  }
  prefix <- paste0("V", as.integer(version_number), "-")

  lapply(templates, function(template) {
    if (!is.data.frame(template) || !"Analysis Code" %in% names(template)) {
      stop("Amendment EDGE templates require an Analysis Code column.")
    }

    codes <- as.character(template[["Analysis Code"]])
    eligible <- !is.na(codes) & nzchar(trimws(codes))
    already_qualified <- rep(FALSE, length(codes))
    already_qualified[eligible] <- startsWith(codes[eligible], prefix)
    codes[eligible & !already_qualified] <- paste0(prefix, codes[eligible & !already_qualified])
    template[["Analysis Code"]] <- codes
    template
  }) |> stats::setNames(names(templates))
}

parse_versioned_edge_analysis_code <- function(analysis_code) {
  values <- as.character(analysis_code)
  matched <- !is.na(values) & grepl("^V[1-9][0-9]*-.+$", values)
  version_number <- rep(NA_integer_, length(values))
  edge_key <- values

  version_number[matched] <- as.integer(sub("^V([1-9][0-9]*)-.*$", "\\1", values[matched]))
  edge_key[matched] <- sub("^V[1-9][0-9]*-", "", values[matched])

  data.frame(
    version_number = version_number,
    edge_key = edge_key,
    stringsAsFactors = FALSE
  )
}

edge_template_export_stem <- function(template_name, version_type) {
  if (length(template_name) != 1L || is.null(template_name) || is.na(template_name) ||
      !nzchar(trimws(as.character(template_name)))) {
    stop("EDGE template export requires a non-empty template name.")
  }

  template_name <- as.character(template_name)
  if (!is_amendment_workflow(version_type)) {
    return(gsub("[^A-Za-z0-9_-]", "_", template_name))
  }

  # Preserve the readable amendment suffix while remaining safe on Windows and Unix.
  safe_name <- gsub("[[:cntrl:]]", "_", template_name)
  safe_name <- gsub('[<>:"/\\\\|?*]', "_", safe_name, perl = TRUE)
  safe_name <- sub("[. ]+$", "", safe_name)
  if (!nzchar(safe_name)) stop("EDGE template name is empty after filename sanitisation.")
  safe_name
}
