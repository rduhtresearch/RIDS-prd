is_amendment_workflow <- function(version_type) {
  length(version_type) == 1L &&
    !is.null(version_type) &&
    !is.na(version_type) &&
    version_type %in% c("substantial_amendment", "distribution_amendment")
}

amendment_workflow_label <- function(version_type) {
  switch(
    as.character(version_type),
    substantial_amendment = "Substantial amendment",
    distribution_amendment = "Distribution amendment",
    "Amendment"
  )
}

completed_template_versions <- function(versions) {
  if (is.null(versions) || nrow(versions) == 0L) return(versions)
  versions[versions$status %in% c("active", "archived"), , drop = FALSE]
}

template_version_display_label <- function(version) {
  version_type <- as.character(version$version_type[[1]])
  version_number <- as.integer(version$version_number[[1]])
  effective_date <- as.Date(version$effective_from_date[[1]])

  type_label <- switch(
    version_type,
    baseline = "Original template",
    substantial_amendment = paste(
      "SUBSTANTIAL AMENDMENT",
      format(effective_date, "%d %b %Y"),
      sep = " - "
    ),
    distribution_amendment = paste(
      "DISTRIBUTION AMENDMENT",
      format(effective_date, "%d %b %Y"),
      sep = " - "
    ),
    "Template"
  )

  label <- paste("Version", version_number, "-", type_label)
  if (identical(as.character(version$status[[1]]), "archived")) {
    label <- paste(label, "- ARCHIVED")
  }
  label
}

template_version_choices <- function(versions) {
  versions <- completed_template_versions(versions)
  if (is.null(versions) || nrow(versions) == 0L) return(character())

  labels <- vapply(
    seq_len(nrow(versions)),
    function(i) template_version_display_label(versions[i, , drop = FALSE]),
    character(1)
  )
  stats::setNames(as.character(versions$version_id), labels)
}

default_template_version_id <- function(versions, resolved_version = NULL) {
  versions <- completed_template_versions(versions)
  if (is.null(versions) || nrow(versions) == 0L) return(NULL)

  if (!is.null(resolved_version) && nrow(resolved_version) > 0L) {
    resolved_id <- as.character(resolved_version$version_id[[1]])
    if (resolved_id %in% as.character(versions$version_id)) return(resolved_id)
  }

  as.character(versions$version_id[[which.max(versions$version_number)]])
}

template_version_filename_token <- function(version) {
  version_type <- as.character(version$version_type[[1]])
  version_number <- as.integer(version$version_number[[1]])
  type_token <- switch(
    version_type,
    baseline = "original",
    substantial_amendment = "substantial_amendment",
    distribution_amendment = "distribution_amendment",
    "template"
  )
  effective_date <- as.Date(version$effective_from_date[[1]])
  date_token <- if (is.na(effective_date)) "" else paste0("_", format(effective_date, "%Y%m%d"))

  paste0("v", version_number, "_", type_token, date_token)
}

amendment_workflow_banner <- function(version_type, effective_from_date,
                                      study_name = NULL, cpms_id = NULL) {
  if (!is_amendment_workflow(version_type)) return(NULL)

  scalar_text <- function(value) {
    if (is.null(value) || length(value) == 0L || is.na(value[[1]])) return("")
    trimws(as.character(value[[1]]))
  }

  parsed_date <- tryCatch(
    as.Date(effective_from_date),
    error = function(e) as.Date(NA)
  )
  effective_label <- if (length(parsed_date) == 1L && !is.na(parsed_date)) {
    format(parsed_date, "%d %b %Y")
  } else {
    "Date not available"
  }

  study_label <- scalar_text(study_name)
  cpms_label <- scalar_text(cpms_id)
  context_label <- if (nzchar(study_label) && nzchar(cpms_label)) {
    paste0(study_label, " · CPMS ", cpms_label)
  } else if (nzchar(study_label)) {
    study_label
  } else if (nzchar(cpms_label)) {
    paste("CPMS", cpms_label)
  } else {
    ""
  }

  type_label <- amendment_workflow_label(version_type)

  shiny::div(
    class = "amendment-workflow-banner",
    role = "note",
    `aria-label` = paste(type_label, "workflow, effective from", effective_label),
    shiny::span(
      class = "amendment-workflow-icon",
      shiny::icon("edit")
    ),
    shiny::strong(class = "amendment-workflow-title", type_label),
    shiny::span(class = "amendment-workflow-separator", "·"),
    shiny::span(class = "amendment-workflow-detail", paste("Effective from", effective_label)),
    if (nzchar(context_label)) shiny::span(class = "amendment-workflow-separator", "·"),
    if (nzchar(context_label)) shiny::span(class = "amendment-workflow-study", context_label)
  )
}

render_amendment_workflow_banner <- function(shared_state) {
  shiny::renderUI({
    amendment_workflow_banner(
      version_type = shared_state$template_version_type,
      effective_from_date = shared_state$template_version_effective_date,
      study_name = shared_state$study_name,
      cpms_id = shared_state$cpms_id
    )
  })
}
