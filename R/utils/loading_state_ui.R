build_loading_state_card <- function(title,
                                     subtitle = "This may take a moment...",
                                     status = c("loading", "success")) {
  status <- match.arg(status)
  icon_node <- switch(
    status,
    loading = div(class = "green-ring"),
    success = tags$span(class = "loading-state-check", HTML("&check;"))
  )

  div(
    class = "loading-state-card",
    div(
      class = "loading-state-icon-shell",
      icon_node
    ),
    div(class = "loading-state-title", title),
    div(class = "loading-state-subtitle", subtitle)
  )
}

build_loading_state_overlay <- function(title,
                                        subtitle = "This may take a moment...",
                                        status = c("loading", "success")) {
  status <- match.arg(status)

  div(
    class = "loading-state-overlay",
    build_loading_state_card(
      title = title,
      subtitle = subtitle,
      status = status
    )
  )
}
