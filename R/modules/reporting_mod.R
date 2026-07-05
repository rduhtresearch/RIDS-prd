reportingUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(
        title = "Reporting",
        width = 12,
        status = "primary",
        solidHeader = FALSE,
        div(
          style = "padding: 1rem 0; color: #697786; font-size: 0.95rem;",
          "This page will be available soon."
        )
      )
    )
  )
}

reportingServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
  })
}
