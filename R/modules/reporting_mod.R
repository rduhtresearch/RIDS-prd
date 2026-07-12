reportingUI <- function(id) {
  ns <- NS(id)
  div(
    class = "rids-page",
    div(class = "rids-page-header", div(div(class = "rids-page-eyebrow", "Insights"), h1("Reporting"), p("Monitor study income and operational performance.")), div(class = "rids-page-mark", icon("chart-line"))),
    div(
      class = "rids-empty-state rids-surface",
      div(class = "rids-empty-icon", icon("chart-bar")),
      h2("Reporting is being prepared"),
      p("This workspace will bring study and income reporting into one consistent view."),
      span(class = "rids-status-chip", "Coming soon")
    )
  )
}

reportingServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
  })
}
