supportUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "rids-page",
    div(
      class = "rids-page-header",
      div(div(class = "rids-page-eyebrow", "Help centre"), h1("Support"), p("Report an issue, suggest an improvement or follow existing work.")),
      div(class = "rids-page-mark", icon("life-ring"))
    ),
      
    fluidRow(
        class = "rids-support-grid",
        column(
          width = 6,
          bs4Card(
            title       = tagList(icon("paper-plane"), " Report an issue"),
            width       = 12,
            status      = "primary",
            solidHeader = FALSE,
            collapsible = FALSE,
            
            p(
              class = "rids-support-copy",
              "Found a bug or want to suggest a feature? Submit feedback through ",
              "the RIDS Feedback Portal. Submissions go directly to the development backlog."
            ),
            
            tags$a(
              href   = "https://forms.cloud.microsoft/e/YveH5gjfuy",
              target = "_blank",
              rel    = "noopener noreferrer",
              class  = "btn btn-primary",
              tagList(icon("paper-plane"), " Open feedback portal")
            )
          )
        ),
        
        column(
          width = 6,
          bs4Card(
            title       = tagList(icon("list-check"), " Track issues"),
            width       = 12,
            status      = "primary",
            solidHeader = FALSE,
            collapsible = FALSE,
            
            p(
              class = "rids-support-copy",
              "Check the status of reported issues, planned features, and ongoing work ",
              "in the shared task tracker."
            ),
            
            tags$a(
              href   = "https://docs.google.com/spreadsheets/d/1GNQ3iY5adVOfo8VzicH73PRDOPTBf2uTbxM0Aka-b9s/edit",
              target = "_blank",
              rel    = "noopener noreferrer",
              class  = "btn btn-primary",
              tagList(icon("list-check"), " Open task tracker")
            )
          )
        )
    )
  )
}

supportServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
    
    # No server-side logic needed — this is a static content page.
    # Module exists for symmetry with other modules and future expansion
    # (e.g. logging when users open the feedback portal, in-app FAQs).
    
  })
}
