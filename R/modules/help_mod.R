helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "rids-help-launcher",
      tagAppendAttributes(
        actionButton(
          ns("toggle"),
          label = "?",
          class = "btn-primary rids-help-toggle"
        ),
        `aria-label` = "Open help panel",
        `aria-controls` = ns("panel"),
        `aria-expanded` = "false"
      )
    ),

    div(
      id = ns("panel"),
      class = "rids-help-panel",
      style = "display: none;",
      role = "dialog",
      `aria-modal` = "true",
      `aria-hidden` = "true",
      `aria-labelledby` = ns("title"),
      tabindex = "-1",
      div(
        class = "rids-help-header",
        span(id = ns("title"), class = "rids-help-title", "Help"),
        tagAppendAttributes(
          actionButton(
            ns("close"),
            "✕",
            class = "btn btn-sm rids-help-close"
          ),
          `aria-label` = "Close help panel"
        )
      ),
      div(
        id = ns("content"),
        class = "rids-help-content",
        uiOutput(ns("help_content"))
      )
    )
  )
}

helpServer <- function(id, content) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$toggle, {
      shinyjs::runjs(sprintf(
        "var panel = document.getElementById(%s); if (panel) panel.style.display = 'flex';",
        jsonlite::toJSON(session$ns("panel"), auto_unbox = TRUE)
      ))
    })
    
    observeEvent(input$close, {
      shinyjs::runjs(sprintf(
        "var panel = document.getElementById(%s); if (panel) panel.style.display = 'none';",
        jsonlite::toJSON(session$ns("panel"), auto_unbox = TRUE)
      ))
    })
    
    output$help_content <- renderUI({
      tagList(
        h5(class = "rids-help-content-title", content$title),
        lapply(content$sections, function(section) {
          div(
            class = "rids-help-section",
            p(strong(section$heading)),
            p(class = "rids-help-copy", section$body)
          )
        })
      )
    })
    outputOptions(output, "help_content", suspendWhenHidden = FALSE)
    
  })
}
