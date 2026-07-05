helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    # ── Floating button ───────────────────────────────────────────────────────
    div(
      style = "position: fixed; bottom: 2rem; right: 2rem; z-index: 1000;",
      actionButton(
        ns("toggle"),
        label    = "?",
        class    = "btn-primary",
        style    = "width: 3rem; height: 3rem; border-radius: 50%; font-weight: 700; font-size: 1.1rem; box-shadow: 0 4px 12px rgba(31, 95, 139, 0.3); padding: 0;"
      )
    ),
    
    # ── Slide-in panel ────────────────────────────────────────────────────────
    shinyjs::hidden(
      div(
        id    = ns("panel"),
        style = "position: fixed; top: 0; right: 0; width: 360px; height: 100vh; background: #ffffff; box-shadow: -4px 0 24px rgba(18, 34, 48, 0.12); z-index: 9998; display: flex; flex-direction: column;",
        div(
          style = "display: flex; align-items: center; justify-content: space-between; padding: 1.25rem 1.5rem; border-bottom: 1px solid #f0f4f8;",
          span(style = "font-weight: 700; font-size: 1rem; color: #1d2a36;", "Help"),
          actionButton(ns("close"), "✕", class = "btn btn-sm", style = "background: none; border: none; font-size: 1rem; color: #6c757d; padding: 0;")
        ),
        div(
          id    = ns("content"),
          style = "flex: 1; overflow-y: auto; padding: 1.5rem;",
          uiOutput(ns("help_content"))
        )
      )
    )
  )
}

helpServer <- function(id, content) {
  moduleServer(id, function(input, output, session) {
    
    # ── Toggle panel ──────────────────────────────────────────────────────────
    observeEvent(input$toggle, {
      shinyjs::show(id = session$ns("panel"), asis = TRUE)
    })
    
    observeEvent(input$close, {
      shinyjs::hide(id = session$ns("panel"), asis = TRUE)
    })
    
    # ── Render content ────────────────────────────────────────────────────────
    output$help_content <- renderUI({
      tagList(
        h5(style = "font-weight: 700; color: #1d2a36; margin-bottom: 1rem;", content$title),
        lapply(content$sections, function(section) {
          div(
            style = "margin-bottom: 1.5rem;",
            p(strong(section$heading)),
            p(style = "color: #697786; font-size: 0.9rem; line-height: 1.6;", section$body)
          )
        })
      )
    })
    
  })
}