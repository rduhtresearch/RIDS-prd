progressUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("progress_bar"))
}

progressServer <- function(id, current_step) {
  moduleServer(id, function(input, output, session) {
    
    steps <- list(
      list(id = "step1", label = "Upload"),
      list(id = "step2", label = "Costs"),
      list(id = "step3", label = "Tags"),
      list(id = "step4", label = "Output")
    )
    
    output$progress_bar <- renderUI({
      current <- current_step()
      
      if (is.null(current) || !current %in% c("step1", "step2", "step3", "step4")) {
        return(NULL)
      }
      
      
      div(
        style = "display: flex; align-items: center; justify-content: center; padding: 1rem 0 0.5rem; gap: 0;",
        lapply(seq_along(steps), function(i) {
          step     <- steps[[i]]
          is_current  <- !is.null(current) && current == step$id
          is_complete <- !is.null(current) && which(sapply(steps, `[[`, "id") == current) > i
          
          circle_bg    <- if (is_complete) "#28a745" else if (is_current) "#1f5f8b" else "#dee2e6"
          circle_color <- if (is_complete || is_current) "#ffffff" else "#6c757d"
          label_color  <- if (is_current) "#1d2a36" else "#6c757d"
          label_weight <- if (is_current) "700" else "400"
          circle_content <- if (is_complete) "✓" else as.character(i)
          
          tagList(
            div(
              style = "display: flex; flex-direction: column; align-items: center; min-width: 80px;",
              div(
                style = sprintf(
                  "width: 2rem; height: 2rem; border-radius: 50%%; background: %s; color: %s; display: flex; align-items: center; justify-content: center; font-weight: 700; font-size: 0.85rem;",
                  circle_bg, circle_color
                ),
                circle_content
              ),
              div(
                style = sprintf("margin-top: 0.4rem; font-size: 0.8rem; color: %s; font-weight: %s;", label_color, label_weight),
                step$label
              )
            ),
            if (i < length(steps)) {
              div(
                style = sprintf(
                  "flex: 1; height: 2px; background: %s; margin-bottom: 1.2rem; max-width: 60px;",
                  if (is_complete) "#28a745" else "#dee2e6"
                )
              )
            }
          )
        })
      )
    })
  })
}