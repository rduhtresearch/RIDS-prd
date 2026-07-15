progressUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("progress_bar"))
}

progressServer <- function(id, current_step, shared_state) {
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
      
      
      metadata_value <- function(value) {
        if (is.null(value) || length(value) == 0) {
          return(NULL)
        }

        value <- trimws(as.character(value[[1]]))
        if (is.na(value) || !nzchar(value)) NULL else value
      }

      metadata <- list(
        Study = metadata_value(shared_state$study_name),
        Scenario = metadata_value(shared_state$scenario_id),
        `Study Site` = metadata_value(shared_state$study_site),
        `CPMS ID` = metadata_value(shared_state$cpms_id)
      )
      metadata <- metadata[!vapply(metadata, is.null, logical(1))]

      div(
        class = "rids-progress-panel",
        div(
          class = "rids-progress",
          lapply(seq_along(steps), function(i) {
            step     <- steps[[i]]
            is_current  <- !is.null(current) && current == step$id
            is_complete <- !is.null(current) && which(sapply(steps, `[[`, "id") == current) > i

            state_class <- if (is_current) {
              "is-current"
            } else if (is_complete) {
              "is-complete"
            } else {
              "is-upcoming"
            }
            circle_content <- if (is_complete) icon("check") else as.character(i)

            tagList(
              div(
                class = paste("rids-progress-step", state_class),
                div(
                  class = "rids-progress-circle",
                  circle_content
                ),
                div(
                  class = "rids-progress-label",
                  step$label
                )
              ),
              if (i < length(steps)) {
                div(
                  class = paste(
                    "rids-progress-line",
                    if (is_complete) "is-complete" else "is-upcoming"
                  )
                )
              }
            )
          })
        ),
        if (length(metadata) > 0) {
          div(
            class = "rids-progress-metadata",
            lapply(names(metadata), function(label) {
              div(
                class = "rids-progress-meta-item",
                span(class = "rids-progress-meta-label", label),
                span(class = "rids-progress-meta-value", metadata[[label]])
              )
            })
          )
        }
      )
    })
  })
}
