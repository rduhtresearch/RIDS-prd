studyWorkspaceUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = paste(
        "display: flex;",
        "justify-content: space-between;",
        "align-items: center;",
        "padding: 0.75rem 1rem;",
        "border-bottom: 1px solid #eee;",
        "margin-bottom: 1rem;"
      ),
      actionLink(
        inputId = ns("back_to_library"),
        label   = tagList(icon("arrow-left"), " Back to library"),
        style   = "font-size: 0.9rem; color: #1f5f8b; text-decoration: none;"
      ),
      div(
        style = "font-size: 1.05rem; font-weight: 600; color: #1d2a36;",
        textOutput(ns("workspace_title"), inline = TRUE)
      )
    ),
    
    uiOutput(ns("body"))
  )
}

studyWorkspaceServer <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    active_study <- reactive({
      req(shared_state$current_study)
      shared_state$current_study
    })
    
    # ── Helper: render a value, falling back to em-dash if NA/empty ──────────
    fmt_value <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(as.character(x))) {
        return(tags$span(style = "color: #aaa;", "—"))
      }
      as.character(x)
    }
    
    # ── Look up the active study's metadata ──────────────────────────────────
    study_meta <- reactive({
      ref <- active_study()
      
      df <- rids_repos()$studies$find_meta(
        as.character(ref$cpms_id),
        as.character(ref$study_site),
        as.character(ref$scenario_id)
      )
      
      if (nrow(df) == 0) return(NULL)
      df[1, ]
    })
    
    # ── Posting lines row count for active study ─────────────────────────────
    posting_count <- reactive({
      ref <- active_study()
      
      rids_repos()$posting_lines$count_for_run(
        as.character(ref$cpms_id),
        as.character(ref$study_site),
        as.character(ref$scenario_id)
      )
    })
    
    # ── Header title ─────────────────────────────────────────────────────────
    output$workspace_title <- renderText({
      if (is.null(shared_state$current_study)) return("No study selected")
      
      meta <- study_meta()
      if (is.null(meta)) {
        ref <- active_study()
        return(paste0("CPMS ", ref$cpms_id, " · ", ref$study_site, " · Scenario ", ref$scenario_id))
      }
      
      if (is.na(meta$study_name) || !nzchar(meta$study_name)) {
        paste0("CPMS ", meta$cpms_id, " · ", meta$study_site, " · Scenario ", meta$scenario_id)
      } else {
        paste0(meta$study_name, " · CPMS ", meta$cpms_id, " · ", meta$study_site)
      }
    })
    
    # ── Body: tabs ───────────────────────────────────────────────────────────
    output$body <- renderUI({
      if (is.null(shared_state$current_study)) {
        return(p(
          style = "color: #697786; padding: 1rem;",
          "No study selected. Open one from the library."
        ))
      }
      
      meta <- study_meta()
      if (is.null(meta)) {
        ref <- active_study()
        return(p(
          style = "color: #c0392b; padding: 1rem;",
          paste0(
            "Study not found: CPMS ",
            ref$cpms_id,
            " / ",
            ref$study_site,
            " / Scenario ",
            ref$scenario_id
          )
        ))
      }
      
      # ── Overview tab content ─────────────────────────────────────────────
      uploaded_at <- tryCatch(
        format(as.POSIXct(meta$upload_timestamp), "%d %b %Y %H:%M"),
        error = function(e) "—"
      )
      uploader_blob <- if (is.na(meta$uploaded_by) || !nzchar(meta$uploaded_by)) {
        uploaded_at
      } else {
        paste0(meta$uploaded_by, " · ", uploaded_at)
      }
      
      kv <- function(label, value) {
        div(
          style = "display: grid; grid-template-columns: 180px 1fr; gap: 1rem; padding: 0.45rem 0;",
          div(
            style = "font-size: 0.82rem; color: #697786; font-weight: 500;",
            label
          ),
          div(
            style = "font-size: 0.92rem; color: #1d2a36;",
            value
          )
        )
      }
      
      overview_panel <- div(
        style = "padding: 1rem;",
        kv("CPMS ID",        fmt_value(meta$cpms_id)),
        kv("Study Site",     fmt_value(meta$study_site)),
        kv("EDGE ID",        fmt_value(meta$edge_id)),
        kv("Scenario",       fmt_value(meta$scenario_id)),
        kv("Speciality",     fmt_value(meta$speciality_name)),
        kv("Original file",  fmt_value(meta$original_filename)),
        kv("Uploaded by",    uploader_blob),
        
        hr(style = "margin: 1rem 0;"),
        
        div(
          style = "font-size: 0.82rem; color: #697786; font-weight: 500; margin-bottom: 0.4rem;",
          "Notes"
        ),
        div(
          style = "font-size: 0.9rem; color: #1d2a36; line-height: 1.5;",
          if (is.na(meta$notes) || !nzchar(meta$notes)) {
            tags$span(style = "color: #aaa; font-style: italic;", "No notes")
          } else {
            meta$notes
          }
        )
      )
      
      # ── Posting lines tab content ────────────────────────────────────────
      n_posting <- posting_count()
      
      posting_panel <- div(
        style = "padding: 1rem;",
        if (n_posting == 0) {
          p(
            style = "color: #aaa; font-style: italic;",
            "No posting lines yet for this study."
          )
        } else {
          tagList(
            p(
              style = "color: #697786; margin-bottom: 1rem;",
              "Download the posting lines for this study as a CSV file."
            ),
            div(
              style = "font-size: 0.82rem; color: #697786; margin-bottom: 1rem;",
              format(n_posting, big.mark = ","), " posting lines"
            ),
            downloadButton(
              ns("download_posting_lines"),
              label = "Download posting lines (CSV)",
              class = "btn-primary"
            )
          )
        }
      )
      
      # ── EDGE templates tab content ───────────────────────────────────────
      edge_zip_exists <- !is.null(meta$edge_zip_path) &&
        !is.na(meta$edge_zip_path) &&
        nzchar(meta$edge_zip_path) &&
        file.exists(meta$edge_zip_path)
      
      edge_panel <- div(
        style = "padding: 1rem;",
        if (edge_zip_exists) {
          tagList(
            p(
              style = "color: #697786; margin-bottom: 1rem;",
              "Download the EDGE templates that were generated for this study."
            ),
            div(
              style = "font-size: 0.82rem; color: #697786; margin-bottom: 1rem;",
              "Generated at upload time — represents the original processing run."
            ),
            downloadButton(
              ns("download_edge_zip"),
              label = "Download EDGE templates (ZIP)",
              class = "btn-primary"
            )
          )
        } else {
          p(
            style = "color: #aaa; font-style: italic;",
            "No EDGE templates ZIP found for this study. The file may have been deleted or never generated."
          )
        }
      )
      
      # ── Files tab content ────────────────────────────────────────────────
      file_row <- function(label, path, dl_id, copy_id) {
        exists <- !is.null(path) && !is.na(path) && nzchar(path) && file.exists(path)
        
        div(
          style = "padding: 0.75rem 0; border-bottom: 1px solid #f0f4f8;",
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.4rem;",
            div(
              style = "font-size: 0.92rem; font-weight: 600; color: #1d2a36;",
              label
            ),
            if (exists) {
              div(
                style = "display: flex; gap: 0.5rem;",
                downloadButton(
                  ns(dl_id),
                  label = "Download",
                  class = "btn-sm btn-outline-primary"
                ),
                actionButton(
                  ns(copy_id),
                  label = tagList(icon("copy"), " Copy path"),
                  class = "btn-sm btn-outline-secondary"
                )
              )
            } else {
              tags$span(style = "color: #aaa; font-style: italic;", "File not found")
            }
          ),
          div(
            style = paste(
              "font-family: monospace;",
              "font-size: 0.78rem;",
              "color: #697786;",
              "background: #f7f9fc;",
              "padding: 0.4rem 0.6rem;",
              "border-radius: 4px;",
              "word-break: break-all;"
            ),
            if (is.null(path) || is.na(path) || !nzchar(path)) "—" else path
          )
        )
      }
      
      files_panel <- div(
        style = "padding: 1rem;",
        p(
          style = "color: #697786; margin-bottom: 1rem;",
          "Files associated with this study. Use Copy path to grab the location for use in your file explorer."
        ),
        file_row("Original ICT workbook", meta$saved_file_path, "download_ict",   "copy_ict"),
        file_row("EDGE templates (ZIP)",  meta$edge_zip_path,    "download_zip_v2", "copy_zip")
      )
      
      # ── Tab structure ────────────────────────────────────────────────────
      bs4Card(
        title       = NULL,
        width       = 12,
        status      = "primary",
        solidHeader = FALSE,
        collapsible = FALSE,
        
        tabsetPanel(
          id = ns("workspace_tabs"),
          tabPanel("Overview",        overview_panel),
          tabPanel("Posting lines",   posting_panel),
          tabPanel("EDGE templates",  edge_panel),
          tabPanel("Files",           files_panel)
        )
      )
    })
    
    # ── Posting lines download ───────────────────────────────────────────────
    output$download_posting_lines <- downloadHandler(
      filename = function() {
        req(shared_state$current_study)
        meta <- study_meta()
        req(meta)
        
        cpms <- meta$cpms_id
        site <- if (!is.na(meta$study_site) && nzchar(meta$study_site)) {
          meta$study_site
        } else {
          "site"
        }
        name <- if (!is.na(meta$study_name) && nzchar(meta$study_name)) {
          gsub("[^A-Za-z0-9_-]+", "_", meta$study_name)
        } else {
          "study"
        }
        ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0(cpms, "_", site, "_", name, "_posting_lines_", ts, ".csv")
      },
      content = function(file) {
        ref <- active_study()
        
        df <- rids_repos()$posting_lines$find_by_run(
          as.character(ref$cpms_id),
          as.character(ref$study_site),
          as.character(ref$scenario_id)
        )
        
        if (nrow(df) == 0) {
          showNotification("No posting lines found for this study", type = "warning")
        }
        
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # ── Original ICT download (Files tab) ────────────────────────────────────
    output$download_ict <- downloadHandler(
      filename = function() {
        meta <- study_meta()
        req(meta)
        if (!is.na(meta$original_filename) && nzchar(meta$original_filename)) {
          meta$original_filename
        } else {
          "ict_workbook.xlsx"
        }
      },
      content = function(file) {
        meta <- study_meta()
        req(meta, !is.na(meta$saved_file_path), file.exists(meta$saved_file_path))
        file.copy(meta$saved_file_path, file)
      }
    )
    
    # ── EDGE ZIP download (Files tab) ────────────────────────────────────────
    output$download_zip_v2 <- downloadHandler(
      filename = function() {
        meta <- study_meta()
        req(meta)
        paste0(meta$cpms_id, "_", meta$study_site, "_", meta$scenario_id, "_edge_templates.zip")
      },
      content = function(file) {
        meta <- study_meta()
        req(meta, !is.na(meta$edge_zip_path), file.exists(meta$edge_zip_path))
        file.copy(meta$edge_zip_path, file)
      }
    )
    
    # ── EDGE ZIP download (EDGE templates tab) ───────────────────────────────
    output$download_edge_zip <- downloadHandler(
      filename = function() {
        meta <- study_meta()
        req(meta)
        paste0(meta$cpms_id, "_", meta$study_site, "_", meta$scenario_id, "_edge_templates.zip")
      },
      content = function(file) {
        meta <- study_meta()
        req(meta, !is.na(meta$edge_zip_path), file.exists(meta$edge_zip_path))
        file.copy(meta$edge_zip_path, file)
      }
    )
    
    # ── Copy ICT path to clipboard ───────────────────────────────────────────
    observeEvent(input$copy_ict, {
      meta <- study_meta()
      req(meta, !is.na(meta$saved_file_path))
      
      shinyjs::runjs(sprintf(
        "navigator.clipboard.writeText('%s');",
        gsub("\\\\", "\\\\\\\\", meta$saved_file_path)
      ))
      showNotification("Path copied to clipboard", type = "message", duration = 2)
    })
    
    # ── Copy EDGE ZIP path to clipboard ──────────────────────────────────────
    observeEvent(input$copy_zip, {
      meta <- study_meta()
      req(meta, !is.na(meta$edge_zip_path))
      
      shinyjs::runjs(sprintf(
        "navigator.clipboard.writeText('%s');",
        gsub("\\\\", "\\\\\\\\", meta$edge_zip_path)
      ))
      showNotification("Path copied to clipboard", type = "message", duration = 2)
    })
    
    # ── Back button ──────────────────────────────────────────────────────────
    observeEvent(input$back_to_library, {
      shared_state$current_study <- NULL
      shinyjs::runjs('$("a[data-value=\'tab_library\']").trigger("click")')
    })
    
  })
}
