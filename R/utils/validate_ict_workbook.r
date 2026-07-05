suppressPackageStartupMessages({
  library(openxlsx)
})

validate_ict_workbook <- function(file_path) {
  
  .TOTAL_HEADERS <- c("Total", "Total Activity Cost", "Total Cost")
  
  sheets   <- openxlsx::getSheetNames(file_path)
  findings <- character()
  
  for (sheet in sheets) {
    df <- tryCatch(
      openxlsx::read.xlsx(file_path, sheet = sheet, colNames = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) next
    
    header_row <- NULL
    for (i in seq_len(min(nrow(df), 30))) {
      row_vals <- trimws(as.character(unlist(df[i, ])))
      if ("Activity" %in% row_vals && any(.TOTAL_HEADERS %in% row_vals)) {
        header_row <- i
        break
      }
    }
    if (is.null(header_row)) next
    
    header_vals <- trimws(as.character(unlist(df[header_row, ])))
    tac_idx     <- max(which(header_vals %in% .TOTAL_HEADERS))   # <-- rightmost
    
    if (ncol(df) <= tac_idx) next
    
    activity_col <- trimws(as.character(df[[1]]))
    data_rows    <- (header_row + 1):nrow(df)
    has_activity <- !is.na(activity_col[data_rows]) & nzchar(activity_col[data_rows])
    
    if (!any(has_activity)) next
    
    last_data_row <- data_rows[max(which(has_activity))]
    
    extra_block <- df[(header_row + 1):last_data_row,
                      (tac_idx + 1):ncol(df), drop = FALSE]
    
    is_populated <- !is.na(extra_block) &
      nzchar(trimws(as.character(as.matrix(extra_block))))
    
    if (any(is_populated)) {
      n_cells <- sum(is_populated)
      findings <- c(
        findings,
        sprintf(
          "Sheet '%s': %d populated cell%s found to the right of the total column (column %s) within the activity table",
          sheet,
          n_cells,
          if (n_cells == 1) "" else "s",
          openxlsx::int2col(tac_idx)
        )
      )
    }
  }
  
  list(
    valid    = length(findings) == 0,
    findings = findings
  )
}