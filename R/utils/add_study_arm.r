add_study_arm <- function(df_long) {
  imap(df_long, function(df, sheet_nm) {
    if (is.null(df) || nrow(df) == 0) return(df)
    clean_sheet_nm <- trimws(sheet_nm)
    
    if (!("Flag" %in% names(df))) {
      warning("add_study_arm(): 'Flag' column missing in sheet '", sheet_nm, "'; Study_Arm set to sheet name.")
      df$Study_Arm <- clean_sheet_nm
      return(df)
    }
    
    df$Study_Arm <- dplyr::case_when(
      df$Flag == "Scheduled / Some Participants"     ~ "SSP",
      df$Flag == "Unscheduled / Itemised Activities" ~ "UA",
      df$Flag == "Setup & Closedown"                 ~ "SC",
      TRUE                                           ~ if ("SheetName" %in% names(df)) trimws(df$SheetName) else clean_sheet_nm
    )
    
    df
  })
}
