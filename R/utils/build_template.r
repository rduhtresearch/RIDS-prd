# suppressPackageStartupMessages({
#   library(dplyr)
#   library(stringr)
# })
# 
# build_all_edge_templates <- function(data) {
#   
#   .SPECIAL_SHEETS <- c("Unscheduled Activities", "Setup & Closedown", "Pharmacy")
# # NOTE: new variables may need to be added in the future - this function should be param
#   
#   .EDGE_COLS <- c(
#     "EDGE Project ID",
#     "Template Name",
#     "Template Level (Project | Participant | ProjectSite)",
#     "Project Arm (Participant only)",
#     "Project Site Name (ProjectSite only)",
#     "Cost Item Description",
#     "Analysis Code",
#     "Cost Category",
#     "Default Cost",
#     "Currency",
#     "Department",
#     "Overhead Cost",
#     "Time", 
#     "Activity Type"
#   )
#   
#   # NOTE: edge_key is assigned upstream by assign_edge_keys() and arrives on
#   # the posting lines data already populated.
#   if (!"edge_key" %in% names(data)) {
#     stop("build_all_edge_templates(): incoming data is missing 'edge_key'. ",
#          "Make sure assign_edge_keys() runs before this in the pipeline.")
#   }
#   
#   # ── Build templates ───────────────────────────────────────────────────────────
#   
#   .build_special <- function(df) {
#     df |>
#       summarise(
#         total = sum(adjusted_amount, na.rm = TRUE),
#         .by   = c(Study_Arm, sheet_name, Activity, row_id, 
#                   staff_group, edge_key, Department, study_name, cpms_id)
#       ) |>
#       mutate(
#         `EDGE Project ID`                                      = cpms_id,
#         `Template Name`                                        = Study_Arm,
#         `Template Level (Project | Participant | ProjectSite)` = "tbc",
#         `Project Arm (Participant only)`                       = Study_Arm,
#         `Project Site Name (ProjectSite only)`                 = NA,
#         `Cost Item Description`                                = str_replace_all(Activity, "\\.", " "),
#         `Analysis Code`                                        = edge_key,
#         `Cost Category`                                        = "Research Cost",
#         `Default Cost`                                         = total,
#         `Currency`                                             = "GBP",
#         `Department`                                           = Department,
#         `Overhead Cost`                                        = NA,
#         `Time`                                                 = NA,
#         `Activity Type`                                        = NA
#       ) |>
#       select(all_of(.EDGE_COLS))
#   }
#   
#   .build_main <- function(df) {
#     visit_keys <- df |>
#       filter(sheet_name != "Pharmacy") |>
#       distinct(Study_Arm, Visit, edge_key)
#     
#     df |>
#       summarise(
#         total = sum(adjusted_amount, na.rm = TRUE),
#         .by   = c(study_name, Visit, Study_Arm, Visit_Label)
#       ) |>
#       left_join(visit_keys, by = c("Study_Arm", "Visit")) |>
#       mutate(
#         `EDGE Project ID`                                      = NA,
#         `Template Name`                                        = Study_Arm,
#         `Template Level (Project | Participant | ProjectSite)` = "Participant",
#         `Project Arm (Participant only)`                       = NA,
#         `Project Site Name (ProjectSite only)`                 = NA,
#         `Cost Item Description`                                = paste0("VISIT - ", str_replace_all(Visit_Label, "\\.", " ")),
#         `Analysis Code`                                        = edge_key,
#         `Cost Category`                                        = "Research Cost",
#         `Default Cost`                                         = total,
#         `Currency`                                             = "GBP",
#         `Department`                                           = NA,
#         `Overhead Cost`                                        = NA,
#         `Time`                                                 = NA,
#         `Activity Type`                                        = NA
#       ) |>
#       select(all_of(.EDGE_COLS))
#   }
#   
#   # ── Dispatch and return ───────────────────────────────────────────────────────
#   
#   special_data <- data |> filter(sheet_name %in% .SPECIAL_SHEETS)
#   main_data    <- data |> filter(!sheet_name %in% .SPECIAL_SHEETS | sheet_name == "Pharmacy")
#   
#   special_list <- special_data |>
#     group_by(sheet_name) |>
#     group_map(~ .build_special(.x), .keep = TRUE) |>
#     setNames(sort(unique(special_data$sheet_name)))
#   
#   main_list <- main_data |>
#     group_by(Study_Arm) |>
#     group_map(~ .build_main(.x), .keep = TRUE) |>
#     setNames(sort(unique(main_data$Study_Arm)))
#   
#   c(special_list, main_list)
# }

# current 
# suppressPackageStartupMessages({
#   library(dplyr)
#   library(stringr)
# })
# 
# build_all_edge_templates <- function(data, visit_lookup, edge_id) {
#   
#   .SPECIAL_SHEETS <- c("Unscheduled Activities", "Setup & Closedown", "Pharmacy")
#   # NOTE: new variables may need to be added in the future - this function should be param
#   
#   .EDGE_COLS <- c(
#     "EDGE Project ID",
#     "Template Name",
#     "Template Level (Project | Participant | ProjectSite)",
#     "Project Arm (Participant only)",
#     "Project Site Name (ProjectSite only)",
#     "Cost Item Description",
#     "Analysis Code",
#     "Cost Category",
#     "Default Cost",
#     "Currency",
#     "Department",
#     "Overhead Cost",
#     "Time", 
#     "Activity Type"
#   )
#   
#   # NOTE: edge_key is assigned upstream by assign_edge_keys() and arrives on
#   # the posting lines data already populated.
#   if (!"edge_key" %in% names(data)) {
#     stop("build_all_edge_templates(): incoming data is missing 'edge_key'. ",
#          "Make sure assign_edge_keys() runs before this in the pipeline.")
#   }
#   
#   if (missing(visit_lookup) || is.null(visit_lookup) || nrow(visit_lookup) == 0) {
#     stop("build_all_edge_templates(): 'visit_lookup' is required and must contain ",
#          "Study, Study_Arm, Visit_Label, Visit_Number.")
#   }
#   
#   # ── Build templates ───────────────────────────────────────────────────────────
#   
#   .build_special <- function(df) {
#     df |>
#       summarise(
#         total = sum(adjusted_amount, na.rm = TRUE),
#         .by   = c(Study_Arm, sheet_name, Activity, row_id, 
#                   staff_group, edge_key, Department, study_name, cpms_id)
#       ) |>
#       mutate(
#         `EDGE Project ID`                                      = edge_id,
#         `Template Name`                                        = sheet_name,
#         `Template Level (Project | Participant | ProjectSite)` = NA,
#         `Project Arm (Participant only)`                       = sheet_name,
#         `Project Site Name (ProjectSite only)`                 = NA,
#         `Cost Item Description`                                = str_replace_all(Activity, "\\.", " "),
#         `Analysis Code`                                        = edge_key,
#         `Cost Category`                                        = "Research Cost",
#         `Default Cost`                                         = total,
#         `Currency`                                             = "GBP",
#         `Department`                                           = Department,
#         `Overhead Cost`                                        = NA,
#         `Time`                                                 = NA,
#         `Activity Type`                                        = NA
#       ) |>
#       select(all_of(.EDGE_COLS))
#   }
#   
#   .build_main <- function(df) {
#     visit_keys <- df |>
#       filter(sheet_name != "Pharmacy") |>
#       distinct(Study_Arm, Visit, edge_key)
#     
#     df |>
#       summarise(
#         total = sum(adjusted_amount, na.rm = TRUE),
#         .by   = c(study_name, Visit, Study_Arm)
#       ) |>
#       left_join(visit_keys, by = c("Study_Arm", "Visit")) |>
#       left_join(
#         visit_lookup |> select(Study, Study_Arm, Visit_Number, Visit_Label),
#         by = c("study_name" = "Study", "Study_Arm", "Visit" = "Visit_Number")
#       ) |>
#       arrange(Visit) |>
#       mutate(
#         `EDGE Project ID`                                      = edge_id,
#         `Template Name`                                        = Study_Arm,
#         `Template Level (Project | Participant | ProjectSite)` = NA,
#         `Project Arm (Participant only)`                       = NA,
#         `Project Site Name (ProjectSite only)`                 = NA,
#         `Cost Item Description`                                = paste0(
#           Visit, " - ", str_replace_all(Visit_Label, "\\.", " ")
#         ),
#         `Analysis Code`                                        = edge_key,
#         `Cost Category`                                        = "Research Cost",
#         `Default Cost`                                         = total,
#         `Currency`                                             = "GBP",
#         `Department`                                           = NA,
#         `Overhead Cost`                                        = NA,
#         `Time`                                                 = NA,
#         `Activity Type`                                        = NA
#       ) |>
#       select(all_of(.EDGE_COLS))
#   }
#   
#   # ── Dispatch and return ───────────────────────────────────────────────────────
#   
#   special_data <- data |> filter(sheet_name %in% .SPECIAL_SHEETS)
#   main_data    <- data |> filter(!sheet_name %in% .SPECIAL_SHEETS | sheet_name == "Pharmacy")
#   
#   special_list <- special_data |>
#     group_by(sheet_name) |>
#     group_map(~ .build_special(.x), .keep = TRUE) |>
#     setNames(sort(unique(special_data$sheet_name)))
#   
#   main_list <- main_data |>
#     group_by(Study_Arm) |>
#     group_map(~ .build_main(.x), .keep = TRUE) |>
#     setNames(sort(unique(main_data$Study_Arm)))
#   
#   c(special_list, main_list)
# }
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

resolve_visit_label <- function(visit, visit_label, visit_label_lookup) {
  visit_text <- str_squish(str_replace_all(visit, "\\.", " "))
  label_text <- str_squish(str_replace_all(visit_label, "\\.", " "))
  lookup_text <- str_squish(str_replace_all(visit_label_lookup, "\\.", " "))

  dplyr::case_when(
    is.na(label_text) | label_text == "" ~ lookup_text,
    label_text == visit_text & !is.na(lookup_text) & lookup_text != "" ~ lookup_text,
    TRUE ~ label_text
  )
}

format_visit_prefix <- function(visit, visit_label) {
  visit_text <- str_squish(str_replace_all(visit, "\\.", " "))
  label_text <- str_squish(str_replace_all(visit_label, "\\.", " "))

  dplyr::case_when(
    is.na(label_text) | label_text == "" ~ visit_text,
    str_starts(label_text, fixed(visit_text)) ~ label_text,
    TRUE ~ paste0(visit_text, " - ", label_text)
  )
}

visit_sort_key <- function(visit) {
  visit_num <- suppressWarnings(as.integer(str_match(visit, "VISIT\\s*-\\s*(\\d+)")[, 2]))
  if_else(is.na(visit_num), seq_along(visit), visit_num)
}

build_all_edge_templates <- function(data, visit_lookup, edge_id) {
  
  .SPECIAL_SHEETS <- c("Unscheduled Activities", "Setup & Closedown", "Pharmacy")
  .CUSTOM_SHEET   <- "Custom Activities"
  # NOTE: new variables may need to be added in the future - this function should be param
  
  .EDGE_COLS <- c(
    "EDGE Project ID",
    "Template Name",
    "Template Level (Project | Participant | ProjectSite)",
    "Project Arm (Participant only)",
    "Project Site Name (ProjectSite only)",
    "Cost Item Description",
    "Analysis Code",
    "Cost Category",
    "Default Cost",
    "Currency",
    "Department",
    "Overhead Cost",
    "Time", 
    "Activity Type"
  )

  empty_edge_template <- tibble::tibble(
    `EDGE Project ID` = character(),
    `Template Name` = character(),
    `Template Level (Project | Participant | ProjectSite)` = character(),
    `Project Arm (Participant only)` = character(),
    `Project Site Name (ProjectSite only)` = character(),
    `Cost Item Description` = character(),
    `Analysis Code` = character(),
    `Cost Category` = character(),
    `Default Cost` = numeric(),
    `Currency` = character(),
    `Department` = character(),
    `Overhead Cost` = character(),
    `Time` = character(),
    `Activity Type` = character()
  )
  
  # NOTE: edge_key is assigned upstream by assign_edge_keys() and arrives on
  # the posting lines data already populated.
  if (!"edge_key" %in% names(data)) {
    stop("build_all_edge_templates(): incoming data is missing 'edge_key'. ",
         "Make sure assign_edge_keys() runs before this in the pipeline.")
  }
  
  if (missing(visit_lookup) || is.null(visit_lookup) || nrow(visit_lookup) == 0) {
    stop("build_all_edge_templates(): 'visit_lookup' is required and must contain ",
         "Study, Study_Arm, Visit_Label, Visit_Number.")
  }

  visit_labels_lookup <- visit_lookup |>
    filter(!is.na(Visit_Label), nzchar(trimws(Visit_Label))) |>
    summarise(
      visit_label_lookup = dplyr::first(Visit_Label),
      .by = c(Study, Visit_Number)
    )
  
  # ── Build templates ───────────────────────────────────────────────────────────

  screening_failure_default_cost <- function(total) {
    # Screening failure EDGE defaults are temporarily forced to zero while the
    # rest of the export pipeline continues to reconcile on adjusted totals.
    total * 0
  }
  
  .build_special <- function(df) {
    df |>
      summarise(
        total = sum(adjusted_amount, na.rm = TRUE),
        .by   = c(Study_Arm, sheet_name, Activity, row_id, 
                  staff_group, edge_key, Department, study_name, cpms_id)
      ) |>
      mutate(
        `EDGE Project ID`                                      = edge_id,
        `Template Name`                                        = sheet_name,
        `Template Level (Project | Participant | ProjectSite)` = NA,
        `Project Arm (Participant only)`                       = sheet_name,
        `Project Site Name (ProjectSite only)`                 = NA,
        `Cost Item Description`                                = str_replace_all(Activity, "\\.", " "),
        `Analysis Code`                                        = edge_key,
        `Cost Category`                                        = "Research Cost",
        `Default Cost`                                         = total,
        `Currency`                                             = "GBP",
        `Department`                                           = Department,
        `Overhead Cost`                                        = NA,
        `Time`                                                 = NA,
        `Activity Type`                                        = NA
      ) |>
      select(all_of(.EDGE_COLS))
  }

  .build_screening <- function(df) {
    df |>
      summarise(
        total = sum(adjusted_amount, na.rm = TRUE),
        .by   = c(sheet_name, Visit, Visit_Label, Activity, row_id, staff_group, edge_key, study_name, cpms_id)
      ) |>
      left_join(
        visit_labels_lookup,
        by = c("study_name" = "Study", "Visit" = "Visit_Number")
      ) |>
      arrange(Visit, row_id, staff_group) |>
      mutate(
        Visit_Label = resolve_visit_label(Visit, Visit_Label, visit_label_lookup),
        `EDGE Project ID`                                      = edge_id,
        `Template Name`                                        = sheet_name,
        `Template Level (Project | Participant | ProjectSite)` = NA,
        `Project Arm (Participant only)`                       = sheet_name,
        `Project Site Name (ProjectSite only)`                 = NA,
        `Cost Item Description`                                = paste0(
          format_visit_prefix(Visit, Visit_Label),
          " - ",
          str_replace_all(Activity, "\\.", " ")
        ),
        `Analysis Code`                                        = edge_key,
        `Cost Category`                                        = "Research Cost",
        `Default Cost`                                         = screening_failure_default_cost(total),
        `Currency`                                             = "GBP",
        `Department`                                           = NA,
        `Overhead Cost`                                        = NA,
        `Time`                                                 = NA,
        `Activity Type`                                        = NA
      ) |>
      select(-visit_label_lookup) |>
      select(all_of(.EDGE_COLS))
  }
  
  # ── ADDON ── Build custom-activity rows in the chosen Study_Arm template ──
  # Custom rows are user-added activities that should appear inside the
  # selected arm's template alongside the visit rows. One EDGE row per
  # custom_activity (i.e. per edge_key), summing the slot amounts. Description
  # comes from the user-entered Activity name.
  .build_custom <- function(df) {
    df |>
      filter(sheet_name == .CUSTOM_SHEET) |>
      summarise(
        total = sum(adjusted_amount, na.rm = TRUE),
        .by   = c(Study_Arm, Activity, edge_key, study_name, cpms_id)
      ) |>
      mutate(
        `EDGE Project ID`                                      = edge_id,
        `Template Name`                                        = Study_Arm,
        `Template Level (Project | Participant | ProjectSite)` = NA,
        `Project Arm (Participant only)`                       = NA,
        `Project Site Name (ProjectSite only)`                 = NA,
        `Cost Item Description`                                = str_replace_all(Activity, "\\.", " "),
        `Analysis Code`                                        = edge_key,
        `Cost Category`                                        = "Research Cost",
        `Default Cost`                                         = total,
        `Currency`                                             = "GBP",
        `Department`                                           = NA,
        `Overhead Cost`                                        = NA,
        `Time`                                                 = NA,
        `Activity Type`                                        = NA
      ) |>
      select(all_of(.EDGE_COLS))
  }
  # ──────────────────────────────────────────────────────────────────────────
  
  .build_main <- function(df) {
    # Custom rows are excluded from the visit-style build path — they're
    # handled separately by .build_custom() and merged below.
    df <- df |> filter(sheet_name != .CUSTOM_SHEET)
    template_name <- dplyr::first(df$template_arm)
    
    if (nrow(df) == 0) {
      # Empty arm (everything was custom) — return an empty template with
      # the right column shape so downstream code doesn't break.
      return(empty_edge_template)
    }

    itemised_rows <- df |> filter(is_itemised_export_row(sheet_name, Study_Arm))
    visit_rows    <- df |> filter(!is_itemised_export_row(sheet_name, Study_Arm))
    scheduled_visit_sequence <- visit_rows |>
      distinct(Visit) |>
      mutate(
        row_type = "scheduled",
        visit_sort = visit_sort_key(Visit)
      ) |>
      arrange(visit_sort, Visit) |>
      mutate(display_visit = paste0("VISIT - ", str_pad(row_number(), 3, pad = "0"))) |>
      select(Visit, row_type, display_visit)

    itemised_visit_sequence <- itemised_rows |>
      distinct(Visit) |>
      mutate(
        row_type = "itemised",
        visit_sort = visit_sort_key(Visit)
      ) |>
      arrange(visit_sort, Visit) |>
      mutate(
        display_visit = paste0(
          "VISIT - ",
          str_pad(row_number() + nrow(scheduled_visit_sequence), 3, pad = "0")
        )
      ) |>
      select(Visit, row_type, display_visit)

    itemised_built <- if (nrow(itemised_rows) > 0) {
      itemised_rows |>
        summarise(
          total = sum(adjusted_amount, na.rm = TRUE),
          .by   = c(study_name, Visit, Study_Arm, Visit_Label, Activity, row_id, staff_group, edge_key)
        ) |>
        mutate(row_type = "itemised") |>
        left_join(itemised_visit_sequence, by = c("Visit", "row_type")) |>
        left_join(
          visit_labels_lookup,
          by = c("study_name" = "Study", "Visit" = "Visit_Number")
        ) |>
        arrange(Visit, row_id, staff_group) |>
        mutate(
          Visit_Label = resolve_visit_label(Visit, Visit_Label, visit_label_lookup),
          item_text = str_replace_all(Activity, "\\.", " "),
          visit_prefix = format_visit_prefix(display_visit, Visit_Label),
          `EDGE Project ID`                                      = edge_id,
          `Template Name`                                        = template_name,
          `Template Level (Project | Participant | ProjectSite)` = NA,
          `Project Arm (Participant only)`                       = NA,
          `Project Site Name (ProjectSite only)`                 = NA,
          `Cost Item Description`                                = paste0(visit_prefix, " - ", item_text),
          `Analysis Code`                                        = edge_key,
          `Cost Category`                                        = "Research Cost",
          `Default Cost`                                         = screening_failure_default_cost(total),
          `Currency`                                             = "GBP",
          `Department`                                           = NA,
          `Overhead Cost`                                        = NA,
          `Time`                                                 = NA,
          `Activity Type`                                        = NA
        ) |>
        select(-row_type) |>
        select(-visit_label_lookup) |>
        select(all_of(.EDGE_COLS))
    } else {
      empty_edge_template
    }

    visit_built <- if (nrow(visit_rows) > 0) {
      visit_keys <- visit_rows |>
        filter(sheet_name != "Pharmacy") |>
        distinct(Study_Arm, Visit, edge_key)
      
      visit_rows |>
        summarise(
          total = sum(adjusted_amount, na.rm = TRUE),
          .by   = c(study_name, Visit, Study_Arm, Visit_Label)
        ) |>
        mutate(row_type = "scheduled") |>
        left_join(scheduled_visit_sequence, by = c("Visit", "row_type")) |>
        left_join(visit_keys, by = c("Study_Arm", "Visit")) |>
        left_join(
          visit_labels_lookup,
          by = c("study_name" = "Study", "Visit" = "Visit_Number")
        ) |>
        arrange(Visit) |>
        mutate(
          Visit_Label = resolve_visit_label(Visit, Visit_Label, visit_label_lookup),
          `EDGE Project ID`                                      = edge_id,
          `Template Name`                                        = template_name,
          `Template Level (Project | Participant | ProjectSite)` = NA,
          `Project Arm (Participant only)`                       = NA,
          `Project Site Name (ProjectSite only)`                 = NA,
          `Cost Item Description`                                = format_visit_prefix(display_visit, Visit_Label),
          `Analysis Code`                                        = edge_key,
          `Cost Category`                                        = "Research Cost",
          `Default Cost`                                         = total,
          `Currency`                                             = "GBP",
          `Department`                                           = NA,
          `Overhead Cost`                                        = NA,
          `Time`                                                 = NA,
          `Activity Type`                                        = NA
        ) |>
        select(-row_type) |>
        select(-visit_label_lookup) |>
        select(all_of(.EDGE_COLS))
    } else {
      empty_edge_template
    }

    bind_rows(visit_built, itemised_built)
  }
  
  # ── Dispatch and return ───────────────────────────────────────────────────────
  
  special_data <- data |> filter(sheet_name %in% .SPECIAL_SHEETS)
  screening_data <- data |> filter(is_screening_failure_sheet(sheet_name))
  
  # Main data = everything that isn't a "special sheet" AND isn't custom,
  # plus Pharmacy (which appears in both buckets historically).
  # Custom rows are handled separately and merged into the per-arm output below.
  main_data <- data |> 
    filter(
      (!sheet_name %in% .SPECIAL_SHEETS | sheet_name == "Pharmacy") &
        sheet_name != .CUSTOM_SHEET &
        !is_screening_failure_sheet(sheet_name)
    ) |>
    mutate(
      template_arm = resolve_edge_template_arm(sheet_name, Study_Arm)
    )
  
  custom_data <- data |> filter(sheet_name == .CUSTOM_SHEET)
  
  special_list <- if (nrow(special_data) > 0) {
    special_data |>
      group_by(sheet_name) |>
      group_map(~ .build_special(.x), .keep = TRUE) |>
      setNames(sort(unique(special_data$sheet_name)))
  } else {
    list()
  }

  screening_list <- if (nrow(screening_data) > 0) {
    screening_data |>
      group_by(sheet_name) |>
      group_map(~ .build_screening(.x), .keep = TRUE) |>
      setNames(sort(unique(screening_data$sheet_name)))
  } else {
    list()
  }
  
  main_list <- if (nrow(main_data) > 0) {
    main_data |>
      group_by(template_arm) |>
      group_map(~ .build_main(.x), .keep = TRUE) |>
      setNames(sort(unique(main_data$template_arm)))
  } else {
    list()
  }
  
  # ── ADDON ── Merge custom rows into their selected arms' templates ────────
  # For each Study_Arm that has custom rows, build the custom block and
  # bind_rows() it onto the existing arm template. The arm could be in
  # main_list OR special_list (e.g. if the user picked "Pharmacy"), so we
  # try main first, then special, then create a new entry.
  if (nrow(custom_data) > 0) {
    custom_built <- .build_custom(custom_data)
    
    for (arm in unique(custom_built$`Template Name`)) {
      arm_custom <- custom_built |> filter(`Template Name` == arm)
      
      if (arm %in% names(main_list)) {
        main_list[[arm]] <- bind_rows(main_list[[arm]], arm_custom)
      } else if (arm %in% names(special_list)) {
        special_list[[arm]] <- bind_rows(special_list[[arm]], arm_custom)
      } else {
        main_list[[arm]] <- arm_custom
      }
    }
  }
  # ──────────────────────────────────────────────────────────────────────────
  
  c(special_list, screening_list, main_list)
}
