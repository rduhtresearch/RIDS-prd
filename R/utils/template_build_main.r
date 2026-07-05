# template_build_main.r
#
# Cost adjustment, EDGE key assignment, and EDGE template generation.
# Sourced by app.R — do not modify pipeline_fixed.r or posting_test.r.
#
# Entry points (called in order by app.R):
#   1. adjust_posting_lines(out)             -> posting_lines_adjusted
#   2. assign_edge_keys(posting_lines_adjusted) -> posting_lines_adjusted (with edge_key)
#   3. build_all_edge_templates(posting_lines_adjusted) -> named list of dataframes

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

# ── Constants ──────────────────────────────────────────────────────────────────

# Sheets adjusted per-activity (not per-visit)
ADJUSTMENT_SPECIAL <- c("Unscheduled Activities", "Setup & Closedown")

# All special sheets (excluded from main arm processing)
SPECIAL_SHEETS <- c("Unscheduled Activities", "Setup & Closedown", "Pharmacy")

# Canonical EDGE import column order
edge_cols <- c(
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
  "Time"
)

format_full_visit_name <- function(visit, visit_label) {
  clean_visit <- str_squish(str_replace_all(visit, "\\.", " "))
  clean_label <- str_squish(str_replace_all(visit_label, "\\.", " "))

  case_when(
    is.na(clean_label) | clean_label == "" ~ clean_visit,
    clean_label == clean_visit ~ clean_visit,
    str_detect(clean_label, "^VISIT\\s*-\\s*\\d{3}\\b") ~ clean_label,
    TRUE ~ paste(clean_visit, clean_label, sep = " - ")
  )
}

# ── Cost adjustment ────────────────────────────────────────────────────────────

# Core adjustment logic: scales posting lines so they sum to the Step 2 saved
# contract cost exactly as saved, with the rounding residual absorbed by the
# DIRECT line.
adjust_postings <- function(df, group_vars) {
  df %>%
    mutate(contract_price = contract_cost) %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(
      base_sum        = sum(posting_amount, na.rm = TRUE),
      contract_price  = first(contract_price),
      multiplier      = if_else(base_sum == 0, NA_real_, contract_price / base_sum),
      adjusted_amount = if_else(base_sum == 0, 0, round(posting_amount * multiplier, 2))
    ) %>%
    mutate(
      residual        = round(contract_price - sum(adjusted_amount, na.rm = TRUE), 2),
      has_direct      = any(posting_line_type_id == "DIRECT"),
      is_residual_row = if_else(
        has_direct,
        posting_line_type_id == "DIRECT" & row_number() == min(which(posting_line_type_id == "DIRECT")),
        row_number() == 1L
      ),
      adjusted_amount = if_else(
        is_residual_row,
        round(adjusted_amount + residual, 2),
        adjusted_amount
      )
    ) %>%
    mutate(
      adjusted_sum_check = round(sum(adjusted_amount, na.rm = TRUE), 2),
      diff_check         = round(contract_price - adjusted_sum_check, 2)
    ) %>%
    select(-has_direct) %>%
    ungroup()
}

#' Adjust posting amounts to match contract price.
#'
#' Special sheets (UA, SC) are grouped per-activity.
#' All other sheets are grouped per-visit within Study_Arm.
adjust_posting_lines <- function(out) {
  bind_rows(
    # UA and SC: adjust per activity
    out %>%
      filter(sheet_name %in% ADJUSTMENT_SPECIAL) %>%
      adjust_postings(c("row_id", "Activity", "staff_group", "scenario_id")),
    
    # Everything else: adjust per visit within Study_Arm
    out %>%
      filter(!sheet_name %in% ADJUSTMENT_SPECIAL) %>%
      mutate(adj_group = trimws(
        coalesce(Study_Arm, sheet_name)
      )) %>%
      adjust_postings(c("adj_group", "Visit", "scenario_id")) %>%
      select(-adj_group)
  )
}

# ── EDGE key assignment ────────────────────────────────────────────────────────

#' Stamp each posting line with a unique EDGE-XXXX key.
#'
#' Special sheets: keyed by sheet_name + Activity + row_id + staff_group.
#' Main arms:      keyed by Study_Arm + Visit.
assign_edge_keys <- function(data) {
  
  special_keys <- data |>
    filter(sheet_name %in% SPECIAL_SHEETS) |>
    distinct(sheet_name, Activity, row_id, staff_group, Study_Arm) |>
    mutate(edge_key = paste0("EDGE-", str_pad(row_number(), width = 4, pad = "0")))
  
  main_keys <- data |>
    filter(!sheet_name %in% SPECIAL_SHEETS) |>
    distinct(Study_Arm, Visit) |>
    mutate(edge_key = paste0("EDGE-", str_pad(
      row_number() + nrow(special_keys), width = 4, pad = "0"
    )))
  
  data |>
    left_join(special_keys, by = c("sheet_name", "Activity", "row_id",
                                   "staff_group", "Study_Arm")) |>
    left_join(main_keys,    by = c("Study_Arm", "Visit")) |>
    mutate(edge_key = coalesce(edge_key.x, edge_key.y)) |>
    select(-edge_key.x, -edge_key.y)
}

# ── Template builders ──────────────────────────────────────────────────────────

#' Build an EDGE template for one special sheet (UA / SC / Pharmacy).
#' One row per activity, summed across posting line types.
build_edge_template <- function(data) {
  
  data |>
    summarise(
      total = sum(adjusted_amount, na.rm = TRUE),
      .by   = c(Study_Arm, sheet_name, Activity, row_id, staff_group, edge_key)
    ) |>
    mutate(
      Full_Visit_Name                                        = paste0("VISIT - ", str_replace_all(Activity, "\\.", " ")),
      `EDGE Project ID`                                      = NA,
      `Template Name`                                        = Study_Arm,
      `Template Level (Project | Participant | ProjectSite)` = "Participant",
      `Project Arm (Participant only)`                       = NA,
      `Project Site Name (ProjectSite only)`                 = NA,
      `Cost Item Description`                                = Full_Visit_Name,
      `Analysis Code`                                        = edge_key,
      `Cost Category`                                        = "Research Cost",
      `Default Cost`                                         = total,
      `Currency`                                             = "GBP",
      `Department`                                           = NA,
      `Overhead Cost`                                        = NA,
      `Time`                                                 = NA
    ) |>
    select(all_of(edge_cols))
}

#' Build an EDGE template for one main study arm.
#' One row per visit, summed across all activities (including Pharmacy).
#' Visit_Label comes directly from the posting data — no ict_table join needed.
build_edge_template_main <- function(data) {
  
  # Edge keys live on main arm rows (not Pharmacy rows)
  visit_keys <- data |>
    filter(sheet_name != "Pharmacy") |>
    distinct(Study_Arm, Visit, edge_key)
  
  data |>
    summarise(
      total = sum(adjusted_amount, na.rm = TRUE),
      .by   = c(study_name, Visit, Study_Arm, Visit_Label)
    ) |>
    left_join(visit_keys, by = c("Study_Arm", "Visit")) |>
    mutate(
      Full_Visit_Name                                        = format_full_visit_name(Visit, Visit_Label),
      `EDGE Project ID`                                      = NA,
      `Template Name`                                        = Study_Arm,
      `Template Level (Project | Participant | ProjectSite)` = "Participant",
      `Project Arm (Participant only)`                       = NA,
      `Project Site Name (ProjectSite only)`                 = NA,
      `Cost Item Description`                                = Full_Visit_Name,
      `Analysis Code`                                        = edge_key,
      `Cost Category`                                        = "Research Cost",
      `Default Cost`                                         = total,
      `Currency`                                             = "GBP",
      `Department`                                           = NA,
      `Overhead Cost`                                        = NA,
      `Time`                                                 = NA
    ) |>
    select(all_of(edge_cols))
}

# ── Entry point ────────────────────────────────────────────────────────────────

#' Build all EDGE templates from an adjusted + keyed posting plan.
#'
#' @param data  Output of adjust_posting_lines() |> assign_edge_keys().
#' @return Named list of dataframes — one per special sheet, one per study arm.
build_all_edge_templates <- function(data) {
  
  special_data  <- data |> filter(sheet_name %in% SPECIAL_SHEETS)
  main_data     <- data |> filter(!sheet_name %in% SPECIAL_SHEETS | sheet_name == "Pharmacy")
  
  special_names <- sort(unique(special_data$sheet_name))
  main_arms     <- sort(unique(main_data$Study_Arm))
  
  special_list <- special_data |>
    group_by(sheet_name) |>
    group_map(~ build_edge_template(.x), .keep = TRUE) |>
    setNames(special_names)
  
  main_list <- main_data |>
    group_by(Study_Arm) |>
    group_map(~ build_edge_template_main(.x), .keep = TRUE) |>
    setNames(main_arms)
  
  c(special_list, main_list)
}
