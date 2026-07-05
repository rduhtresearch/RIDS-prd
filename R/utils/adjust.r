suppressPackageStartupMessages(library(dplyr))

# Uses `contract_cost` exactly as saved in Step 2. Rounded mode saves whole
# pounds; unrounded mode saves pence, and Step 4 uses those saved values as-is.
adjust_posting_lines <- function(out) {
  
  .ADJUSTMENT_SPECIAL <- c("Unscheduled Activities", "Setup & Closedown")
  
  .adjust <- function(df, group_vars) {
    if (nrow(df) == 0) return(df)

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
  
  bind_rows(
    out %>%
      filter(sheet_name %in% .ADJUSTMENT_SPECIAL) %>%
      .adjust(c("row_id", "Activity", "staff_group", "scenario_id")),

    out %>%
      filter(
        !sheet_name %in% .ADJUSTMENT_SPECIAL,
        is_itemised_adjustment_row(Study_Arm)
      ) %>%
      .adjust(c("row_id", "Activity", "staff_group", "scenario_id")),
    
    out %>%
      filter(
        !sheet_name %in% .ADJUSTMENT_SPECIAL,
        !is_itemised_adjustment_row(Study_Arm)
      ) %>%
      # Group scheduled rows by arm/visit. Screening Failure rows reach this
      # branch with Study_Arm set to their synthetic sheet name, so their
      # itemised output still reconciles to the duplicated visit total.
      mutate(adj_group = trimws(coalesce(Study_Arm, sheet_name))) %>%
      .adjust(c("adj_group", "Visit", "scenario_id")) %>%
      select(-adj_group)
  )
}
