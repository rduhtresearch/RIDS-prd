suppressPackageStartupMessages(library(dplyr))

validate_adjustment_contract_costs <- function(out) {
  if (!"contract_cost" %in% names(out)) {
    stop("Step 4 cannot generate templates because contract costs are missing. Reprocess the workbook from Step 1.")
  }

  missing_cost <- is.na(out$contract_cost) | !is.finite(out$contract_cost)
  if (any(missing_cost)) {
    stop(
      "Step 4 cannot generate templates because ", sum(missing_cost),
      " posting row(s) did not match a saved Step 2 contract cost. ",
      "Reprocess the workbook from Step 1 and review Step 2."
    )
  }

  ua_rows <- trimws(coalesce(as.character(out$Study_Arm), "")) == "UA"
  ua_identity <- if ("Arm_Identity" %in% names(out)) {
    trimws(coalesce(as.character(out$Arm_Identity), ""))
  } else {
    rep("", nrow(out))
  }
  invalid_ua <- ua_rows & ua_identity %in% c("", "UA")
  if (any(invalid_ua)) {
    stop(
      "Step 4 cannot generate UA templates because source-arm identity is missing. ",
      "Reprocess the workbook from Step 1."
    )
  }

  invisible(out)
}

# Uses `contract_cost` exactly as saved in Step 2. Rounded mode saves whole
# pounds; unrounded mode saves pence, and Step 4 uses those saved values as-is.
adjust_posting_lines <- function(out) {
  validate_adjustment_contract_costs(out)
  
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

  adjusted <- bind_rows(
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

  unreconciled <- is.na(adjusted$adjusted_amount) |
    is.na(adjusted$diff_check) |
    abs(adjusted$diff_check) > 0.01
  if (any(unreconciled)) {
    stop(
      "Step 4 cost adjustment failed to reconcile ", sum(unreconciled),
      " posting row(s) to their saved Step 2 contract cost."
    )
  }

  adjusted
}
