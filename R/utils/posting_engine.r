suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(openxlsx)
})

source("R/persistence/connection.R", local = FALSE)
source("R/persistence/db_helpers.R", local = FALSE)
source("R/persistence/repositories/rules_repository.R", local = FALSE)
source("R/persistence/repositories/ict_costing_repository.R", local = FALSE)

# ======================================================================
# Helpers
# ======================================================================

#' Evaluate a single rule condition against the current row context.
#'
#' Supports: is_medic = TRUE / FALSE (MVP).
#' Returns TRUE when no condition is specified (unconditional rule).
#' Returns FALSE for unknown conditions (fail-safe).
condition_passes <- function(condition_field, condition_op, condition_value, is_medic) {
  if (is.na(condition_field) || condition_field == "") return(TRUE)
  
  if (condition_field == "is_medic" && condition_op == "=" &&
      condition_value %in% c("TRUE", "FALSE")) {
    return(is_medic == (condition_value == "TRUE"))
  }
  
  FALSE
}

normalize_mff_split_config <- function(mff_split_enabled = FALSE, mff_split_pct = 0) {
  split_enabled <- isTRUE(mff_split_enabled)
  split_pct <- suppressWarnings(as.numeric(mff_split_pct))

  if (!is.finite(split_pct)) {
    split_pct <- 0
  }
  if (split_pct < 0 || split_pct > 1) {
    stop("mff_split_pct must be between 0 and 1 inclusive.")
  }

  list(
    mff_split_enabled = split_enabled,
    mff_split_pct = split_pct,
    split_active = split_enabled && split_pct > 0
  )
}

# ======================================================================
# 1. ICT Input
# ======================================================================

#' Read the Stage B output workbook (all sheets) into a single long dataframe.
#'
#' This is the output of pipeline_fixed.r's process_workbook(), which has already:
#'   - Resolved the old Visit_Name ambiguity into Visit_Label + Activity columns
#'   - Added Study_Arm per row (via add_study_arm.r)
#'   - Pivoted to long format (one row per activity-visit occurrence)
#'
#' @param ict  Path to the .xlsx workbook, OR a pre-loaded named list of dataframes
#'             (as returned by process_workbook()), OR a single pre-stacked dataframe.
#' @return     Single dataframe with a `sheet_name` column identifying the source sheet.
read_ict_workbook <- function(ict) {
  
  # Already a single stacked dataframe (e.g. passed from memory)
  if (is.data.frame(ict)) {
    df <- ict
    
    # Named list of dataframes (direct output from process_workbook / run_stage_b)
  } else if (is.list(ict) && !is.data.frame(ict)) {
    df <- imap_dfr(ict, function(d, nm) {
      if (is.null(d) || nrow(d) == 0) return(NULL)
      d$sheet_name <- nm
      d
    })
    
    # File path
  } else if (is.character(ict) && length(ict) == 1) {
    if (!file.exists(ict)) stop("read_ict_workbook(): file not found: ", ict)
    sheet_names <- getSheetNames(ict)
    df <- map_dfr(sheet_names, function(sheet) {
      d <- read.xlsx(ict, sheet = sheet)
      d$sheet_name <- sheet
      d
    })
  } else {
    stop("read_ict_workbook(): `ict` must be a filepath, dataframe, or named list of dataframes.")
  }
  
  # Stable row_id (unique across all sheets)
  if (!"row_id" %in% names(df)) df$row_id <- seq_len(nrow(df))
  
  # Required columns
  required <- c("Activity.Type", "Staff.Role", "Activity.Cost")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("read_ict_workbook(): missing required columns: ", paste(missing, collapse = ", "))
  }
  
  # MVP placeholders for routing context
  if (!"provider_org" %in% names(df)) df$provider_org <- NA_character_
  if (!"pi_org"       %in% names(df)) df$pi_org       <- NA_character_
  if (!"calc_tag"     %in% names(df)) df$calc_tag      <- NA_character_
  
  # Ensure corrected-schema columns exist (from pipeline_fixed.r).
  # These should already be present; warn if missing so it's obvious.
  if (!"Study_Arm" %in% names(df)) {
    warning("read_ict_workbook(): 'Study_Arm' column missing -- defaulting to sheet_name. ",
            "This means the input may not have come from the corrected pipeline.")
    df$Study_Arm <- df$sheet_name
  }
  if (!"Arm_Identity" %in% names(df)) {
    warning("read_ict_workbook(): 'Arm_Identity' column missing -- defaulting to Study_Arm. ",
            "Multi-arm unscheduled rows may not join correctly.")
    source_sheet <- if ("SheetName" %in% names(df)) {
      df$SheetName
    } else if ("sheet_name" %in% names(df)) {
      df$sheet_name
    } else {
      df$Study_Arm
    }
    df$Arm_Identity <- ifelse(df$Study_Arm == "UA", source_sheet, df$Study_Arm)
  }
  if (!"Visit_Label" %in% names(df)) {
    warning("read_ict_workbook(): 'Visit_Label' column missing -- defaulting to Visit. ",
            "The old overloaded Visit_Name may cause incorrect joins downstream.")
    df$Visit_Label <- if ("Visit" %in% names(df)) df$Visit else NA_character_
  }
  if (!"staff_group" %in% names(df)) {
    warning("read_ict_workbook(): 'staff_group' column missing -- defaulting to 1. ",
            "Duplicate activities with different staff may not join correctly.")
    df$staff_group <- 1L
  }
  
  df
}

# ======================================================================
# 2. Row Normalisation
# ======================================================================

#' Normalise and derive row-level context columns.
#'
#' Adds: activity_type_norm, staff_role_norm, row_category_auto, row_category,
#'       is_medic, scenario_id, ruleset_id.
normalise_rows <- function(df, scenario_id, ruleset_id) {
  df %>%
    mutate(
      activity_type_norm = str_to_lower(str_trim(.data$Activity.Type)),
      staff_role_norm    = str_to_lower(str_trim(.data$Staff.Role)),
      
      row_category_auto = if_else(
        str_detect(activity_type_norm, "investigation"),
        "INVESTIGATION",
        "BASELINE"
      ),
      
      # Clean calc_tag: blank / whitespace -> NA
      calc_tag = if_else(is.na(calc_tag), NA_character_, str_trim(as.character(calc_tag))),
      calc_tag = if_else(calc_tag == "", NA_character_, calc_tag),
      
      # Effective category: manual override wins, else auto
      row_category = if_else(!is.na(calc_tag), calc_tag, row_category_auto),
      
      is_medic    = (str_trim(.data$Staff.Role) == "Medical Staff"),
      scenario_id = .env$scenario_id,
      ruleset_id  = .env$ruleset_id
    )
}

# ======================================================================
# 3. DB Layer
# ======================================================================

#' Load dist_rules, amount_map, and routing_rules from the finance rules DB.
load_rules <- function(db_path, ruleset_id) {
  if (identical(get0("STORAGE_MODE", ifnotfound = "duckdb"), "postgres")) {
    return(rids_repos()$rules$ruleset_bundle(ruleset_id))
  }

  if (!file.exists(db_path)) stop("load_rules(): DB not found: ", db_path)
  
  with_read_connection(db_path, function(con) {
    rules_repository(con)$ruleset_bundle(ruleset_id)
  })
}

#' Join saved contract costs from ict_costing_tbl using the CORRECTED composite key.
#'
#' ── Why this exists ──
#' The old system (join_testv10 -> import_refactor_v22) joined on `Visit_Name`
#' which was overloaded:
#'   Scheduled rows:  merge(... by.y = c("Visit_Number", "Visit_Name", ...))
#'                    where Visit_Name = column header like "Screening"
#'   UA/SSP/SC rows:  merge(... by.y = c("Visit_Number", "Visit_Name", ...))
#'                    where Visit_Name = activity name like "Blood Test"
#' Same column, different semantics -> silent misjoins and NAs.
#'
#' pipeline_fixed.r split this into:
#'   Visit_Label   = visit dimension  (e.g. "Screening", "Day 30")
#'   Activity_Name = activity dimension (e.g. "Blood Test", "CT Scan")
#'
#' This function joins on the UNAMBIGUOUS key:
#'   (cpms_id, study_site, scenario_id, Visit, Study_Arm, Activity)
#'   -> (CPMS_ID, study_site, scenario_id, Visit_Number, Study_Arm, Activity_Name)
#'
#' For MFF summary rows (Activity_Name IS NULL in the lookup), we fall back to
#' visit-level: (CPMS_ID, study_site, scenario_id, Visit_Number, Study_Arm).
#'
#' @param df       Normalised posting dataframe.
#' @param db_path  Path to the DuckDB containing ict_costing_tbl.
#' @return df with `contract_cost` column attached from saved Step 2 values.
join_ict_costs <- function(df, db_path) {
  if (identical(get0("STORAGE_MODE", ifnotfound = "duckdb"), "postgres")) {
    repo <- rids_repos()$ict_costing
    ict <- if (repo$exists_table()) repo$all_contract_costs() else NULL
  } else if (!file.exists(db_path)) {
    warning("join_ict_costs(): DB not found: ", db_path, " -- skipping contract cost join.")
    df$contract_cost <- NA_real_
    return(df)
  } else {
    ict <- with_read_connection(db_path, function(con) {
      repo <- ict_costing_repository(con)
      if (!repo$exists_table()) {
        return(NULL)
      }
      repo$all_contract_costs()
    })
  }

  if (is.null(ict)) {
    warning("join_ict_costs(): ict_costing_tbl not found in DB -- skipping.")
    df$contract_cost <- NA_real_
    return(df)
  }
  
  # ── Row-level join: activity rows (Activity_Name NOT NULL) ─────────────────
  # staff_group is the disambiguator: same activity at the same visit with
  # different staff/costs gets a unique staff_group in pipeline_fixed.r.
  # Including it in the join key gives a clean 1:1 match.
  if (!"Arm_Identity" %in% names(df)) {
    df$Arm_Identity <- df$Study_Arm
  }
  if (!"Arm_Identity" %in% names(ict)) {
    ict$Arm_Identity <- ict$Study_Arm
  }

  ict_activity <- ict %>%
    filter(!is.na(Activity_Name)) %>%
    select(CPMS_ID, study_site, scenario_id, Visit_Number, Arm_Identity,
           Activity_Name, staff_group, Contract_Cost)
  
  df <- df %>%
    left_join(
      ict_activity %>% rename(contract_cost_activity = Contract_Cost),
      by = c("cpms_id"     = "CPMS_ID",
             "study_site"  = "study_site",
             "scenario_id" = "scenario_id",
             "Visit"       = "Visit_Number",
             "Arm_Identity" = "Arm_Identity",
             "Activity"    = "Activity_Name",
             "staff_group" = "staff_group")
    )
  
  # ── Visit-level join: MFF summary rows (Activity_Name IS NULL) ─────────────
  # MFF rows don't have activity_occurrence_id, so join on visit-level key only.
  ict_visit <- ict %>%
    filter(is.na(Activity_Name)) %>%
    select(CPMS_ID, study_site, scenario_id, Visit_Number, Arm_Identity, Contract_Cost) %>%
    rename(contract_cost_visit = Contract_Cost)
  
  df <- df %>%
    left_join(
      ict_visit,
      by = c("cpms_id"   = "CPMS_ID",
             "study_site" = "study_site",
             "scenario_id" = "scenario_id",
             "Visit"     = "Visit_Number",
             "Arm_Identity" = "Arm_Identity")
    )
  
  # ── Coalesce: prefer activity-level, fall back to visit-level ──────────────
  df <- df %>%
    mutate(contract_cost = coalesce(contract_cost_activity, contract_cost_visit)) %>%
    select(-contract_cost_activity, -contract_cost_visit)
  
  df
}

# ======================================================================
# 4. Distribution Rules -> Posting Lines
# ======================================================================

#' Match distribution rules to each ICT row and explode into posting lines.
#'
#' Carries the corrected-schema columns (Study_Arm, Visit_Label,
#' activity_occurrence_id) through so they appear in the final output.
apply_dist_rules <- function(df, dist_rules, scenario_id,
                             mff_split_enabled = FALSE, mff_split_pct = 0) {
  split_cfg <- normalize_mff_split_config(mff_split_enabled, mff_split_pct)
  
  # Columns to carry through — intersect so missing optional cols don't error
  carry_cols <- intersect(
    names(df),
    c("sheet_name", "Study_Arm", "Arm_Identity", "Visit", "Activity", "cpms_id", "study_name",
      "row_id", "scenario_id", "row_category_auto", "calc_tag", "row_category",
      "is_medic", "Visit_Label", "activity_occurrence_id", "staff_group",
      "provider_org", "pi_org", "Activity.Cost", "contract_cost")
  )
  
  df %>%
    select(all_of(carry_cols)) %>%
    mutate(
      activity_cost_num = as.numeric(gsub("[^0-9.]", "", .data$Activity.Cost))
    ) %>%
    rowwise() %>%
    mutate(
      posting_lines = list({
        cand <- dist_rules %>%
          filter(.data$scenario_id == .env$scenario_id,
                 .data$row_category == .env$row_category) %>%
          mutate(
            ok = pmap_lgl(
              list(condition_field, condition_op, condition_value),
              ~ condition_passes(..1, ..2, ..3, is_medic)
            )
          ) %>%
          filter(ok) %>%
          arrange(priority)
        
        if (!split_cfg$split_active) {
          cand <- cand %>%
            filter(.data$posting_line_type_id != "MFF_SPLIT_NEW_CC")
        }
        
        unique(cand$posting_line_type_id)
      })
    ) %>%
    ungroup() %>%
    select(-Activity.Cost) %>%
    unnest(posting_lines) %>%
    rename(posting_line_type_id = posting_lines)
}

# ======================================================================
# 5. Amount Calculation
# ======================================================================

#' Join amount_map and compute posting_amount = AC * mff_rate * base_mult * split_mult.
apply_amount_map <- function(posting_plan, amount_map, mff_rate,
                             mff_split_enabled = FALSE, mff_split_pct = 0) {
  split_cfg <- normalize_mff_split_config(mff_split_enabled, mff_split_pct)
  mff_uplift <- mff_rate - 1
  existing_mff_rate <- if (split_cfg$split_active) {
    1 + (mff_uplift * (1 - split_cfg$mff_split_pct))
  } else {
    mff_rate
  }

  posting_plan <- posting_plan %>%
    left_join(amount_map, by = "posting_line_type_id") %>%
    mutate(
      calc_method = coalesce(calc_method, "STANDARD")
    ) %>%
    group_by(row_id) %>%
    mutate(
      total_before_mff_factor = sum(
        if_else(
          calc_method == "MFF_SPLIT_ONLY",
          0,
          base_mult * split_mult
        ),
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    mutate(
      missing_amount_map = is.na(base_mult) | is.na(split_mult) | is.na(calc_method),
      posting_amount = case_when(
        calc_method == "STANDARD" ~ activity_cost_num * existing_mff_rate * base_mult * split_mult,
        calc_method == "MFF_SPLIT_ONLY" ~ activity_cost_num * total_before_mff_factor * mff_uplift * split_cfg$mff_split_pct,
        TRUE ~ NA_real_
      )
    )
  
  if (any(posting_plan$missing_amount_map)) {
    bad <- posting_plan %>%
      filter(missing_amount_map) %>%
      distinct(posting_line_type_id)
    stop("Missing amount_map for posting_line_type_id(s): ",
         paste(bad$posting_line_type_id, collapse = ", "))
  }
  
  posting_plan
}

# ======================================================================
# 6. Routing
# ======================================================================

#' Resolve the destination bucket for a single posting line.
resolve_destination <- function(scenario_id, posting_line_type_id, is_medic,
                                routing_rules) {
  cand <- routing_rules %>%
    filter(.data$scenario_id          == .env$scenario_id,
           .data$posting_line_type_id == .env$posting_line_type_id) %>%
    mutate(
      ok = pmap_lgl(
        list(condition_field, condition_op, condition_value),
        ~ condition_passes(..1, ..2, ..3, is_medic)
      )
    ) %>%
    filter(ok) %>%
    arrange(priority)
  
  if (nrow(cand) == 0) return(NA_character_)
  cand$destination_bucket[[1]]
}

#' Apply routing rules to the full posting plan.
apply_routing <- function(posting_plan, routing_rules) {
  posting_plan <- posting_plan %>%
    rowwise() %>%
    mutate(
      destination_bucket = resolve_destination(
        scenario_id, posting_line_type_id, is_medic, routing_rules
      )
    ) %>%
    ungroup()
  
  if (any(is.na(posting_plan$destination_bucket))) {
    bad <- posting_plan %>%
      filter(is.na(destination_bucket)) %>%
      distinct(scenario_id, posting_line_type_id)
    stop("Missing routing for posting line: ",
         bad$scenario_id[1], " / ", bad$posting_line_type_id[1])
  }
  
  posting_plan
}

# ======================================================================
# 7. Entity Resolution
# ======================================================================

#' Map destination buckets to logical entities.
resolve_entities <- function(posting_plan) {
  posting_plan %>%
    mutate(
      destination_entity = case_when(
        destination_bucket == "DEST_PROVIDER" ~ provider_org,
        destination_bucket == "DEST_PI_ORG"   ~ pi_org,
        destination_bucket == "DEST_RD"       ~ "R&D",
      destination_bucket == "DEST_TRUST_OH" ~ "TRUST_OVERHEAD",
      destination_bucket == "DEST_SUPPORT"  ~ "SUPPORT_BUCKET",
      destination_bucket == "DEST_MFF_SPLIT" ~ "MFF_SPLIT_CC",
      TRUE                                  ~ "UNKNOWN"
    ),
      cost_code = NA_character_
    )
}

# ======================================================================
# 8. Final Output
# ======================================================================

#' Trim to canonical output columns and sort.
#'
#' Includes the corrected-schema columns so that any downstream consumer
#' (EDGE import, reporting, etc.) can join unambiguously on
#' (cpms_id, Visit, Study_Arm, Activity) -- never the old overloaded Visit_Name.
select_output_cols <- function(posting_plan) {
  
  # Core output columns (always present)
  core <- c(
    "row_id", "scenario_id", "row_category_auto", "calc_tag", "row_category",
    "is_medic", "cpms_id", "study_name", "Study_Arm", "Activity", "Visit",
    "posting_line_type_id", "posting_amount",
    "destination_bucket", "destination_entity", "cost_code"
  )
  
  # Optional columns from corrected schema (present if pipeline_fixed.r was used)
  optional <- c("sheet_name", "Arm_Identity", "Visit_Label", "activity_occurrence_id", "staff_group", "contract_cost")
  
  all_cols <- c(core, intersect(optional, names(posting_plan)))
  
  posting_plan %>%
    select(all_of(all_cols)) %>%
    arrange(row_id, posting_line_type_id)
}

# ======================================================================
# Entry Point
# ======================================================================

#' Generate a long posting plan from ICT data and DuckDB finance rules.
#'
#' @param ict            Path to Stage B output .xlsx, OR a pre-loaded dataframe / named list.
#' @param rules_db_path  Path to the finance rules DuckDB (dist_rules, amount_map, routing_rules).
#' @param scenario_id    Scenario letter ("A" .. "H").
#' @param ruleset_id     Ruleset identifier (default "COMM_AH_V1").
#' @param mff_rate       MFF multiplier (default 1.08).
#' @param ict_db_path    Optional. Path to the DuckDB containing ict_costing_tbl.
#'                       If provided, contract costs are joined using the corrected
#'                       composite key (CPMS_ID + Visit_Number + Study_Arm + Activity_Name).
#'                       If NULL (default), contract_cost column is omitted.
#' @param output_path    Optional. If provided, writes the result as CSV here.
#' @return Dataframe: the long posting plan (returned invisibly).
generate_posting_plan <- function(ict,
                                  rules_db_path,
                                  scenario_id,
                                  ruleset_id  = "COMM_AH_V1",
                                  mff_rate    = 1.08,
                                  mff_split_enabled = FALSE,
                                  mff_split_pct = 0,
                                  ict_db_path = NULL,
                                  output_path = NULL) {
  
  # 1. Read & validate ICT
  message("--- Reading ICT data ---")
  df <- read_ict_workbook(ict)
  
  # 2. Normalise rows
  message("--- Normalising row context ---")
  df <- normalise_rows(df, scenario_id, ruleset_id)
  
  # 3. Optional: join contract costs from ict_costing_tbl (corrected keys)
  if (!is.null(ict_db_path)) {
    message("--- Joining ICT contract costs (corrected keys) ---")
    df <- join_ict_costs(df, ict_db_path)
  }
  
  # 4. Load finance rules from DB
  message("--- Loading finance rules ---")
  rules <- load_rules(rules_db_path, ruleset_id)
  
  # 5. Match dist rules -> posting lines
  message("--- Applying distribution rules ---")
  plan <- apply_dist_rules(
    df,
    rules$dist_rules,
    scenario_id,
    mff_split_enabled = mff_split_enabled,
    mff_split_pct = mff_split_pct
  )
  
  # 6. Calculate amounts
  message("--- Calculating posting amounts ---")
  plan <- apply_amount_map(
    plan,
    rules$amount_map,
    mff_rate,
    mff_split_enabled = mff_split_enabled,
    mff_split_pct = mff_split_pct
  )
  
  # 7. Route to destinations
  message("--- Resolving routing ---")
  plan <- apply_routing(plan, rules$routing_rules)
  
  # 8. Resolve entities
  message("--- Resolving destination entities ---")
  plan <- resolve_entities(plan)
  
  # 9. Final output
  out <- select_output_cols(plan)
  
  if (!is.null(output_path)) {
    write_csv(out, output_path)
    message("Wrote posting plan to: ", output_path)
  }
  
  message("--- Posting plan complete: ", nrow(out), " rows, scenario ", scenario_id, " ---")
  invisible(out)
}

# -- CLI convenience -----------------------------------------------------------
# Uncomment and set paths to run standalone:
#
# generate_posting_plan(
#   ict           = "path/to/stage_b_output.xlsx",
#   rules_db_path = "path/to/finance_rules_AH.duckdb",
#   scenario_id   = "A",
#   ict_db_path   = "path/to/ict_local.duckdb",    # optional: contract cost join
#   output_path   = "path/to/posting_plan.csv"
# )
