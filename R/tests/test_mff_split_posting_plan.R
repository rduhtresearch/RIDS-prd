suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

.passed <- 0L
.failed <- 0L

.expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .passed <<- .passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .failed <<- .failed + 1L
  }
}

.expect_error <- function(label, expr) {
  err <- tryCatch({ force(expr); NULL }, error = function(e) e)
  if (!is.null(err)) {
    cat("  PASS  ", label, "  (errored as expected)\n", sep = "")
    .passed <<- .passed + 1L
  } else {
    cat("  FAIL  ", label, "  (expected error, got none)\n", sep = "")
    .failed <<- .failed + 1L
  }
}

.near <- function(x, y, tol = 1e-9) {
  isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tol))
}

.make_rules <- function() {
  dist_rules <- tibble(
    scenario_id = c(
      rep("A", 11L),
      rep("B", 16L)
    ),
    row_category = c(
      rep("BASELINE", 6L),
      rep("INVESTIGATION", 3L),
      rep("SETUP_CLOSE_DEPARTMENTAL", 2L),
      rep("BASELINE", 13L),
      rep("TRAINING_FEE", 3L)
    ),
    condition_field = c(
      rep(NA_character_, 11L),
      "is_medic", "is_medic", "is_medic", "is_medic", "is_medic", "is_medic", "is_medic",
      "is_medic", "is_medic", "is_medic", "is_medic", "is_medic", "is_medic",
      rep(NA_character_, 3L)
    ),
    condition_op = c(
      rep(NA_character_, 11L),
      rep("=", 13L),
      rep(NA_character_, 3L)
    ),
    condition_value = c(
      rep(NA_character_, 11L),
      "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE",
      "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE",
      rep(NA_character_, 3L)
    ),
    posting_line_type_id = c(
      "DIRECT", "CAPACITY_RD", "INDIRECT_50_DELIVERY", "INDIRECT_25_TRUST", "INDIRECT_25_PI", "MFF_SPLIT_NEW_CC",
      "DIRECT", "CAPACITY_RD", "MFF_SPLIT_NEW_CC",
      "DIRECT", "MFF_SPLIT_NEW_CC",
      "DIRECT", "CAPACITY_RD", "INDIRECT_50_DELIVERY", "INDIRECT_25_TRUST", "INDIRECT_25_PI", "MFF_SPLIT_NEW_CC",
      "DIRECT_40_PI", "DIRECT_60_TEAM", "CAPACITY_RD", "INDIRECT_50_DELIVERY", "INDIRECT_25_TRUST", "INDIRECT_25_PI", "MFF_SPLIT_NEW_CC",
      "DIRECT", "CAPACITY_RD", "MFF_SPLIT_NEW_CC"
    ),
    priority = c(
      10, 20, 30, 40, 50, 60,
      10, 20, 30,
      10, 20,
      20, 30, 40, 50, 60, 70,
      5, 6, 20, 30, 40, 50, 60,
      10, 20, 30
    )
  )

  amount_map <- tibble(
    posting_line_type_id = c(
      "DIRECT", "CAPACITY_RD", "INDIRECT_50_DELIVERY", "INDIRECT_25_TRUST",
      "INDIRECT_25_PI", "DIRECT_40_PI", "DIRECT_60_TEAM", "MFF_SPLIT_NEW_CC"
    ),
    base_mult = c(1.0, 0.2, 0.7, 0.7, 0.7, 1.0, 1.0, 0.0),
    split_mult = c(1.0, 1.0, 0.5, 0.25, 0.25, 0.4, 0.6, 0.0),
    applies_to_row_category = c("BOTH", "BOTH", "BASELINE", "BASELINE", "BASELINE", "BASELINE", "BASELINE", "BOTH"),
    calc_method = c("STANDARD", "STANDARD", "STANDARD", "STANDARD", "STANDARD", "STANDARD", "STANDARD", "MFF_SPLIT_ONLY")
  )

  routing_rules <- tibble(
    scenario_id = c(rep("A", 6L), rep("B", 8L)),
    condition_field = rep(NA_character_, 14L),
    condition_op = rep(NA_character_, 14L),
    condition_value = rep(NA_character_, 14L),
    posting_line_type_id = c(
      "DIRECT", "CAPACITY_RD", "INDIRECT_50_DELIVERY", "INDIRECT_25_TRUST", "INDIRECT_25_PI", "MFF_SPLIT_NEW_CC",
      "DIRECT", "CAPACITY_RD", "INDIRECT_50_DELIVERY", "INDIRECT_25_TRUST", "INDIRECT_25_PI", "DIRECT_40_PI", "DIRECT_60_TEAM", "MFF_SPLIT_NEW_CC"
    ),
    destination_bucket = c(
      "DEST_PROVIDER", "DEST_RD", "DEST_SUPPORT", "DEST_TRUST_OH", "DEST_PI_ORG", "DEST_MFF_SPLIT",
      "DEST_PROVIDER", "DEST_RD", "DEST_SUPPORT", "DEST_TRUST_OH", "DEST_PI_ORG", "DEST_PI_ORG", "DEST_SUPPORT", "DEST_MFF_SPLIT"
    ),
    priority = rep(10L, 14L)
  )

  list(dist_rules = dist_rules, amount_map = amount_map, routing_rules = routing_rules)
}

.make_row <- function(cost = 53, scenario_id = "A", activity_type = "Baseline procedure",
                      staff_role = "Research Nurse", calc_tag = NA_character_) {
  tibble(
    row_id = 1L,
    sheet_name = "Treatment Arm",
    cpms_id = "CP1",
    study_site = "RDUHT",
    study_name = "Study A",
    Visit = "VISIT - 001",
    Visit_Label = "Screening",
    Study_Arm = "Treatment",
    Activity = "Blood Test",
    staff_group = 1L,
    provider_org = "RDUHT",
    pi_org = "PI_ORG",
    calc_tag = calc_tag,
    Activity.Type = activity_type,
    Staff.Role = staff_role,
    Activity.Cost = as.character(cost)
  ) %>%
    normalise_rows(scenario_id = scenario_id, ruleset_id = "COMM_AH_V1")
}

.evaluate_row <- function(df, rules, scenario_id, mff_rate = 1.08,
                          mff_split_enabled = FALSE, mff_split_pct = 0) {
  plan <- apply_dist_rules(
    df,
    rules$dist_rules,
    scenario_id,
    mff_split_enabled = mff_split_enabled,
    mff_split_pct = mff_split_pct
  )
  plan <- apply_amount_map(
    plan,
    rules$amount_map,
    mff_rate = mff_rate,
    mff_split_enabled = mff_split_enabled,
    mff_split_pct = mff_split_pct
  )
  plan <- apply_routing(plan, rules$routing_rules)
  resolve_entities(plan)
}

run_mff_split_posting_plan_tests <- function() {
  cat("\n=== mff split posting plan tests ===\n\n")
  .passed <<- 0L
  .failed <<- 0L

  source("R/utils/posting_lines.r")
  rules <- .make_rules()

  cat("[ split disabled ]\n")
  disabled <- .evaluate_row(
    .make_row(),
    rules,
    scenario_id = "A",
    mff_rate = 1.08,
    mff_split_enabled = FALSE,
    mff_split_pct = 0
  )

  .expect("disabled path generates 5 posting lines", nrow(disabled) == 5L)
  .expect("disabled path excludes MFF split line", !"MFF_SPLIT_NEW_CC" %in% disabled$posting_line_type_id)
  .expect("disabled direct matches current formula",
          .near(disabled$posting_amount[disabled$posting_line_type_id == "DIRECT"], 57.24))
  .expect("disabled capacity matches current formula",
          .near(disabled$posting_amount[disabled$posting_line_type_id == "CAPACITY_RD"], 11.448))
  .expect("disabled indirect 50 matches current formula",
          .near(disabled$posting_amount[disabled$posting_line_type_id == "INDIRECT_50_DELIVERY"], 20.034))
  .expect("disabled indirect 25 trust matches current formula",
          .near(disabled$posting_amount[disabled$posting_line_type_id == "INDIRECT_25_TRUST"], 10.017))
  .expect("disabled indirect 25 PI matches current formula",
          .near(disabled$posting_amount[disabled$posting_line_type_id == "INDIRECT_25_PI"], 10.017))
  .expect("disabled total reconciles to current exact total",
          .near(sum(disabled$posting_amount), 108.756))

  cat("\n[ split enabled baseline ]\n")
  enabled <- .evaluate_row(
    .make_row(),
    rules,
    scenario_id = "A",
    mff_rate = 1.08,
    mff_split_enabled = TRUE,
    mff_split_pct = 0.25
  )

  .expect("enabled path generates 6 posting lines", nrow(enabled) == 6L)
  .expect("enabled path includes MFF split line", "MFF_SPLIT_NEW_CC" %in% enabled$posting_line_type_id)
  .expect("enabled direct uses adjusted MFF rate",
          .near(enabled$posting_amount[enabled$posting_line_type_id == "DIRECT"], 56.18))
  .expect("enabled capacity uses adjusted MFF rate",
          .near(enabled$posting_amount[enabled$posting_line_type_id == "CAPACITY_RD"], 11.236))
  .expect("enabled indirect 50 uses adjusted MFF rate",
          .near(enabled$posting_amount[enabled$posting_line_type_id == "INDIRECT_50_DELIVERY"], 19.663))
  .expect("enabled indirect 25 trust uses adjusted MFF rate",
          .near(enabled$posting_amount[enabled$posting_line_type_id == "INDIRECT_25_TRUST"], 9.8315))
  .expect("enabled indirect 25 PI uses adjusted MFF rate",
          .near(enabled$posting_amount[enabled$posting_line_type_id == "INDIRECT_25_PI"], 9.8315))
  .expect("enabled MFF split amount matches carved-out uplift",
          .near(enabled$posting_amount[enabled$posting_line_type_id == "MFF_SPLIT_NEW_CC"], 2.014))
  .expect("enabled exact total reconciles to disabled total",
          .near(sum(enabled$posting_amount), sum(disabled$posting_amount)))
  .expect("enabled split destination bucket resolves",
          identical(enabled$destination_bucket[enabled$posting_line_type_id == "MFF_SPLIT_NEW_CC"], "DEST_MFF_SPLIT"))
  .expect("enabled split destination entity resolves",
          identical(enabled$destination_entity[enabled$posting_line_type_id == "MFF_SPLIT_NEW_CC"], "MFF_SPLIT_CC"))

  cat("\n[ rule-shape coverage ]\n")
  investigation <- .evaluate_row(
    .make_row(activity_type = "Investigation"),
    rules,
    scenario_id = "A",
    mff_rate = 1.08,
    mff_split_enabled = TRUE,
    mff_split_pct = 0.25
  )
  .expect("investigation MFF factor derives as 1.2",
          .near(investigation$posting_amount[investigation$posting_line_type_id == "MFF_SPLIT_NEW_CC"], 53 * 1.2 * 0.08 * 0.25))

  setup_close <- .evaluate_row(
    .make_row(calc_tag = "SETUP_CLOSE_DEPARTMENTAL"),
    rules,
    scenario_id = "A",
    mff_rate = 1.08,
    mff_split_enabled = TRUE,
    mff_split_pct = 0.25
  )
  .expect("setup-close MFF factor derives as 1.0",
          .near(setup_close$posting_amount[setup_close$posting_line_type_id == "MFF_SPLIT_NEW_CC"], 53 * 1.0 * 0.08 * 0.25))

  trd_medic <- .evaluate_row(
    .make_row(scenario_id = "B", staff_role = "Medical Staff"),
    rules,
    scenario_id = "B",
    mff_rate = 1.08,
    mff_split_enabled = TRUE,
    mff_split_pct = 0.25
  )
  .expect("TRD medic baseline derives factor 1.9",
          .near(trd_medic$posting_amount[trd_medic$posting_line_type_id == "MFF_SPLIT_NEW_CC"], 53 * 1.9 * 0.08 * 0.25))
  .expect("TRD medic uses 40/60 direct split",
          setequal(trd_medic$posting_line_type_id,
                   c("DIRECT_40_PI", "DIRECT_60_TEAM", "CAPACITY_RD", "INDIRECT_50_DELIVERY", "INDIRECT_25_TRUST", "INDIRECT_25_PI", "MFF_SPLIT_NEW_CC")))

  zero_pct <- .evaluate_row(
    .make_row(),
    rules,
    scenario_id = "A",
    mff_rate = 1.08,
    mff_split_enabled = TRUE,
    mff_split_pct = 0
  )
  .expect("enabled with zero pct behaves like disabled",
          identical(zero_pct$posting_line_type_id, disabled$posting_line_type_id) &&
            .near(sum(zero_pct$posting_amount), sum(disabled$posting_amount)))

  .expect_error("invalid split pct > 1 errors",
                .evaluate_row(.make_row(), rules, scenario_id = "A",
                              mff_rate = 1.08, mff_split_enabled = TRUE, mff_split_pct = 1.2))
  .expect_error("invalid split pct < 0 errors",
                .evaluate_row(.make_row(), rules, scenario_id = "A",
                              mff_rate = 1.08, mff_split_enabled = TRUE, mff_split_pct = -0.1))

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .passed, "    FAILED: ", .failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")
  invisible(list(passed = .passed, failed = .failed))
}
