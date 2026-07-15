suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
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

.make_adjust_rows <- function(contract_cost) {
  tibble(
    row_id = c(1L, 2L),
    Activity = c("Blood Test", "Blood Test"),
    staff_group = c(1L, 1L),
    scenario_id = c("A", "A"),
    sheet_name = c("Treatment Arm", "Treatment Arm"),
    Study_Arm = c("Treatment", "Treatment"),
    Visit = c("VISIT - 001", "VISIT - 001"),
    posting_line_type_id = c("DIRECT", "INDIRECT"),
    posting_amount = c(100, 50),
    contract_cost = c(contract_cost, contract_cost)
  )
}

.make_ssp_rows <- function() {
  tibble(
    row_id = c(11L, 12L, 13L, 14L),
    Activity = c("Blood Test", "ECG", "Blood Test", "Main Visit Summary"),
    staff_group = c(1L, 1L, 2L, 1L),
    scenario_id = c("A", "A", "A", "A"),
    sheet_name = c("Treatment Arm", "Treatment Arm", "Treatment Arm", "Treatment Arm"),
    Study_Arm = c("SSP", "SSP", "SSP", "Treatment Arm"),
    Visit = c("VISIT - 001", "VISIT - 001", "VISIT - 001", "VISIT - 001"),
    Visit_Label = c("Screening", "Screening", "Screening", "Screening"),
    posting_line_type_id = c("DIRECT", "DIRECT", "DIRECT", "DIRECT"),
    posting_amount = c(10, 20, 15, 100),
    contract_cost = c(25, 40, 35, 200),
    adjusted_amount = c(25, 40, 35, 200),
    study_name = c("Study A", "Study A", "Study A", "Study A")
  )
}

.write_ua_workflow_fixture <- function(path) {
  make_arm_sheet <- function(ua_cost, include_ssp = FALSE) {
    rows <- list(
      c("Study", "Study A"),
      c("Study Id", "CP-E2E"),
      c("Activity", "Activity Type", "Department", "Cost Type", "Staff Role",
        "Time Required", "Activity Cost", "Visit One", "Total Activity Cost", "Total"),
      c("Visit Summary", "Visit", "R&D", "Research", "Nurse", "30", "100", "1", "100", "100"),
      c("Unscheduled / Itemised Activities", rep(NA_character_, 9)),
      c("Ad hoc", "Investigation", "Lab", "Research", "Nurse", "20",
        as.character(ua_cost), "1", as.character(ua_cost), as.character(ua_cost))
    )
    if (include_ssp) {
      rows <- append(rows, list(
        c("Scheduled / Some Participants", rep(NA_character_, 9)),
        c("Questionnaire", "Visit", "R&D", "Research", "Nurse", "10", "25", "1", "25", "25")
      ))
    }
    matrix_rows <- lapply(rows, function(row) {
      length(row) <- 10L
      row
    })
    as.data.frame(do.call(rbind, matrix_rows), stringsAsFactors = FALSE)
  }

  setup_sheet <- make_arm_sheet(50, FALSE)[1:4, ]
  setup_sheet[4, 1] <- "Site setup"

  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, " Arm A ")
  openxlsx::writeData(workbook, " Arm A ", make_arm_sheet(11, TRUE), colNames = FALSE)
  openxlsx::addWorksheet(workbook, "Arm B")
  openxlsx::writeData(workbook, "Arm B", make_arm_sheet(22, FALSE), colNames = FALSE)
  openxlsx::addWorksheet(workbook, "Setup & Closedown")
  openxlsx::writeData(workbook, "Setup & Closedown", setup_sheet, colNames = FALSE)
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  path
}

run_contract_cost_source_of_truth_tests <- function() {
  cat("\n=== contract cost source-of-truth tests ===\n\n")
  .passed <<- 0L
  .failed <<- 0L

  source("R/utils/posting_lines.r")
  source("R/utils/template_build_main.r")
  source("R/utils/screening_failure_transform.R")
  source("R/utils/adjust.r")
  source("R/utils/assign_edge_keys.R")
  source("R/utils/build_template.r")
  source("R/utils/pipeline_fixed.r")
  source("R/utils/add_study_arm.r")

  db_path <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit({
    dbDisconnect(con, shutdown = TRUE)
    if (file.exists(db_path)) unlink(db_path)
  }, add = TRUE)

  dbExecute(con, "
    CREATE TABLE ict_costing_tbl (
      CPMS_ID VARCHAR,
      study_site VARCHAR,
      scenario_id VARCHAR,
      Study VARCHAR,
      Visit_Number VARCHAR,
      Study_Arm VARCHAR,
      Visit_Label VARCHAR,
      Activity_Name VARCHAR,
      ICT_Cost DOUBLE,
      Contract_Cost DOUBLE,
      activity_occurrence_id VARCHAR,
      staff_group INTEGER
    )
  ")

	  dbWriteTable(
	    con,
	    "ict_costing_tbl",
    tibble(
      CPMS_ID = c("CP1", "CP1", "CP1", "CP1"),
      study_site = c("RDUHT", "RDUHT", "NDDHT", "RDUHT"),
      scenario_id = c("A", "A", "A", "B"),
      Study = c("Study A", "Study A", "Study A", "Study A"),
      Visit_Number = c("VISIT - 001", "VISIT - 002", "VISIT - 001", "VISIT - 001"),
      Study_Arm = c("Treatment", "Treatment", "Treatment", "Treatment"),
      Visit_Label = c("Screening", "Follow-up", "Screening", "Screening"),
      Activity_Name = c("Blood Test", NA_character_, "Blood Test", "Blood Test"),
      ICT_Cost = c(100.11, 200.22, 300.33, 400.44),
      Contract_Cost = c(123.45, 210.55, 333.33, 444.44),
      activity_occurrence_id = c("AO1", NA_character_, "AO2", "AO3"),
      staff_group = c(1L, 1L, 1L, 1L)
    ),
    append = TRUE
  )

  dbWriteTable(
    con,
    "ict_costing_tbl",
    tibble(
      CPMS_ID = c("CP1", "CP1"),
      study_site = c("RDUHT", "RDUHT"),
      scenario_id = c("A", "A"),
      Study = c("Study A", "Study A"),
      Visit_Number = c("VISIT - 001", "VISIT - 001"),
      Study_Arm = c("Arm A", "SSP"),
      Visit_Label = c("Screening", "Screening"),
      Activity_Name = c("Visit Summary", "Blood Test"),
      ICT_Cost = c(100, 25),
      Contract_Cost = c(2032, 25),
      activity_occurrence_id = c("ARM-A-1", "SSP-1"),
      staff_group = c(1L, 2L)
    ),
	    append = TRUE
	  )

  dbExecute(con, "ALTER TABLE ict_costing_tbl ADD COLUMN Arm_Identity VARCHAR")
  dbExecute(con, "UPDATE ict_costing_tbl SET Arm_Identity = Study_Arm")
  dbExecute(con, "UPDATE ict_costing_tbl SET Arm_Identity = 'Arm A' WHERE Study_Arm = 'SSP'")
  dbExecute(con, "
    INSERT INTO ict_costing_tbl (
      CPMS_ID, study_site, scenario_id, Study, Visit_Number, Study_Arm, Visit_Label,
      Activity_Name, ICT_Cost, Contract_Cost, activity_occurrence_id, staff_group,
      Arm_Identity
    ) VALUES
      ('CP1', 'RDUHT', 'A', 'Study A', 'VISIT - 001', 'UA', 'Unscheduled', 'Extra Bloods', 10, 111, 'UA-A-1', 1, 'Arm A'),
      ('CP1', 'RDUHT', 'A', 'Study A', 'VISIT - 001', 'UA', 'Unscheduled', 'Extra Bloods', 20, 222, 'UA-B-1', 1, 'Arm B'),
      ('CP1', 'RDUHT', 'A', 'Study A', 'VISIT - 001', 'SSP', 'Screening', 'Blood Test', 35, 35, 'SSP-B-1', 2, 'Arm B')
  ")

  cat("[ join_ict_costs ]\n")
  joined_activity <- join_ict_costs(
    tibble(
      cpms_id = "CP1",
      study_site = "RDUHT",
      scenario_id = "A",
      Visit = "VISIT - 001",
      Study_Arm = "Treatment",
      Activity = "Blood Test",
      staff_group = 1L
    ),
    db_path
  )
  .expect("activity-level join uses saved Contract_Cost",
          identical(joined_activity$contract_cost[[1]], 123.45))
  .expect("join ignores other study_site rows for same CPMS",
          !identical(joined_activity$contract_cost[[1]], 333.33))
  .expect("join ignores other scenario rows for same site and CPMS",
          !identical(joined_activity$contract_cost[[1]], 444.44))

  joined_visit <- join_ict_costs(
    tibble(
      cpms_id = "CP1",
      study_site = "RDUHT",
      scenario_id = "A",
      Visit = "VISIT - 002",
      Study_Arm = "Treatment",
      Activity = "MFF Summary",
      staff_group = 1L
    ),
    db_path
  )
  .expect("visit-level fallback uses saved Contract_Cost",
          identical(joined_visit$contract_cost[[1]], 210.55))

  joined_other_site <- join_ict_costs(
    tibble(
      cpms_id = "CP1",
      study_site = "NDDHT",
      scenario_id = "A",
      Visit = "VISIT - 001",
      Study_Arm = "Treatment",
      Activity = "Blood Test",
      staff_group = 1L
    ),
    db_path
  )
  .expect("same CPMS joins to the active site only",
          identical(joined_other_site$contract_cost[[1]], 333.33))

  joined_other_scenario <- join_ict_costs(
    tibble(
      cpms_id = "CP1",
      study_site = "RDUHT",
      scenario_id = "B",
      Visit = "VISIT - 001",
      Study_Arm = "Treatment",
      Activity = "Blood Test",
      staff_group = 1L
    ),
    db_path
  )
	  .expect("same CPMS and site joins to the active scenario only",
	          identical(joined_other_scenario$contract_cost[[1]], 444.44))

  joined_ua_arm <- join_ict_costs(
    tibble(
      cpms_id = "CP1",
      study_site = "RDUHT",
      scenario_id = "A",
      Visit = "VISIT - 001",
      Study_Arm = "UA",
      Arm_Identity = "Arm B",
      Activity = "Extra Bloods",
      staff_group = 1L
    ),
    db_path
  )
  .expect("UA contract cost join uses Arm_Identity rather than generic Study_Arm",
          identical(joined_ua_arm$contract_cost[[1]], 222))

  joined_multi_arm_ua <- join_ict_costs(
    tibble(
      cpms_id = c("CP1", "CP1"),
      study_site = c("RDUHT", "RDUHT"),
      scenario_id = c("A", "A"),
      Visit = c("VISIT - 001", "VISIT - 001"),
      Study_Arm = c("UA", "UA"),
      Arm_Identity = c("Arm A", "Arm B"),
      Activity = c("Extra Bloods", "Extra Bloods"),
      staff_group = c(1L, 1L)
    ),
    db_path
  )
  .expect("multi-arm UA cost join does not fan out posting rows",
          nrow(joined_multi_arm_ua) == 2L)
  .expect("multi-arm UA join preserves each arm's saved cost",
          identical(joined_multi_arm_ua$contract_cost, c(111, 222)))

  joined_multi_arm_ssp <- join_ict_costs(
    tibble(
      cpms_id = c("CP1", "CP1"), study_site = c("RDUHT", "RDUHT"),
      scenario_id = c("A", "A"), Visit = c("VISIT - 001", "VISIT - 001"),
      Study_Arm = c("SSP", "SSP"), Arm_Identity = c("Arm A", "Arm B"),
      Activity = c("Blood Test", "Blood Test"), staff_group = c(2L, 2L)
    ),
    db_path
  )
  .expect("SSP cost joins retain the originating parent arm without fan-out",
          nrow(joined_multi_arm_ssp) == 2L &&
            identical(joined_multi_arm_ssp$contract_cost, c(25, 35)))

  versioned_db_path <- tempfile(fileext = ".duckdb")
  versioned_con <- dbConnect(duckdb::duckdb(), dbdir = versioned_db_path)
  dbExecute(versioned_con, "
    CREATE TABLE ict_costing_tbl (
      CPMS_ID VARCHAR, study_site VARCHAR, scenario_id VARCHAR, version_id INTEGER,
      Study VARCHAR, Visit_Number VARCHAR, Study_Arm VARCHAR, Arm_Identity VARCHAR,
      Visit_Label VARCHAR, Activity_Name VARCHAR, ICT_Cost DOUBLE, Contract_Cost DOUBLE,
      activity_occurrence_id INTEGER, staff_group INTEGER
    )
  ")
  dbExecute(versioned_con, "
    INSERT INTO ict_costing_tbl VALUES
      ('CPV', 'SITE', 'A', 1, 'Study V', 'VISIT - 001', 'UA', 'Arm A',
       'Unscheduled', 'Ad hoc', 10, 101, 1, 1),
      ('CPV', 'SITE', 'A', 2, 'Study V', 'VISIT - 001', 'UA', 'Arm A',
       'Unscheduled', 'Ad hoc', 20, 202, 1, 1)
  ")
  dbDisconnect(versioned_con, shutdown = TRUE)

  versioned_input <- tibble(
    cpms_id = "CPV", study_site = "SITE", scenario_id = "A", version_id = 2L,
    Visit = "VISIT - 001", Study_Arm = "UA", Arm_Identity = "Arm A",
    Activity = "Ad hoc", staff_group = 1L
  )
  versioned_join <- join_ict_costs(versioned_input, versioned_db_path)
  .expect("versioned cost join reads only the selected template version",
          nrow(versioned_join) == 1L && identical(versioned_join$contract_cost[[1]], 202))

  versioned_con <- dbConnect(duckdb::duckdb(), dbdir = versioned_db_path)
  dbExecute(versioned_con, "
    INSERT INTO ict_costing_tbl VALUES
      ('CPV', 'SITE', 'A', 2, 'Study V', 'VISIT - 001', 'UA', 'Arm A',
       'Unscheduled', 'Ad hoc', 30, 203, 2, 1)
  ")
  dbDisconnect(versioned_con, shutdown = TRUE)

  repeated_occurrence_join <- join_ict_costs(
    bind_rows(
      versioned_input %>% mutate(activity_occurrence_id = 1L),
      versioned_input %>% mutate(activity_occurrence_id = 2L)
    ),
    versioned_db_path
  )
  .expect("repeated activity occurrences join one-to-one using occurrence ID",
          nrow(repeated_occurrence_join) == 2L &&
            identical(repeated_occurrence_join$contract_cost, c(202, 203)))

  mixed_occurrence_join <- join_ict_costs(
    bind_rows(
      repeated_occurrence_join %>% select(-contract_cost),
      versioned_input %>% mutate(Activity = "MFF Summary", activity_occurrence_id = NA_integer_)
    ),
    versioned_db_path
  )
  .expect("rows without occurrence IDs do not disable occurrence matching for activity rows",
          nrow(mixed_occurrence_join) == 3L &&
            identical(mixed_occurrence_join$contract_cost[1:2], c(202, 203)))

  duplicate_join_error <- tryCatch(
    join_ict_costs(versioned_input, versioned_db_path),
    error = identity
  )
  .expect("duplicate cost keys are rejected before they fan out posting rows",
          inherits(duplicate_join_error, "error") &&
            grepl("would fan out", conditionMessage(duplicate_join_error)))
  unlink(versioned_db_path)

  cat("\n[ persist_ict_to_duckdb ]\n")
  scoped_db_path <- tempfile(fileext = ".duckdb")
  scoped_con <- dbConnect(duckdb::duckdb(), dbdir = scoped_db_path)
  on.exit({
    if (inherits(scoped_con, "DBIConnection")) {
      dbDisconnect(scoped_con, shutdown = TRUE)
    }
    if (file.exists(scoped_db_path)) unlink(scoped_db_path)
  }, add = TRUE)

  dbExecute(scoped_con, "
    CREATE TABLE ict_costing_tbl (
      CPMS_ID VARCHAR,
      study_site VARCHAR,
      scenario_id VARCHAR,
      Study VARCHAR,
      Visit_Number VARCHAR,
      Study_Arm VARCHAR,
      Visit_Label VARCHAR,
      Activity_Name VARCHAR,
      ICT_Cost DOUBLE,
      Contract_Cost DOUBLE,
      activity_occurrence_id VARCHAR,
      staff_group INTEGER
    )
  ")
  dbDisconnect(scoped_con, shutdown = TRUE)

  persist_ict_to_duckdb(
    scoped_db_path,
    tibble(
      CPMS_ID = "CP1",
      study_site = "RDUHT",
      scenario_id = "A",
      Study = "Study A",
      Visit_Number = "VISIT - 001",
      Study_Arm = "Treatment",
      Visit_Label = "Screening",
      Activity_Name = "Blood Test",
      ICT_Cost = 100.11,
      activity_occurrence_id = "AO1",
      staff_group = 1L
    )
  )

  persist_ict_to_duckdb(
    scoped_db_path,
    tibble(
      CPMS_ID = c("CP1", "CP1"),
      study_site = c("NDDHT", "RDUHT"),
      scenario_id = c("A", "B"),
      Study = c("Study A", "Study A"),
      Visit_Number = c("VISIT - 001", "VISIT - 001"),
      Study_Arm = c("Treatment", "Treatment"),
      Visit_Label = c("Screening", "Screening"),
      Activity_Name = c("Blood Test", "Blood Test"),
      ICT_Cost = c(300.33, 400.44),
      activity_occurrence_id = c("AO2", "AO3"),
      staff_group = c(1L, 1L)
    )
  )

  scoped_con <- dbConnect(duckdb::duckdb(), dbdir = scoped_db_path, read_only = TRUE)
  persisted_rows <- dbGetQuery(
    scoped_con,
    "SELECT CPMS_ID, study_site, scenario_id FROM ict_costing_tbl ORDER BY study_site, scenario_id"
  )
  dbDisconnect(scoped_con, shutdown = TRUE)
  scoped_con <- NULL

  .expect("scoped persistence keeps separate site/scenario rows for same CPMS",
          nrow(persisted_rows) == 3L)
  .expect("saving one site does not delete the other site",
          any(persisted_rows$study_site == "NDDHT" & persisted_rows$scenario_id == "A"))
  .expect("saving one scenario does not delete the other scenario",
          any(persisted_rows$study_site == "RDUHT" & persisted_rows$scenario_id == "A") &&
            any(persisted_rows$study_site == "RDUHT" & persisted_rows$scenario_id == "B"))

  cat("\n[ adjust_posting_lines ]\n")
  adjusted_exact <- adjust_posting_lines(.make_adjust_rows(123.45))
  .expect("saved pence value is preserved as contract target",
          identical(adjusted_exact$contract_price[[1]], 123.45))
  .expect("adjusted totals reconcile to saved pence value",
          identical(round(sum(adjusted_exact$adjusted_amount), 2), 123.45))
  .expect("residual stays on the DIRECT row",
          isTRUE(adjusted_exact$is_residual_row[[1]]) &&
            isFALSE(adjusted_exact$is_residual_row[[2]]))

  adjusted_rounded <- adjust_posting_lines(.make_adjust_rows(123))
  .expect("whole-pound saved values remain whole-pound totals",
          identical(round(sum(adjusted_rounded$adjusted_amount), 2), 123))

  missing_cost_error <- tryCatch(
    adjust_posting_lines(.make_adjust_rows(NA_real_)),
    error = identity
  )
  .expect("missing contract costs stop Step 4 before zero-value templates are built",
          inherits(missing_cost_error, "error") &&
            grepl("did not match a saved Step 2 contract cost", conditionMessage(missing_cost_error)))

  adjusted_ssp <- adjust_posting_lines(.make_ssp_rows())
  ssp_rows <- adjusted_ssp %>% filter(Study_Arm == "SSP") %>% arrange(row_id)
  .expect("SSP rows keep separate saved contract costs",
          isTRUE(all.equal(unname(ssp_rows$contract_price), c(25, 40, 35))))
  .expect("SSP rows adjust to their own saved totals",
          isTRUE(all.equal(unname(round(ssp_rows$adjusted_amount, 2)), c(25, 40, 35))))

  ua_adjust_input <- tibble(
    row_id = c(21L, 22L),
    Activity = c("Extra Bloods", "Extra Bloods"),
    staff_group = c(1L, 1L),
    scenario_id = c("A", "A"),
    sheet_name = c("Unscheduled Activities", "Unscheduled Activities"),
    Study_Arm = c("UA", "UA"),
    Arm_Identity = c("Arm A", "Arm B"),
    Visit = c("VISIT - 001", "VISIT - 001"),
    posting_line_type_id = c("DIRECT", "DIRECT"),
    posting_amount = c(10, 10),
    contract_cost = c(111.11, 222)
  )
  adjusted_ua <- adjust_posting_lines(ua_adjust_input) %>% arrange(Arm_Identity)
  .expect("UA adjustments retain separate arm-specific pence targets",
          identical(adjusted_ua$contract_price, c(111.11, 222)))
  .expect("UA rows adjust independently for each arm",
          identical(adjusted_ua$adjusted_amount, c(111.11, 222)))

  generic_ua_error <- tryCatch(
    adjust_posting_lines(ua_adjust_input %>% mutate(Arm_Identity = "UA")),
    error = identity
  )
  .expect("generic UA identities are rejected with a reprocessing instruction",
          inherits(generic_ua_error, "error") &&
            grepl("source-arm identity is missing", conditionMessage(generic_ua_error)))

  sc_adjusted <- adjust_posting_lines(tibble(
    row_id = c(31L, 31L),
    Activity = c("Site setup", "Site setup"),
    staff_group = c(1L, 1L),
    scenario_id = c("A", "A"),
    sheet_name = c("Setup & Closedown", "Setup & Closedown"),
    Study_Arm = c("SC", "SC"),
    Arm_Identity = c("Arm A", "Arm B"),
    Visit = c("VISIT - 001", "VISIT - 001"),
    posting_line_type_id = c("DIRECT", "INDIRECT"),
    posting_amount = c(60, 40),
    contract_cost = c(150, 150)
  ))
  .expect("Setup & Closedown remains one activity adjustment group",
          identical(round(sum(sc_adjusted$adjusted_amount), 2), 150))

  cat("\n[ workbook arm identity ]\n")
  special_lookup_input <- tibble(
    Activity = c("Extra Bloods", "Site setup"),
    Activity.Cost = c(10, 20),
    `Visit One` = c(1, 1),
    Total.Activity.Cost = c(10, 20),
    Total = c(10, 20),
    Flag = c("Unscheduled / Itemised Activities", "Setup & Closedown"),
    SheetName = c("Arm A", "Arm A"),
    staff_group = c(1L, 1L)
  )
  special_lookup <- build_ua_ssp_lookup_from_sheet(
    special_lookup_input, "Study A", "CP1"
  ) %>% arrange(Study_Arm)
  .expect("ingestion uses source identity for UA and canonical identity for SC",
          identical(special_lookup$Arm_Identity, c("SC", "Arm A")))
  .expect("ingestion keeps generic UA and SC routing arms",
          identical(special_lookup$Study_Arm, c("SC", "UA")))

  trimmed_arms <- add_study_arm(list(
    ` Treatment Arm ` = tibble(
      Flag = c("Scheduled / All Participants", "Scheduled / Some Participants"),
      SheetName = c(" Treatment Arm ", " Treatment Arm ")
    )
  ))[[1]]
  .expect("study-arm assignment trims source sheet names",
          identical(trimmed_arms$Study_Arm, c("Treatment Arm", "SSP")))

  cat("\n[ end-to-end workbook workflow ]\n")
  fixture_path <- .write_ua_workflow_fixture(tempfile(fileext = ".xlsx"))
  fixture_db <- tempfile(fileext = ".duckdb")
  fixture_con <- dbConnect(duckdb::duckdb(), dbdir = fixture_db)
  dbExecute(fixture_con, "
    CREATE TABLE ict_costing_tbl (
      CPMS_ID VARCHAR, study_site VARCHAR, scenario_id VARCHAR, version_id INTEGER,
      Study VARCHAR, Visit_Number VARCHAR, Study_Arm VARCHAR, Arm_Identity VARCHAR,
      Visit_Label VARCHAR, Activity_Name VARCHAR, ICT_Cost DOUBLE, Contract_Cost DOUBLE,
      activity_occurrence_id INTEGER, staff_group INTEGER
    )
  ")
  dbDisconnect(fixture_con, shutdown = TRUE)

  if (!exists("app_log_info", mode = "function")) {
    assign("app_log_info", function(...) invisible(NULL), envir = .GlobalEnv)
  }
  fixture_processed <- process_workbook(
    fixture_path, db_path = fixture_db, study_site = "SITE", scenario_id = "A", version_id = 1L
  )
  fixture_con <- dbConnect(duckdb::duckdb(), dbdir = fixture_db)
  fixture_repo <- ict_costing_repository(fixture_con)
  fixture_costs <- fixture_repo$find_by_run("CP-E2E", "SITE", "A", 1L)
  fixture_costs$Contract_Cost <- fixture_costs$ICT_Cost
  fixture_repo$replace_run(fixture_costs, "CP-E2E", "SITE", "A", 1L)
  dbDisconnect(fixture_con, shutdown = TRUE)

  fixture_prepared <- prepare_posting_input(
    fixture_processed, scenario_id = "A", ict_db_path = fixture_db
  ) %>%
    filter(Study_Arm %in% c("UA", "SC", "SSP"))
  fixture_postings <- fixture_prepared %>%
    mutate(
      posting_line_type_id = "DIRECT",
      posting_amount = as.numeric(Activity.Cost),
      destination_bucket = "DEST_RD",
      destination_entity = "R&D",
      cost_code = NA_character_
    )
  fixture_adjusted <- assign_edge_keys(adjust_posting_lines(fixture_postings))
  fixture_templates <- build_all_edge_templates(
    fixture_adjusted,
    visit_lookup = fixture_costs %>%
      distinct(Study, Study_Arm, Visit_Label, Visit_Number),
    edge_id = "EDGE-E2E"
  )
  .expect("workbook workflow exports one UA template per source arm with exact costs",
          identical(fixture_templates[["UA - Arm A"]]$`Default Cost`, 11) &&
            identical(fixture_templates[["UA - Arm B"]]$`Default Cost`, 22))
  .expect("workbook workflow retains the non-zero Setup cost",
          identical(fixture_templates[["Setup & Closedown"]]$`Default Cost`, 100))
  .expect("workbook workflow routes SSP into its trimmed parent arm",
          "Arm A" %in% names(fixture_templates) &&
            !("SSP" %in% names(fixture_templates)) &&
            identical(fixture_templates[["Arm A"]]$`Default Cost`, 25))
  unlink(c(fixture_path, fixture_db))

  cat("\n[ template_build_main alignment ]\n")
  adjusted_template <- adjust_postings(.make_adjust_rows(123.45), c("Study_Arm", "Visit", "scenario_id"))
  .expect("template_build_main uses exact saved contract cost",
          identical(adjusted_template$contract_price[[1]], 123.45))
  .expect("template_build_main totals reconcile to saved pence value",
          identical(round(sum(adjusted_template$adjusted_amount), 2), 123.45))

  cat("\n[ active EDGE template build ]\n")

  ua_keyed <- assign_edge_keys(adjusted_ua %>% mutate(
    Department = "Pathology",
    `Staff.Role` = "Nurse",
    study_name = "Study A",
    cpms_id = "CP1"
  ))
  ua_template <- build_all_edge_templates(
    ua_keyed,
    visit_lookup = tibble(
      Study = "Study A",
      Study_Arm = "Arm A",
      Visit_Label = "Screening",
      Visit_Number = "VISIT - 001"
    ),
    edge_id = "EDGE-PROJ-1"
  )
  .expect("UA Step 4 templates retain distinct arm-specific costs",
          identical(ua_template[["UA - Arm A"]]$`Default Cost`, 111.11) &&
            identical(ua_template[["UA - Arm B"]]$`Default Cost`, 222))

  sc_keyed <- assign_edge_keys(sc_adjusted %>% mutate(
    Department = "Research & Development",
    `Staff.Role` = "Administrator",
    study_name = "Study A",
    cpms_id = "CP1"
  ))
  sc_template <- build_all_edge_templates(
    sc_keyed,
    visit_lookup = tibble(
      Study = "Study A",
      Study_Arm = "SC",
      Visit_Label = "Setup",
      Visit_Number = "VISIT - 001"
    ),
    edge_id = "EDGE-PROJ-1"
  )
  .expect("Setup & Closedown template retains its saved default cost",
          identical(sc_template[["Setup & Closedown"]]$`Default Cost`, 150))

  ssp_keyed <- assign_edge_keys(adjusted_ssp)
  ssp_template <- build_all_edge_templates(
    ssp_keyed,
    visit_lookup = tibble(
      Study = "Study A",
      Study_Arm = c("SSP", "Treatment Arm"),
      Visit_Label = c("VISIT - 001 - Screening", "Screening"),
      Visit_Number = c("VISIT - 001", "VISIT - 001")
    ),
    edge_id = "EDGE-PROJ-1"
  )
  treatment_rows_built <- ssp_template[["Treatment Arm"]] %>% arrange(`Cost Item Description`)
  ssp_item_rows_built <- treatment_rows_built %>% filter(grepl("Blood Test|ECG", `Cost Item Description`))
  scheduled_rows_built <- treatment_rows_built %>% filter(!grepl("Blood Test|ECG", `Cost Item Description`))

  .expect("SSP repeated items on the same visit get separate EDGE keys",
          dplyr::n_distinct(ssp_keyed$edge_key[ssp_keyed$Study_Arm == "SSP"]) == 3L)
  .expect("SSP rows merge into the main arm template",
          !("SSP" %in% names(ssp_template)) && "Treatment Arm" %in% names(ssp_template))
  .expect("main arm template shows one row per source SSP item",
          nrow(ssp_item_rows_built) == 3L)
  .expect("merged SSP rows include the item names",
          all(grepl("Blood Test|ECG", ssp_item_rows_built$`Cost Item Description`)))
  .expect("main arm SSP rows retain their exact adjusted default costs",
          identical(ssp_item_rows_built$`Default Cost`, c(25, 35, 40)))
  .expect("main arm SSP descriptions are visibly prefixed",
          all(grepl("^\\[SSP\\] ", ssp_item_rows_built$`Cost Item Description`)))
  .expect("SSP rows do not duplicate the visit prefix",
          !any(grepl("^VISIT - 001 - VISIT - 001", ssp_item_rows_built$`Cost Item Description`)))
  .expect("non-SSP scheduled rows still roll up by visit",
          nrow(scheduled_rows_built) == 1L &&
            identical(scheduled_rows_built$`Default Cost`[[1]], 200))

  standalone_ssp_error <- tryCatch(
    build_all_edge_templates(
      assign_edge_keys(.make_ssp_rows() %>%
        filter(Study_Arm == "SSP") %>%
        mutate(sheet_name = " SSP ")),
      visit_lookup = tibble(
        Study = "Study A", Study_Arm = "SSP", Visit_Label = "Screening",
        Visit_Number = "VISIT - 001"
      ),
      edge_id = "EDGE-PROJ-1"
    ),
    error = identity
  )
  .expect("standalone SSP templates are rejected after name normalization",
          inherits(standalone_ssp_error, "error") &&
            grepl("standalone SSP template is not valid", conditionMessage(standalone_ssp_error)))
  .expect("main arm rows do not repeat a visit-only label",
          identical(
            build_edge_template_main(tibble(
              study_name = "Study A",
              Visit = "VISIT - 001",
              Study_Arm = "Treatment Arm",
              Visit_Label = "VISIT - 001",
              adjusted_amount = 50,
              sheet_name = "Treatment Arm",
              edge_key = "EDGE-0001"
            ))$`Cost Item Description`,
            "VISIT - 001"
          ))
  lookup_recovery_template <- build_all_edge_templates(
    assign_edge_keys(adjust_posting_lines(tibble(
      row_id = 1L,
      Activity = "Visit Summary",
      staff_group = 1L,
      scenario_id = "A",
      sheet_name = "Treatment Arm",
      Study_Arm = "Treatment Arm",
      Visit = "VISIT - 001",
      Visit_Label = "VISIT - 001",
      posting_line_type_id = "DIRECT",
      posting_amount = 100,
      contract_cost = 100,
      adjusted_amount = 100,
      study_name = "Study A"
    ))),
    visit_lookup = tibble(
      Study = "Study A",
      Study_Arm = "Treatment Arm",
      Visit_Label = "Screening",
      Visit_Number = "VISIT - 001"
    ),
    edge_id = "EDGE-PROJ-1"
  )
  .expect("main arm rows recover visit label from lookup when posting data only has visit number",
          identical(
            lookup_recovery_template[["Treatment Arm"]]$`Cost Item Description`,
            "VISIT - 001 - Screening"
          ))
  renumber_input <- tibble(
    row_id = c(1L, 2L, 3L, 4L, 5L),
    Activity = c("Visit Summary", "Visit Summary", "Visit Summary", "Questionnaire", "ECG"),
    staff_group = c(1L, 1L, 1L, 1L, 1L),
    scenario_id = c("A", "A", "A", "A", "A"),
    sheet_name = c("Treatment Arm", "Treatment Arm", "Treatment Arm", "Treatment Arm", "Treatment Arm"),
    Study_Arm = c("Treatment Arm", "Treatment Arm", "Treatment Arm", "SSP", "SSP"),
    Visit = c("VISIT - 001", "VISIT - 004", "VISIT - 006", "VISIT - 002", "VISIT - 003"),
    Visit_Label = c("Screening", "Week 12", "Week 48", "Week 4", "Week 8"),
    posting_line_type_id = c("DIRECT", "DIRECT", "DIRECT", "DIRECT", "DIRECT"),
    posting_amount = c(100, 120, 140, 20, 30),
    contract_cost = c(100, 120, 140, 20, 30),
    adjusted_amount = c(100, 120, 140, 20, 30),
    study_name = c("Study A", "Study A", "Study A", "Study A", "Study A")
  )
  renumber_template <- build_all_edge_templates(
    assign_edge_keys(adjust_posting_lines(renumber_input)),
    visit_lookup = tibble(
      Study = c("Study A", "Study A", "Study A", "Study A", "Study A"),
      Study_Arm = c("Treatment Arm", "Treatment Arm", "Treatment Arm", "SSP", "SSP"),
      Visit_Label = c("Screening", "Week 12", "Week 48", "Week 4", "Week 8"),
      Visit_Number = c("VISIT - 001", "VISIT - 004", "VISIT - 006", "VISIT - 002", "VISIT - 003")
    ),
    edge_id = "EDGE-PROJ-1"
  )
	  .expect("scheduled main visits are renumbered densely before SSP rows",
	          identical(
	            renumber_template[["Treatment Arm"]]$`Cost Item Description`,
	            c(
	              "VISIT - 001 - Screening",
	              "VISIT - 002 - Week 12",
	              "VISIT - 003 - Week 48",
	              "[SSP] VISIT - 002 - Week 4 - Questionnaire",
	              "[SSP] VISIT - 003 - Week 8 - ECG"
	            )
	          ))

  ua_templates <- build_all_edge_templates(
    tibble(
      row_id = c(1L, 2L),
      scenario_id = c("A", "A"),
      sheet_name = c("Unscheduled Activities", "Unscheduled Activities"),
      Study_Arm = c("UA", "UA"),
      Arm_Identity = c("Arm A", "Arm B"),
      Activity = c("Extra Bloods", "Extra Bloods"),
      staff_group = c(1L, 1L),
      edge_key = c("EDGE-UA-A", "EDGE-UA-B"),
      Department = c("Pathology", "Pathology"),
      Staff_Role = c("Nurse", "Medic"),
      adjusted_amount = c(111, 222),
      study_name = c("Study A", "Study A"),
      cpms_id = c("CP1", "CP1")
    ),
    visit_lookup = tibble(
      Study = "Study A",
      Study_Arm = "Arm A",
      Visit_Label = "Screening",
      Visit_Number = "VISIT - 001"
    ),
    edge_id = "EDGE-PROJ-1"
  )
  .expect("UA templates are split by Arm_Identity",
          all(c("UA - Arm A", "UA - Arm B") %in% names(ua_templates)))
  .expect("UA template descriptions include Staff_Role",
          identical(ua_templates[["UA - Arm B"]]$`Cost Item Description`, "Extra Bloods - Medic"))

  cat("\n[ screening failure duplication ]\n")
  screening_input <- tibble(
    row_id = c(1L, 2L, 3L, 4L, 5L, 6L),
    scenario_id = rep("A", 6),
    row_category_auto = rep("BASELINE", 6),
    calc_tag = NA_character_,
    row_category = rep("BASELINE", 6),
    is_medic = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
    cpms_id = rep("CP1", 6),
    study_site = rep("RDUHT", 6),
    study_name = rep("Study A", 6),
    Study_Arm = c("Arm A", "Arm A", "Arm B", "Arm B", "SSP", "SSP"),
    Activity = c("Visit Summary", "Visit Summary", "Visit Summary", "Visit Summary", "Blood Test", "Blood Test"),
    Visit = c("VISIT - 001", "VISIT - 002", "VISIT - 001", "VISIT - 003", "VISIT - 001", "VISIT - 002"),
    posting_line_type_id = rep("DIRECT", 6),
    posting_amount = c(100, 120, 200, 240, 25, 30),
    destination_bucket = rep("DEST_RD", 6),
    destination_entity = rep("R&D", 6),
    cost_code = NA_character_,
    sheet_name = c("Arm A", "Arm A", "Arm B", "Arm B", "Arm A", "Arm A"),
    Visit_Label = c("Screening", "Follow-up", "Baseline", "Visit 3", "Screening", "Follow-up"),
    activity_occurrence_id = c("AO1", "AO2", "BO1", "BO3", "SO1", "SO2"),
    staff_group = c(1L, 1L, 1L, 1L, 2L, 2L),
    contract_cost = c(100, 120, 200, 240, 25, 30),
    Department = c("Dept A", "Dept A", "Dept B", "Dept B", "Lab", "Lab"),
    Staff.Role = c("Nurse", "Nurse", "Coordinator", "Coordinator", "Medical Staff", "Medical Staff"),
    activity_type = rep("Visit", 6),
    time_required = c(30, 45, 40, 50, 20, 20)
  )

  screening_source <- list(
    `Arm A` = tibble(
      Study_Arm = c("Arm A", "Arm A", "SSP"),
      Arm_Identity = c("Arm A", "Arm A", "Arm A"),
      Visit = c("VISIT - 001", "VISIT - 002", "VISIT - 001"),
      Visit_Label = c("Screening", "Follow-up", "Screening"),
      Activity = c("Visit Summary", "Visit Summary", "Blood Test"),
      Activity.Type = c("Visit", "Visit", "Investigation"),
      Staff.Role = c("Nurse", "Nurse", "Medical Staff"),
      Activity.Cost = c("100", "120", "25"),
      study_name = c("Study A", "Study A", "Study A"),
      cpms_id = c("CP1", "CP1", "CP1"),
      study_site = c("RDUHT", "RDUHT", "RDUHT"),
      scenario_id = c("A", "A", "A"),
      staff_group = c(1L, 1L, 2L)
    ),
    `Arm B` = tibble(
      Study_Arm = c("Arm B", "Arm B"),
      Arm_Identity = c("Arm B", "Arm B"),
      Visit = c("VISIT - 001", "VISIT - 003"),
      Visit_Label = c("Baseline", "Visit 3"),
      Activity = c("Visit Summary", "Visit Summary"),
      Activity.Type = c("Visit", "Visit"),
      Staff.Role = c("Coordinator", "Coordinator"),
      Activity.Cost = c("200", "240"),
      study_name = c("Study A", "Study A"),
      cpms_id = c("CP1", "CP1"),
      study_site = c("RDUHT", "RDUHT"),
      scenario_id = c("A", "A"),
      staff_group = c(1L, 1L)
    ),
    `Unscheduled Activities` = tibble(
      Study_Arm = "UA",
      Arm_Identity = "Arm A",
      Visit = "VISIT - 001",
      Visit_Label = "Screening",
      Activity = "Ad hoc",
      Activity.Type = "Visit",
      Staff.Role = "Nurse",
      Activity.Cost = "5",
      study_name = "Study A",
      cpms_id = "CP1",
      study_site = "RDUHT",
      scenario_id = "A",
      staff_group = 1L
    )
  )

  .expect("screening arm candidates keep workbook tab order",
          identical(
            screening_failure_candidate_sheets(names(screening_source)),
            c("Arm A", "Arm B")
          ))
  .expect("screening arm resolver defaults to the first eligible main tab",
          identical(resolve_screening_failure_arm(names(screening_source)), "Arm A"))
  .expect("screening arm resolver accepts a valid manual override",
          identical(resolve_screening_failure_arm(names(screening_source), "Arm B"), "Arm B"))
  .expect("screening arm resolver ignores invalid overrides",
          identical(resolve_screening_failure_arm(names(screening_source), "Missing Arm"), "Arm A"))

  duplicated_source <- duplicate_screening_failure_sheets(
    screening_source,
    include_screening_failure = TRUE
  )
  .expect("processed ICT duplication creates one screening failure sheet for the first main arm",
          "Arm A - SCREENING FAILURE" %in% names(duplicated_source))
  .expect("processed ICT duplication does not create a screening failure sheet for later arms",
          !"Arm B - SCREENING FAILURE" %in% names(duplicated_source))
  .expect("processed ICT duplication only copies first-visit rows",
          identical(
            duplicated_source[["Arm A - SCREENING FAILURE"]]$Visit,
            c("VISIT - 001", "VISIT - 001")
          ))
  .expect("processed ICT duplication preserves per-activity source costs",
          identical(
            duplicated_source[["Arm A - SCREENING FAILURE"]]$Activity.Cost,
            c("100", "25")
          ))
  manual_override_source <- duplicate_screening_failure_sheets(
    screening_source,
    include_screening_failure = TRUE,
    screening_failure_arm = "Arm B"
  )
  .expect("processed ICT duplication supports manual arm override",
          "Arm B - SCREENING FAILURE" %in% names(manual_override_source))
  .expect("screening duplication is a no-op for an invalid arm override",
          identical(
            duplicate_screening_failure_sheets(
              screening_source,
              include_screening_failure = TRUE,
              screening_failure_arm = "Missing Arm"
            ),
            duplicated_source
          ))

  screening_prepared <- join_ict_costs(
    prepare_posting_input(
      ict = duplicated_source,
      ict_db_path = NULL,
      scenario_id = "A"
    ),
    db_path
  )
  screening_prepared <- prepare_screening_failure_posting_input(screening_prepared)

  .expect("screening prepared rows keep the saved contract targets",
          identical(
            screening_prepared %>%
              filter(sheet_name == "Arm A - SCREENING FAILURE") %>%
              arrange(Activity, staff_group) %>%
              pull(contract_cost),
            c(25, 2032)
          ))
  .expect("screening prepared rows use the synthetic sheet as the main arm identity",
          identical(
            screening_prepared %>%
              filter(sheet_name == "Arm A - SCREENING FAILURE", Study_Arm != "SSP") %>%
              pull(Study_Arm),
            "Arm A - SCREENING FAILURE"
          ))

  screening_enabled <- tibble(
    row_id = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 5L),
    scenario_id = rep("A", 8),
    row_category_auto = rep("BASELINE", 8),
    calc_tag = NA_character_,
    row_category = rep("BASELINE", 8),
    is_medic = rep(FALSE, 8),
    cpms_id = rep("CP1", 8),
    study_site = rep("RDUHT", 8),
    study_name = rep("Study A", 8),
    Study_Arm = c("Arm A", "Arm A", "Arm A", "Arm A", "Arm A", "Arm A", "Arm A", "Arm B"),
    Activity = c(
      "Informed consent", "Informed consent",
      "Informed consent", "Informed consent",
      "Demographics", "Demographics",
      "Visit Summary", "Visit Summary"
    ),
    Visit = c(
      "VISIT - 001", "VISIT - 001",
      "VISIT - 001", "VISIT - 001",
      "VISIT - 001", "VISIT - 001",
      "VISIT - 002", "VISIT - 001"
    ),
    posting_line_type_id = c(
      "DIRECT", "CAPACITY_RD",
      "DIRECT", "CAPACITY_RD",
      "DIRECT", "CAPACITY_RD",
      "DIRECT", "DIRECT"
    ),
    posting_amount = c(80, 20, 40, 10, 30, 20, 120, 200),
    destination_bucket = rep("DEST_RD", 8),
    destination_entity = rep("R&D", 8),
    cost_code = NA_character_,
    sheet_name = c(
      rep("Arm A - SCREENING FAILURE", 6),
      "Arm A",
      "Arm B"
    ),
    Visit_Label = c(
      rep("Screening", 6),
      "Follow-up",
      "Baseline"
    ),
    activity_occurrence_id = c("AO1", "AO1", "AO2", "AO2", "AO3", "AO3", "AO4", "BO1"),
    staff_group = rep(1L, 8),
    contract_cost = c(rep(2032, 6), 120, 200),
    Department = c(rep("Dept A", 7), "Dept B"),
    Staff.Role = c(rep("Nurse", 7), "Coordinator"),
    activity_type = rep("Visit", 8),
    time_required = c(30, 30, 35, 35, 15, 15, 45, 40)
  )
  screening_enabled <- prepare_screening_failure_posting_input(screening_enabled)

  screening_adjusted <- adjust_posting_lines(screening_enabled)
  screening_keyed <- assign_edge_keys(screening_adjusted)
  screening_templates <- build_all_edge_templates(
    screening_keyed,
    visit_lookup = tibble(
      Study = rep("Study A", 4),
      Study_Arm = c("Arm A", "Arm A", "Arm B", "Arm B"),
      Visit_Label = c("Screening", "Follow-up", "Baseline", "Visit 3"),
      Visit_Number = c("VISIT - 001", "VISIT - 002", "VISIT - 001", "VISIT - 003")
    ),
    edge_id = "EDGE-PROJ-1"
  )

  .expect("screening failure rows survive adjustment and EDGE key assignment",
          all(!is.na(screening_keyed$edge_key[grepl("SCREENING FAILURE$", screening_keyed$sheet_name)])))
  .expect("screening failure templates are built as ordinary main-arm tabs",
          "Arm A - SCREENING FAILURE" %in% names(screening_templates))
  .expect("later arms do not get screening failure templates",
          !"Arm B - SCREENING FAILURE" %in% names(screening_templates))
  .expect("ordinary main-arm templates still roll up by visit",
          identical(
            screening_templates[["Arm A"]]$`Cost Item Description`,
            "VISIT - 001 - Follow-up"
          ))
  .expect("screening templates are itemised per duplicated source row",
          identical(
	            screening_templates[["Arm A - SCREENING FAILURE"]]$`Cost Item Description`,
	            c(
	              "VISIT - 001 - Screening - Informed consent - Nurse",
	              "VISIT - 001 - Screening - Informed consent - Nurse",
	              "VISIT - 001 - Screening - Demographics - Nurse"
	            )
	          ))
  .expect("repeated screening source rows remain separate in the template",
	          sum(
	            screening_templates[["Arm A - SCREENING FAILURE"]]$`Cost Item Description` ==
	              "VISIT - 001 - Screening - Informed consent - Nurse"
	          ) == 2L)
  .expect("screening failure rows get distinct itemised EDGE keys",
          dplyr::n_distinct(
            screening_keyed$edge_key[screening_keyed$sheet_name == "Arm A - SCREENING FAILURE"]
          ) == 3L)
  expected_screening_costs <- screening_keyed %>%
    filter(sheet_name == "Arm A - SCREENING FAILURE") %>%
    summarise(total = sum(adjusted_amount), .by = c(row_id, Activity, staff_group, edge_key)) %>%
    arrange(row_id, staff_group) %>%
    pull(total) %>%
    (\(x) x * 0)()
  .expect("screening template costs default to zero",
          identical(
            screening_templates[["Arm A - SCREENING FAILURE"]]$`Default Cost`,
            expected_screening_costs
          ))
  .expect("screening template costs sum to zero",
          identical(
            round(sum(screening_templates[["Arm A - SCREENING FAILURE"]]$`Default Cost`), 2),
            0
          ))

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .passed, "    FAILED: ", .failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  invisible(list(passed = .passed, failed = .failed))
}
