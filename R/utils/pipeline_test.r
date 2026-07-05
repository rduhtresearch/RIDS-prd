setwd('/Users/tategraham/Documents/NHS/RIDS-v1')
source("R/utils/add_study_arm.r")
source("R/utils/pipeline_fixed.r")
source("R/utils/posting_test.r")
source("R/utils/template_build_main.r")
source("R/utils/adjust.r")
source("R/utils/build_template.r")
source("R/utils/posting_lines.r")

# # Pipeline call sequence:
# posting_lines_adjusted <- adjust_posting_lines(out)
# edge_templates         <- build_all_edge_templates(posting_lines_adjusted)

t <- read.xlsx('/Users/tategraham/Downloads/(67274) - C4771002 - RDUH iCT - Final to agree to CTA.xlsx')
y <- read.xlsx('/Users/tategraham/Documents/NHS/R scripts/Refactor/testing_data/candy study.xlsx')
View(y)
View(t)

# input_file <- '/Users/tategraham/Downloads/(59904) - AriBio - POLARIS-AD - Final to agree to CTA.xlsx'
input_file <- '/Users/tategraham/Documents/NHS/(60681) - ASPIRE - RDUH iCT - downloaded 09Jun2025 - final to agree to CTA.xlsx'
# input_file <- '/Users/tategraham/Documents/NHS/R scripts/Refactor/testing_data/candy study.xlsx'

pw <- process_workbook(
  input_path = input_file,
  db_path    = DB_DIR
)

a <- prepare_posting_input(
  ict = pw,
  scenario_id = "A",
  ict_db_path   = DB_DIR
)

View(a)

a_f <- a |> filter(row_category != "BASELINE" & sheet_name == "Setup & Closedown")
View(a_f)

b <- evaluate_posting_plan(
  prepared_df = a,
  rules_db_path = DB_DIR,
  scenario_id = "A",
)

setwd('/Users/tategraham/Documents/NHS/R scripts')
write.csv(b, "rules_test.csv", row.names = FALSE)

View(b)

f_b <- b |> filter(sheet_name == "Setup & Closedown" & row_category != "BASELINE")
View(f_b)
filter_b <- b |> filter(row_category != "BASELINE")
View(filter_b)


# 
# View(pw$`AR1001 Treatment phase`)
# 
# out <- generate_posting_plan(
#   ict           = pw,
#   rules_db_path = DB_DIR,
#   scenario_id   = "A",
#   ict_db_path   = DB_DIR
# )
# 
# typeof(out)
# class(out)
# View(out)


posting_lines_adjusted <- adjust_posting_lines(b)
View(posting_lines_adjusted)

setwd('/Users/tategraham/Documents/NHS')
# df is your dataframe
write.csv(posting_lines_adjusted, "aspire_out.csv", row.names = FALSE)

message(paste(names(posting_lines_adjusted), collapse = ", "))
message(paste(sapply(posting_lines_adjusted, class), collapse = ", "))

c <- posting_lines_adjusted |> filter(sheet_name == "AR1001 Treatment phase")
View(c)
View(posting_lines_adjusted)
tm <- build_all_edge_templates(posting_lines_adjusted)
View(tm$`AR1001 Treatment phase `)
View(tm$Pharmacy)
View(tm$`AR1001 Treatment phase `)

con <- DBI::dbConnect(duckdb::duckdb(), "~/nhs_finance_app_data/RIDS.duckdb")
posting_lines <- DBI::dbGetQuery(con, "SELECT * FROM posting_lines") |> filter(study_name == "Candy Study")
View(posting_lines)
identical(posting_lines, posting_lines_adjusted)

View(posting_lines_adjusted)

View(pw$`Unscheduled Activities`)


