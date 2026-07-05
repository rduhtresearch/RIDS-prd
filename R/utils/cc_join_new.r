library(tidyverse)
library(writexl)

# Speciality cost centre
speciality_code = '58109'

# File paths
#df <- read_csv('/Users/tategraham/Downloads/Book7(Sheet1).csv')
#df <- read_csv("/Users/tategraham/Downloads/Book8(Sheet1)-2.csv")
#df <- read_csv('/Users/tategraham/Downloads/Book10(Sheet1).csv')
#df <- read_csv('/Users/tategraham/Downloads/Book10(Sheet1)-2.csv')
df <- read_csv("/Users/tategraham/Downloads/Book10(Sheet1)-3.csv")


#posting_lines <- read_csv("/Users/tategraham/Downloads/75743_RDUHT_test_posting_lines_20260601_163338.csv")
#posting_lines <- read_csv('/Users/tategraham/Downloads/66795_RDUHT_Gusto_posting_lines_20260602_104238(in).csv')
#posting_lines <- read_csv("/Users/tategraham/Downloads/66795_NDDHT_t_posting_lines_20260603_155312.csv")
#posting_lines <- read_csv("/Users/tategraham/Downloads/58449_NDDHT_test5_posting_lines_20260604_150208.csv")
#posting_lines <- read_csv("/Users/tategraham/Downloads/58449_RDUHT_test_posting_lines_20260604_160630.csv")
#posting_lines <- read_csv("/Users/tategraham/Downloads/58449_RDUHT_test_posting_lines_20260604_161726.csv")
posting_lines <- read_csv("/Users/tategraham/Downloads/60454_RDUHT_MK-4482_posting_lines_20260605_122051.csv")



# CAPACITY_RD
# DIRECT
# INDIRECT_25_PI
# INDIRECT_25_TRUST
# INDIRECT_50_DELIVERY

# Rename matrix import columns
df <- df |> rename(
  'DIRECT'               = DIRECT_COST,
  'INDIRECT_25_TRUST'    = `INDIRECT_25 [O/Hs]`,
  'INDIRECT_25_PI'       = `INDIRECT_25 [PI CB]`,
  'INDIRECT_50_DELIVERY' = INDIRECT_50,
  'CAPACITY_RD'          = RD_CAPACITY,
  'DIRECT_40_PI'         = TRD40,
  'DIRECT_60_TEAM'       = TRD60
)


# Transpose data to create row per split type
df_long <- df %>%
  mutate(across(
    c(DIRECT, INDIRECT_50_DELIVERY, INDIRECT_25_TRUST, INDIRECT_25_PI, CAPACITY_RD, DIRECT_40_PI, DIRECT_60_TEAM),
    as.character
  )) %>%
  pivot_longer(
    cols      = c(DIRECT, INDIRECT_50_DELIVERY, INDIRECT_25_TRUST, INDIRECT_25_PI, CAPACITY_RD, DIRECT_40_PI, DIRECT_60_TEAM),
    names_to  = "cost_type",
    values_to = "cost_code"
  ) %>%
  filter(!is.na(cost_code), cost_code != "")



# // Column Mapping
# left = df_long, right = posting_lines
# Department      -> Department
# 'Activity Type' -> activity_type
# 'Staff Role'    -> Staff_Roles
# cost_type       -> posting_line_type_id

# Join cost codes from matrix
posting_lines <- posting_lines %>%
  left_join(
    df_long %>%
      filter(!Notes %in% c("Training Fee", "Inflight Training Fee")) %>%
      distinct(Department, `Activity Type`, `Staff Role`, cost_type, cost_code) %>%
      rename(mapped_cost_code = cost_code),
    by = c(
      "Department"           = "Department",
      "activity_type"        = "Activity Type",
      "Staff_Role"           = "Staff Role",
      "posting_line_type_id" = "cost_type"
    )
  ) %>%
  mutate(mapped_cost_code = case_when(
    mapped_cost_code == "Speciality" ~ speciality_code,
    is.na(mapped_cost_code)          ~ "no_match",
    TRUE                             ~ mapped_cost_code
  )) 

View(posting_lines)

# 1. Read the original CSV file
# Replace 'my_data.csv' with your actual file path
data <- read.csv("/Users/tategraham/Downloads/COST_CENTRE_TESTING_csv.csv", stringsAsFactors = FALSE)

# 2. Write to XLSX format
write_xlsx(posting_lines, "/Users/tategraham/Downloads/040626_COST_CENTRE_TESTING_xlsx.xlsx")
write.csv(posting_lines, "/Users/tategraham/Downloads/new_matrix_COST_CENTRE_TESTING_csv.csv", row.names = FALSE)



a <- posting_lines |> filter(mapped_cost_code == "no_match")
View(a)
















# posting_lines <- posting_lines %>%
#   left_join(
#     df_long %>%
#       filter(!Notes %in% c("Training Fee", "Inflight Training Fee")) %>%
#       distinct(Department, `Activity Type`, `Staff Role`, cost_type, cost_code) %>%
#       rename(mapped_cost_code = cost_code),
#     by = c(
#       "Department"           = "Department",
#       "activity_type"        = "Activity Type",
#       "Staff_Role"           = "Staff Role",
#       "posting_line_type_id" = "cost_type"
#     )
#   ) %>%
#   mutate(mapped_cost_code = replace_na(mapped_cost_code, "no_match"))




