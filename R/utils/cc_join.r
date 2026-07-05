library(dplyr)

df <- read_csv('/Users/tategraham/Documents/NHS/R scripts/rules_test.csv')
View(df)

add_cost_centres <- function(posting_output) {
  posting_output %>%
    mutate(
      cost_centre = case_when(
        posting_line_type_id == "CAPACITY_RD" ~ "52207",
        posting_line_type_id == "INDIRECT_25_TRUST" ~ "52206",
        posting_line_type_id == "INDIRECT_50_DELIVERY" ~ "50007",
        TRUE ~ NA_character_
      )
    )
}

test <- add_cost_centres(df)

View(test)









library(dplyr)
library(tibble)
library(stringr)

department_cc_lookup <- tribble(
  ~Department,                      ~department_cost_centre,
  "CRDT Research nurse",            "50007",
  "CRF",                            "50112",
  "Cardiovascular",                 "03634",
  "Cherry Brook",                   "04134",
  "Clinical Chemistry",             "05644",
  "CFR",                            "50112",
  "Dietician",                      "23834",
  "Gastroenterology",               "15039",
  "Haematology",                    "06244",
  "Imaging Centre",                 "61105",
  "Microbiology",                   "06044",
  "Genetics",                       "02334",
  "Nuclear Medicine",               "04735",
  "Opthalmology",                   "13355",
  "Pathology",                      "05944",
  "Radiology",                      "06734",
  "Respritory",                     "22864",
  "Immunology Or Virology",         "06044",
  "Radiotherapy",                   "04734",
  "Inverventional Radiology",       "06739",
  "Dental Radiology",               "38495"
) %>%
  mutate(
    Department_clean = str_squish(Department)
  )

speciality_cc_lookup <- tribble(
  ~speciality,                    ~speciality_cost_centre,
  "Cardiology",                   "52000",
  "Paediatric",                   "53100",
  "Cancer",                       "58109",
  "Orthopedics & Rheumatology",   "59400",
  "Gastro",                       "61105",
  "Respiratory",                  "62100",
  "Renal",                        "63106",
  "Urology",                      "64100",
  "Stroke",                       "66206",
  "Geriatric",                    "66300",
  "Dendron",                      "67103",
  "Dermatology",                  "67302",
  "ED",                           "69101"
) %>%
  mutate(
    speciality_clean = str_squish(speciality)
  )

add_cost_centres <- function(posting_output, study_speciality) {
  
  posting_output %>%
    mutate(
      Department_clean = stringr::str_squish(Department),
      Staff_Role_clean = stringr::str_squish(`Staff.Role`),
      speciality_clean = stringr::str_squish(study_speciality)
    ) %>%
    left_join(
      department_cc_lookup %>%
        select(Department_clean, department_cost_centre),
      by = "Department_clean"
    ) %>%
    left_join(
      speciality_cc_lookup %>%
        select(speciality_clean, speciality_cost_centre),
      by = "speciality_clean"
    ) %>%
    mutate(
      cost_centre = case_when(
        posting_line_type_id == "CAPACITY_RD" ~ "50000",
        posting_line_type_id == "INDIRECT_25_TRUST" ~ "50000",
        posting_line_type_id == "INDIRECT_50_DELIVERY" ~ "50007",
        
        posting_line_type_id == "DIRECT" &
          Staff_Role_clean %in% c("Admin/Data Entry", "Nursing/Manager") &
          Department_clean == "Study Team" ~ "50007",
        
        posting_line_type_id == "DIRECT" &
          Staff_Role_clean == "Medical Staff" ~ department_cost_centre,
        
        posting_line_type_id == "INDIRECT_25_PI" ~ speciality_cost_centre,
        
        TRUE ~ NA_character_
      )
    )
}


df <- read_csv('/Users/tategraham/Documents/NHS/R scripts/rules_test.csv')
test <- add_cost_centres(df, 'Cardiology')

setwd('/Users/tategraham/Documents/NHS')
write.csv(test, "cost_centre_test2.csv", row.names = FALSE)
View(test)

setwd('/Users/tategraham/Documents/NHS')
write.csv(posting_lines_adjusted, "cost_centre_test.csv", row.names = FALSE)

test2 <- test |> filter(row_category == "SETUP_CLOSE_DEPARTMENTAL")
View(test2)

View(test)