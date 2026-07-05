# extract CMPS ID from the raw ICT and add to system variables -
# this is used downstream in step 2 to select ict costing data for modification

extract_cpms_id <- function(path) {
  df <- read.xlsx(path, sheet = 1, rows = 1:2, colNames = FALSE)
  study_id <- df[1, 2]
  return(study_id)
}



