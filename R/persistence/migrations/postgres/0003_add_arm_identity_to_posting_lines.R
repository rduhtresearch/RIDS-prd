migrate <- function(con) {
  fields <- tolower(DBI::dbListFields(con, "posting_lines"))
  if (!"arm_identity" %in% fields) {
    DBI::dbExecute(con, "ALTER TABLE posting_lines ADD COLUMN Arm_Identity VARCHAR")
    DBI::dbExecute(con, "UPDATE posting_lines SET Arm_Identity = Study_Arm WHERE Arm_Identity IS NULL")
  }
}
