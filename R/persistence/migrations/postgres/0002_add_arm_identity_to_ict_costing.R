migrate <- function(con) {
  fields <- tolower(DBI::dbListFields(con, "ict_costing_tbl"))
  if (!"arm_identity" %in% fields) {
    DBI::dbExecute(con, "ALTER TABLE ict_costing_tbl ADD COLUMN Arm_Identity VARCHAR")
    DBI::dbExecute(con, "UPDATE ict_costing_tbl SET Arm_Identity = Study_Arm WHERE Arm_Identity IS NULL")
  }
}
