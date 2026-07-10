migrate <- function(con) {
  fields <- tolower(DBI::dbListFields(con, "posting_lines"))
  if (!"activity_occurrence_id" %in% fields) {
    DBI::dbExecute(con, "ALTER TABLE posting_lines ADD COLUMN activity_occurrence_id INTEGER")
  }
}
