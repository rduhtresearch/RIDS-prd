# specialities repository. All SQL for the specialities lookup lives here.
# (Seeding lives in R/setup.r's specialities_table().)

speciality_repository <- function(con) {
  list(
    list_active = function() {
      rids_dbGetQuery(con, "
        SELECT id, name
        FROM specialities
        WHERE archived_at IS NULL
        ORDER BY name
      ")
    }
  )
}
