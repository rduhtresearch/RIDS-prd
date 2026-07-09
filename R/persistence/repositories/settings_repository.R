# app_settings repository. All SQL for the app_settings table lives here.

settings_repository <- function(con) {
  list(
    find_value = function(key) {
      row <- rids_dbGetQuery(
        con,
        "SELECT value FROM app_settings WHERE key = ? LIMIT 1",
        params = list(key)
      )
      if (nrow(row) == 0) character(0) else row$value[[1]]
    },

    set = function(key, value) {
      existing <- rids_dbGetQuery(
        con,
        "SELECT COUNT(*) AS n FROM app_settings WHERE key = ?",
        params = list(key)
      )$n[[1]]

      if (existing > 0) {
        rids_dbExecute(
          con,
          "UPDATE app_settings SET value = ? WHERE key = ?",
          params = list(as.character(value), key)
        )
      } else {
        rids_dbExecute(
          con,
          "INSERT INTO app_settings (key, value) VALUES (?, ?)",
          params = list(key, as.character(value))
        )
      }
      invisible(TRUE)
    }
  )
}
