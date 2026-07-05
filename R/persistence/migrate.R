# Versioned schema migrations.
#
# Migrations live in R/persistence/migrations/<dialect>/ as ordered files:
#   NNNN_description.sql  — one or more SQL statements separated by ';'
#   NNNN_description.R    — must define migrate(con); used where a step needs
#                           introspection or conditional logic
#
# Applied versions are tracked in schema_migrations(version, applied_at);
# each pending migration runs inside a transaction. The initial migration is
# written with IF NOT EXISTS guards so a pre-migration-era database adopts
# versioning without being modified destructively.

rids_migrations_dir <- function(dialect = "duckdb") {
  override <- trimws(Sys.getenv("RIDS_MIGRATIONS_DIR", ""))
  if (nzchar(override)) {
    return(file.path(override, dialect))
  }
  file.path("R", "persistence", "migrations", dialect)
}

rids_split_sql_statements <- function(sql_text) {
  chunks <- strsplit(paste(sql_text, collapse = "\n"), ";", fixed = TRUE)[[1]]
  statements <- character()
  for (chunk in chunks) {
    lines <- strsplit(chunk, "\n", fixed = TRUE)[[1]]
    meaningful <- lines[!grepl("^\\s*(--.*)?$", lines)]
    if (length(meaningful) > 0) {
      statements <- c(statements, chunk)
    }
  }
  statements
}

rids_applied_migrations <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS schema_migrations (
      version    TEXT PRIMARY KEY,
      applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
  ")
  DBI::dbGetQuery(con, "SELECT version FROM schema_migrations")$version
}

run_migrations <- function(con, dialect = "duckdb", migrations_dir = rids_migrations_dir(dialect)) {
  if (!dir.exists(migrations_dir)) {
    stop("Migrations directory not found: ", migrations_dir,
         " (working directory: ", getwd(), ")")
  }

  files <- sort(list.files(migrations_dir, pattern = "^[0-9]{4}_.*\\.(sql|R)$", full.names = TRUE))
  if (length(files) == 0) {
    stop("No migration files found in: ", migrations_dir)
  }

  applied <- rids_applied_migrations(con)
  ran <- character()

  for (path in files) {
    version <- sub("\\.(sql|R)$", "", basename(path))
    if (version %in% applied) {
      next
    }

    DBI::dbWithTransaction(con, {
      if (grepl("\\.sql$", path)) {
        for (statement in rids_split_sql_statements(readLines(path, warn = FALSE))) {
          DBI::dbExecute(con, statement)
        }
      } else {
        migration_env <- new.env(parent = globalenv())
        sys.source(path, envir = migration_env)
        if (!is.function(migration_env$migrate)) {
          stop("R migration must define migrate(con): ", path)
        }
        migration_env$migrate(con)
      }

      DBI::dbExecute(
        con,
        "INSERT INTO schema_migrations (version) VALUES (?)",
        params = list(version)
      )
    })

    ran <- c(ran, version)
  }

  invisible(ran)
}
