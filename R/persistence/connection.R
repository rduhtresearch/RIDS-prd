# Repository construction and the transitional accessor.
#
# build_repositories(con) wires every repository against one connection.
# rids_repos() is the transitional seam for code that still relies on the
# global CON (legacy pattern): it lazily builds — and caches — the repository
# set against whatever CON currently is, rebuilding if CON changes (tests
# swap CON per suite). It disappears once services receive repositories by
# injection and the global CON is retired.

build_repositories <- function(con) {
  list(
    settings = settings_repository(con),
    app_logs = app_log_repository(con),
    credentials = api_credential_repository(con),
    users = user_repository(con),
    sessions = session_repository(con),
    auth_audit = auth_audit_repository(con),
    studies = study_repository(con),
    ict_costing = ict_costing_repository(con),
    posting_lines = posting_line_repository(con),
    rules = rules_repository(con),
    specialities = speciality_repository(con)
  )
}

# Run fn against a short-lived read-only connection to the given database
# file. Used by the posting engine, which deliberately reads rules/costs on
# its own read-only connection rather than the app's main one. The DuckDB
# specifics of acquiring that connection live here, not in domain code.
with_read_connection <- function(db_path, fn) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  fn(con)
}

.rids_repo_cache <- new.env(parent = emptyenv())

rids_repos <- function() {
  con <- get("CON", envir = .GlobalEnv, inherits = FALSE)
  cached_con <- .rids_repo_cache$con
  if (is.null(cached_con) || !identical(cached_con, con)) {
    .rids_repo_cache$con <- con
    .rids_repo_cache$repos <- build_repositories(con)
  }
  .rids_repo_cache$repos
}
