# Container entry point. Schema migrations run during app startup
# (global.R -> db_main() -> run_migrations), so no separate migrate step is
# required; a failed migration fails the boot loudly.

host <- Sys.getenv("RIDS_APP_HOST", "0.0.0.0")
port <- suppressWarnings(as.integer(Sys.getenv("RIDS_APP_PORT", "3838")))
if (is.na(port) || port <= 0) {
  port <- 3838L
}

message("Starting RIDS on ", host, ":", port,
        " (storage mode: ", Sys.getenv("RIDS_STORAGE_MODE", "duckdb"), ")")

shiny::runApp(
  appDir = "/app",
  host = host,
  port = port,
  launch.browser = FALSE
)
