# dependencies.R
required_packages <- c(
  "DBI",
  "duckdb",
  "sodium",
  "shiny",
  "bs4Dash",
  "waiter",
  "shinyFeedback",
  "shinyjs",
  "reactable",
  "DT",
  "jsonlite",
  "scales",
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "readr",
  "openxlsx",
  "zip"
)

ensure_user_library <- function() {
  default_lib <- normalizePath(.libPaths()[1], winslash = "/", mustWork = FALSE)
  default_writable <- dir.exists(default_lib) && file.access(default_lib, 2) == 0

  if (isTRUE(default_writable)) {
    return(invisible(default_lib))
  }

  user_lib <- Sys.getenv("R_LIBS_USER", unset = "")
  if (!nzchar(user_lib)) {
    user_lib <- path.expand("~/R/win-library")
  }

  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(unique(c(normalizePath(user_lib, winslash = "/", mustWork = FALSE), .libPaths())))
  invisible(user_lib)
}

ensure_user_library()

missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  message("All packages already installed.")
}
