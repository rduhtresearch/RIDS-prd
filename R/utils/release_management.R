`%||%` <- get0("%||%", ifnotfound = function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(y)
  }

  x
})

required_app_files <- function() {
  c(
    "app.R",
    "global.R",
    "R/setup.r",
    "R/utils/deployment_config.R",
    "R/utils/auth.r",
    "R/utils/logging.R"
  )
}

ensure_required_app_files <- function(app_dir, required_files = required_app_files()) {
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  missing_files <- required_files[!file.exists(file.path(app_dir, required_files))]

  if (length(missing_files) > 0) {
    stop(
      "The app folder is missing required files: ",
      paste(missing_files, collapse = ", "),
      "."
    )
  }

  invisible(TRUE)
}

run_system_command <- function(command, args = character(), workdir = getwd()) {
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(workdir)

  output <- system2(command, args = args, stdout = TRUE, stderr = TRUE)

  status <- attr(output, "status") %||% 0L
  if (!identical(status, 0L)) {
    stop(
      sprintf(
        "Command failed: %s %s\n%s",
        command,
        paste(args, collapse = " "),
        paste(output, collapse = "\n")
      )
    )
  }

  output
}

copy_directory_contents <- function(from_dir, to_dir) {
  dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)

  entries <- list.files(from_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (length(entries) == 0) {
    return(invisible(to_dir))
  }

  ok <- file.copy(entries, to_dir, recursive = TRUE, overwrite = TRUE, copy.mode = TRUE)
  if (!all(ok)) {
    failed <- basename(entries[!ok])
    stop("Failed to copy release files: ", paste(failed, collapse = ", "))
  }

  invisible(to_dir)
}

export_working_tree_snapshot <- function(repo_dir, target_dir, overwrite = FALSE) {
  repo_dir <- normalizePath(repo_dir, winslash = "/", mustWork = TRUE)
  target_dir <- normalizePath(target_dir, winslash = "/", mustWork = FALSE)

  if (dir.exists(target_dir)) {
    existing_entries <- list.files(target_dir, all.files = TRUE, no.. = TRUE)
    if (length(existing_entries) > 0 && !isTRUE(overwrite)) {
      stop("Release folder already exists and is not empty: ", target_dir)
    }

    if (isTRUE(overwrite)) {
      unlink(target_dir, recursive = TRUE, force = TRUE)
    }
  }

  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  required_entries <- c("app.R", "global.R", "R", "www")
  optional_entries <- c("config.R")
  keep_entries <- c(required_entries, optional_entries[file.exists(file.path(repo_dir, optional_entries))])
  missing_entries <- required_entries[!file.exists(file.path(repo_dir, required_entries))]
  if (length(missing_entries) > 0) {
    stop(
      "Cannot bootstrap a release from the working tree because required files are missing: ",
      paste(missing_entries, collapse = ", ")
    )
  }

  for (entry in keep_entries) {
    source_path <- file.path(repo_dir, entry)
    target_path <- file.path(target_dir, entry)

    if (dir.exists(source_path)) {
      copy_directory_contents(source_path, target_path)
    } else {
      dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
      ok <- file.copy(source_path, target_path, overwrite = TRUE, copy.mode = TRUE)
      if (!isTRUE(ok)) {
        stop("Failed to copy release file: ", entry)
      }
    }
  }

  ensure_required_app_files(target_dir)
  invisible(target_dir)
}

default_bootstrap_release_version <- function() {
  "v1.0.0"
}

read_release_pointer <- function(path, default = "") {
  if (!file.exists(path)) {
    return(default)
  }

  value <- trimws(readLines(path, warn = FALSE, n = 1L))
  if (!nzchar(value)) {
    return(default)
  }

  value
}

write_release_pointer <- function(path, version) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(version), path, useBytes = TRUE)
  invisible(path)
}

append_deploy_log <- function(log_path, action, version, status, message = "") {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

  if (!file.exists(log_path)) {
    header <- paste(
      c("timestamp", "operator", "action", "version", "status", "message"),
      collapse = "\t"
    )
    writeLines(header, log_path, useBytes = TRUE)
  }

  operator <- Sys.info()[["user"]] %||% Sys.getenv("USERNAME", unset = "unknown")
  fields <- c(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    operator,
    action,
    version,
    status,
    gsub("[\r\n\t]", " ", as.character(message %||% ""))
  )

  write(fields, file = log_path, append = TRUE, sep = "\t", ncolumns = length(fields))
  invisible(log_path)
}

run_release_smoke_check <- function(release_dir, config_path) {
  release_dir <- normalizePath(release_dir, winslash = "/", mustWork = TRUE)
  config_path <- normalizePath(config_path, winslash = "/", mustWork = FALSE)

  ensure_required_app_files(release_dir)

  if (!file.exists(config_path)) {
    stop("Shared deployment config was not found: ", config_path)
  }

  original_wd <- getwd()
  original_config <- Sys.getenv("RIDS_CONFIG_PATH", unset = "")
  on.exit({
    setwd(original_wd)
    Sys.setenv(RIDS_CONFIG_PATH = original_config)
  }, add = TRUE)

  setwd(release_dir)
  Sys.setenv(RIDS_CONFIG_PATH = config_path)

  smoke_env <- new.env(parent = baseenv())
  sys.source("R/utils/deployment_config.R", envir = smoke_env)
  smoke_env$load_runtime_config(release_dir)
  sys.source("R/setup.r", envir = smoke_env)

  invisible(TRUE)
}
