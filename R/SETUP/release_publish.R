script_path_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_path_arg) > 0) {
  normalizePath(sub("^--file=", "", script_path_arg[[1]]), winslash = "/", mustWork = TRUE)
} else {
  normalizePath("R/SETUP/release_publish.R", winslash = "/", mustWork = TRUE)
}

repo_dir <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
setwd(repo_dir)

source(file.path(repo_dir, "R/utils/deployment_config.R"))
source(file.path(repo_dir, "R/utils/release_management.R"))

usage <- function() {
  paste(
    "Usage:",
    "  Rscript R/SETUP/release_publish.R publish-local --version v0.5.0",
    "  Rscript R/SETUP/release_publish.R rollback --version v0.4.1",
    "Options:",
    "  --force    Rebuild an existing release folder during publish-local.",
    sep = "\n"
  )
}

parse_args <- function(args) {
  if (length(args) == 0) {
    stop(usage())
  }

  mode <- trimws(args[[1]])
  version <- ""
  force <- FALSE

  idx <- 2L
  while (idx <= length(args)) {
    arg <- args[[idx]]

    if (identical(arg, "--force")) {
      force <- TRUE
      idx <- idx + 1L
      next
    }

    if (startsWith(arg, "--version=")) {
      version <- sub("^--version=", "", arg)
      idx <- idx + 1L
      next
    }

    if (identical(arg, "--version")) {
      if (idx == length(args)) {
        stop("Missing value after --version.\n", usage())
      }
      version <- args[[idx + 1L]]
      idx <- idx + 2L
      next
    }

    if (!nzchar(version)) {
      version <- arg
      idx <- idx + 1L
      next
    }

    stop("Unexpected argument: ", arg, "\n", usage())
  }

  if (!mode %in% c("publish-local", "rollback")) {
    stop(
      "Mode must be 'publish-local' or 'rollback'. ",
      "Release versions are manual labels, not Git tags.\n",
      usage()
    )
  }

  version <- trimws(version)
  if (!nzchar(version)) {
    stop("A version is required.\n", usage())
  }

  list(mode = mode, version = version, force = force)
}

settings <- parse_args(commandArgs(trailingOnly = TRUE))

shared_dir <- file.path(repo_dir, "shared")
releases_dir <- file.path(repo_dir, "releases")
config_path <- file.path(shared_dir, "deployment_config.R")
current_release_path <- file.path(shared_dir, "current_release.txt")
deploy_log_path <- file.path(shared_dir, "deploy_log.tsv")

if (!file.exists(config_path)) {
  stop("Shared deployment config was not found. Run R/SETUP/new_setup.R first.")
}

dir.create(shared_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(releases_dir, recursive = TRUE, showWarnings = FALSE)

execute_publish_local <- function(version, force = FALSE) {
  export_working_tree_snapshot(
    repo_dir = repo_dir,
    target_dir = file.path(releases_dir, version),
    overwrite = force
  )

  run_release_smoke_check(file.path(releases_dir, version), config_path)
  write_release_pointer(current_release_path, version)
}

execute_rollback <- function(version) {
  release_dir <- file.path(releases_dir, version)

  if (!dir.exists(release_dir) || !file.exists(file.path(release_dir, "app.R"))) {
    stop("Cannot roll back because the release folder does not exist: ", release_dir)
  }

  write_release_pointer(current_release_path, version)
}

result <- tryCatch({
  if (identical(settings$mode, "publish-local")) {
    execute_publish_local(settings$version, force = settings$force)
  } else {
    execute_rollback(settings$version)
  }

  append_deploy_log(
    deploy_log_path,
    action = settings$mode,
    version = settings$version,
    status = "success",
    message = sprintf("%s completed successfully.", settings$mode)
  )

  message(tools::toTitleCase(settings$mode), " complete: ", settings$version)
  0L
}, error = function(e) {
  append_deploy_log(
    deploy_log_path,
    action = settings$mode,
    version = settings$version,
    status = "failed",
    message = conditionMessage(e)
  )
  message("Release operation failed: ", conditionMessage(e))
  1L
})

quit(status = result, save = "no")
