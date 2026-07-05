`%||%` <- get0("%||%", ifnotfound = function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(y)
  }

  x
})

required_app_files <- get0("required_app_files", ifnotfound = function() {
  c(
    "app.R",
    "global.R",
    "R/setup.r",
    "R/utils/deployment_config.R",
    "R/utils/auth.r",
    "R/utils/logging.R"
  )
})

ensure_required_app_files <- get0("ensure_required_app_files", ifnotfound = function(app_dir, required_files = required_app_files()) {
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
})

copy_directory_contents <- get0("copy_directory_contents", ifnotfound = function(from_dir, to_dir) {
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
})

deployment_config_candidates <- function(app_dir = getwd()) {
  env_path <- trimws(Sys.getenv("RIDS_CONFIG_PATH", ""))
  candidates <- c()

  if (nzchar(env_path)) {
    candidates <- c(candidates, env_path)
  }

  candidates <- c(
    candidates,
    file.path(app_dir, "shared", "deployment_config.R"),
    file.path(app_dir, "deployment", "deployment_config.R"),
    file.path(dirname(dirname(app_dir)), "shared", "deployment_config.R"),
    file.path(app_dir, "config.R")
  )

  unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
}

read_runtime_config <- function(path) {
  cfg_env <- new.env(parent = baseenv())
  tryCatch(
    sys.source(path, envir = cfg_env),
    error = function(e) {
      file_lines <- tryCatch(
        readLines(path, warn = FALSE),
        error = function(read_error) {
          sprintf("<unable to read %s: %s>", path, conditionMessage(read_error))
        }
      )

      numbered_lines <- paste(
        sprintf("%4d: %s", seq_along(file_lines), file_lines),
        collapse = "\n"
      )

      stop(
        paste0(
          "Failed to parse deployment config: ",
          normalizePath(path, winslash = "/", mustWork = FALSE),
          "\n",
          "Source contents:\n",
          numbered_lines,
          "\n",
          "Original error: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  get_value <- function(name, default = NULL) {
    if (exists(name, envir = cfg_env, inherits = FALSE)) {
      get(name, envir = cfg_env, inherits = FALSE)
    } else {
      default
    }
  }

  storage_mode <- tolower(trimws(as.character(get_value("STORAGE_MODE", "duckdb"))))
  if (!nzchar(storage_mode)) {
    storage_mode <- "duckdb"
  }

  cfg <- list(
    storage_mode = storage_mode,
    db_dir = as.character(get_value("DB_DIR", "")),
    ict_upload_dir = as.character(get_value("ICT_UPLOAD_DIR", "")),
    edge_output_dir = as.character(get_value("EDGE_OUTPUT_DIR", "")),
    credential_secret = as.character(get_value("CREDENTIAL_SECRET", "")),
    app_status = as.character(get_value("APP_STATUS", "live")),
    app_log_dir = as.character(get_value("APP_LOG_DIR", file.path(getwd(), "logs"))),
    app_host = as.character(get_value("APP_HOST", "127.0.0.1")),
    app_port = suppressWarnings(as.integer(get_value("APP_PORT", 3838L))),
    source_path = normalizePath(path, winslash = "/", mustWork = FALSE)
  )

  if (!nzchar(cfg$db_dir)) {
    stop("The deployment config is missing DB_DIR: ", path)
  }

  if (!nzchar(cfg$ict_upload_dir)) {
    stop("The deployment config is missing ICT_UPLOAD_DIR: ", path)
  }

  if (!nzchar(cfg$edge_output_dir)) {
    stop("The deployment config is missing EDGE_OUTPUT_DIR: ", path)
  }

  cfg$credential_secret <- trimws(cfg$credential_secret)
  if (!nzchar(cfg$credential_secret)) {
    stop("The deployment config is missing CREDENTIAL_SECRET: ", path)
  }

  if (nchar(cfg$credential_secret) < 16) {
    stop("The deployment config CREDENTIAL_SECRET must be at least 16 characters: ", path)
  }

  cfg$app_status <- tolower(trimws(cfg$app_status))
  if (!cfg$app_status %in% c("dev", "test", "live")) {
    cfg$app_status <- "live"
  }

  if (!nzchar(cfg$app_log_dir)) {
    cfg$app_log_dir <- file.path(getwd(), "logs")
  }

  if (is.na(cfg$app_port) || cfg$app_port <= 0) {
    cfg$app_port <- 3838L
  }

  cfg
}

load_runtime_config <- function(app_dir = getwd()) {
  candidates <- deployment_config_candidates(app_dir)

  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(read_runtime_config(candidate))
    }
  }

  stop(
    paste(
      "No deployment config found.",
      "Run R/SETUP/new_setup.R before launching the app.",
      "Looked in:",
      paste(candidates, collapse = ", ")
    )
  )
}

connect_primary_database <- function(config) {
  if (!identical(config$storage_mode, "duckdb")) {
    stop("Only DuckDB is supported in this release. Config requested: ", config$storage_mode)
  }

  open_duckdb_connection(config$db_dir)
}

open_duckdb_connection <- function(db_dir, read_only = FALSE, config = list()) {
  # Keep a stable reference to the driver object for older DuckDB R package
  # builds that can invalidate connections when the anonymous driver is GC'd.
  drv <- duckdb::duckdb(
    dbdir = db_dir,
    read_only = read_only,
    config = config
  )
  con <- DBI::dbConnect(drv)
  attr(con, "duckdb_driver") <- drv
  con
}

close_duckdb_connection <- function(con) {
  drv <- attr(con, "duckdb_driver", exact = TRUE)

  try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)

  if (!is.null(drv)) {
    try(duckdb::duckdb_shutdown(drv), silent = TRUE)
  }

  invisible(TRUE)
}

# Fold any leftover write-ahead log (DB.wal) back into the main DuckDB file.
#
# A WAL is left on disk whenever the app process is killed before DuckDB
# checkpoints (e.g. the launcher terminal is closed). Opening the file
# read-write replays the WAL, and a clean shutdown (CHECKPOINT + shutdown = TRUE)
# folds it into the main file and removes the .wal. This requires exclusive
# access, so it errors if another process still holds the database (i.e. RIDS is
# still running) rather than producing a stale, inconsistent file.
duckdb_wal_path <- function(db_path) {
  paste0(db_path, ".wal")
}

consolidate_duckdb_wal <- function(db_path) {
  con <- open_duckdb_connection(db_path, read_only = FALSE)
  on.exit(try(close_duckdb_connection(con), silent = TRUE), add = TRUE)

  DBI::dbExecute(con, "CHECKPOINT")
  invisible(TRUE)
}

write_deployment_config <- function(path, config) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  encode_r_string <- function(value) {
    value <- as.character(value %||% "")
    value <- gsub("\\\\", "/", value)
    value <- gsub('"', '\\"', value, fixed = TRUE)
    value
  }

  encode_path <- function(value) {
    value <- as.character(value %||% "")
    if (!nzchar(value)) {
      return("")
    }

    encode_r_string(normalizePath(value, winslash = "/", mustWork = FALSE))
  }

  lines <- c(
    "# Auto-generated by R/SETUP/new_setup.R — do not edit manually",
    "# Re-run R/SETUP/new_setup.R to update these values",
    paste0('STORAGE_MODE   <- "', encode_r_string(config$storage_mode), '"'),
    paste0('DB_DIR         <- "', encode_path(config$db_dir), '"'),
    paste0('ICT_UPLOAD_DIR <- "', encode_path(config$ict_upload_dir), '"'),
    paste0('EDGE_OUTPUT_DIR <- "', encode_path(config$edge_output_dir), '"'),
    paste0('CREDENTIAL_SECRET <- "', encode_r_string(config$credential_secret), '"'),
    paste0('APP_STATUS     <- "', encode_r_string(config$app_status %||% "live"), '"'),
    paste0('APP_LOG_DIR    <- "', encode_path(config$app_log_dir), '"'),
    paste0('APP_HOST       <- "', encode_r_string(config$app_host), '"'),
    paste0("APP_PORT       <- ", as.integer(config$app_port))
  )

  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

local_release_cache_root <- function(root = NULL) {
  root <- trimws(as.character(root %||% ""))
  if (nzchar(root)) {
    return(normalizePath(root, winslash = "/", mustWork = FALSE))
  }

  env_root <- trimws(Sys.getenv("RIDS_LOCAL_CACHE_DIR", unset = ""))
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = FALSE))
  }

  local_appdata <- trimws(Sys.getenv("LOCALAPPDATA", unset = ""))
  if (nzchar(local_appdata)) {
    return(normalizePath(file.path(local_appdata, "RIDS"), winslash = "/", mustWork = FALSE))
  }

  normalizePath(file.path(path.expand("~"), ".rids"), winslash = "/", mustWork = FALSE)
}

release_cache_marker_path <- function(release_dir) {
  file.path(release_dir, ".release_cache.dcf")
}

release_cache_signature <- function(release_dir) {
  if (!dir.exists(release_dir)) {
    return("")
  }

  files <- list.files(release_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]

  if (length(files) == 0L) {
    return("")
  }

  relative_files <- substring(files, nchar(paste0(normalizePath(release_dir, winslash = "/", mustWork = TRUE), "/")) + 1L)
  hashes <- unname(tools::md5sum(files))
  paste(paste(relative_files, hashes, sep = ":"), collapse = "|")
}

read_release_cache_marker <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }

  contents <- tryCatch(read.dcf(path), error = function(e) NULL)
  if (is.null(contents) || nrow(contents) == 0L) {
    return(list())
  }

  as.list(contents[1, , drop = TRUE])
}

write_release_cache_marker <- function(path, release, source_dir, source_signature) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  marker <- data.frame(
    release = as.character(release),
    source_dir = normalizePath(source_dir, winslash = "/", mustWork = FALSE),
    source_signature = as.character(source_signature),
    synced_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors = FALSE
  )

  write.dcf(marker, file = path)
  invisible(path)
}

prune_local_release_cache <- function(local_releases_dir, current_release, keep_previous = 1L) {
  if (!dir.exists(local_releases_dir)) {
    return(invisible(character()))
  }

  keep_previous <- max(0L, as.integer(keep_previous %||% 0L))
  entries <- list.dirs(local_releases_dir, recursive = FALSE, full.names = TRUE)
  entries <- entries[dir.exists(entries)]

  if (length(entries) == 0L) {
    return(invisible(character()))
  }

  current_dir <- normalizePath(file.path(local_releases_dir, current_release), winslash = "/", mustWork = FALSE)
  entries <- unique(normalizePath(entries, winslash = "/", mustWork = FALSE))
  entries <- entries[entries != current_dir]

  if (length(entries) <= keep_previous) {
    return(invisible(character()))
  }

  info <- file.info(entries)
  ordered_entries <- entries[order(info$mtime, decreasing = TRUE, na.last = TRUE)]
  to_remove <- ordered_entries[seq.int(keep_previous + 1L, length(ordered_entries))]

  removed <- character()
  for (entry in to_remove) {
    unlink(entry, recursive = TRUE, force = TRUE)
    if (!dir.exists(entry)) {
      removed <- c(removed, entry)
    }
  }

  invisible(removed)
}

sync_release_to_local_cache <- function(releases_dir,
                                        current_release,
                                        local_cache_root = NULL,
                                        keep_previous = 1L) {
  releases_dir <- normalizePath(releases_dir, winslash = "/", mustWork = TRUE)
  current_release <- trimws(as.character(current_release %||% ""))

  if (!nzchar(current_release)) {
    stop("A current release is required to sync the local cache.")
  }

  source_release_dir <- file.path(releases_dir, current_release)
  if (!dir.exists(source_release_dir)) {
    stop("The active release folder does not exist: ", source_release_dir)
  }

  ensure_required_app_files(source_release_dir)

  cache_root <- local_release_cache_root(local_cache_root)
  local_releases_dir <- file.path(cache_root, "releases")
  target_dir <- file.path(local_releases_dir, current_release)
  marker_path <- release_cache_marker_path(target_dir)
  source_signature <- release_cache_signature(source_release_dir)
  marker <- read_release_cache_marker(marker_path)

  cache_is_ready <- dir.exists(target_dir) &&
    identical(marker$release %||% "", current_release) &&
    identical(marker$source_signature %||% "", source_signature)

  if (isTRUE(cache_is_ready)) {
    ensure_required_app_files(target_dir)
    prune_local_release_cache(local_releases_dir, current_release, keep_previous = keep_previous)

    return(list(
      app_dir = normalizePath(target_dir, winslash = "/", mustWork = TRUE),
      cache_root = cache_root,
      source_release_dir = normalizePath(source_release_dir, winslash = "/", mustWork = TRUE),
      marker_path = marker_path,
      source_signature = source_signature,
      synced = FALSE
    ))
  }

  dir.create(local_releases_dir, recursive = TRUE, showWarnings = FALSE)

  temp_dir <- file.path(
    local_releases_dir,
    sprintf("%s-sync-%s-%s", current_release, Sys.getpid(), format(Sys.time(), "%Y%m%d%H%M%S"))
  )
  backup_dir <- paste0(target_dir, ".backup-", Sys.getpid())

  unlink(temp_dir, recursive = TRUE, force = TRUE)
  unlink(backup_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(backup_dir, recursive = TRUE, force = TRUE), add = TRUE)

  copy_directory_contents(source_release_dir, temp_dir)
  ensure_required_app_files(temp_dir)
  write_release_cache_marker(
    path = release_cache_marker_path(temp_dir),
    release = current_release,
    source_dir = source_release_dir,
    source_signature = source_signature
  )

  if (dir.exists(target_dir)) {
    if (!file.rename(target_dir, backup_dir)) {
      stop("Failed to move the previous local cached release out of the way: ", target_dir)
    }
  }

  renamed <- file.rename(temp_dir, target_dir)
  if (!isTRUE(renamed)) {
    if (dir.exists(backup_dir) && !dir.exists(target_dir)) {
      file.rename(backup_dir, target_dir)
    }
    stop("Failed to activate the synced local cached release: ", target_dir)
  }

  unlink(backup_dir, recursive = TRUE, force = TRUE)
  ensure_required_app_files(target_dir)
  prune_local_release_cache(local_releases_dir, current_release, keep_previous = keep_previous)

  list(
    app_dir = normalizePath(target_dir, winslash = "/", mustWork = TRUE),
    cache_root = cache_root,
    source_release_dir = normalizePath(source_release_dir, winslash = "/", mustWork = TRUE),
    marker_path = release_cache_marker_path(target_dir),
    source_signature = source_signature,
    synced = TRUE
  )
}

write_launcher_r_script <- function(path,
                                    releases_dir,
                                    current_release_path,
                                    config_path,
                                    app_host,
                                    app_port) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  lines <- c(
    "# Auto-generated by R/SETUP/new_setup.R",
    sprintf('releases_dir <- "%s"', normalizePath(releases_dir, winslash = "/", mustWork = FALSE)),
    sprintf('current_release_path <- "%s"', normalizePath(current_release_path, winslash = "/", mustWork = FALSE)),
    sprintf('config_path <- "%s"', normalizePath(config_path, winslash = "/", mustWork = FALSE)),
    sprintf('app_host <- "%s"', app_host),
    sprintf("app_port <- %sL", as.integer(app_port)),
    "",
    "if (!file.exists(current_release_path)) {",
    "  stop('No active release has been published yet. Run R/SETUP/release_publish.R publish-local --version vX.Y.Z first.')",
    "}",
    "",
    "if (!file.exists(config_path)) {",
    "  stop('deployment_config.R was not found: ', config_path)",
    "}",
    "",
    "current_release <- trimws(readLines(current_release_path, warn = FALSE, n = 1L))",
    "if (!nzchar(current_release)) {",
    "  stop('current_release.txt is empty: ', current_release_path)",
    "}",
    "",
    "app_dir <- file.path(releases_dir, current_release)",
    "if (!file.exists(file.path(app_dir, 'app.R'))) {",
    "  stop('app.R was not found in the active release folder: ', app_dir)",
    "}",
    "",
    "ensure_launcher_library <- function() {",
    "  user_lib <- Sys.getenv('R_LIBS_USER', unset = '')",
    "  if (!nzchar(user_lib)) {",
    "    version_parts <- strsplit(as.character(getRversion()), '[.]')[[1]]",
    "    major_minor <- paste(version_parts[1], version_parts[2], sep = '.')",
    "    local_appdata <- Sys.getenv('LOCALAPPDATA', unset = '')",
    "    if (nzchar(local_appdata)) {",
    "      user_lib <- file.path(local_appdata, 'R', 'win-library', major_minor)",
    "    } else {",
    "      user_lib <- file.path(path.expand('~'), 'AppData', 'Local', 'R', 'win-library', major_minor)",
    "    }",
    "  }",
    "  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)",
    "  .libPaths(unique(c(normalizePath(user_lib, winslash = '/', mustWork = FALSE), .libPaths())))",
    "  invisible(user_lib)",
    "}",
    "",
    "ensure_launcher_library()",
    "Sys.setenv(RIDS_CONFIG_PATH = config_path)",
    "Sys.setenv(RIDS_APP_VERSION = current_release)",
    "release_mtime <- file.info(current_release_path)$mtime",
    "if (!is.na(release_mtime)) {",
    "  Sys.setenv(RIDS_APP_LAST_UPDATED = format(release_mtime, '%Y-%m-%d'))",
    "}",
    "suppressPackageStartupMessages(library(shiny))",
    "message('Starting RIDS ', current_release, ' on ', app_host, ':', app_port)",
    "shiny::runApp(appDir = app_dir, host = app_host, port = app_port, launch.browser = FALSE)"
  )

  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

write_prepare_r_script <- function(path,
                                   releases_dir,
                                   current_release_path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  lines <- c(
    "# Auto-generated by R/SETUP/new_setup.R",
    sprintf('releases_dir <- "%s"', normalizePath(releases_dir, winslash = "/", mustWork = FALSE)),
    sprintf('current_release_path <- "%s"', normalizePath(current_release_path, winslash = "/", mustWork = FALSE)),
    "",
    "if (!file.exists(current_release_path)) {",
    "  stop('No active release has been published yet. Run R/SETUP/release_publish.R publish-local --version vX.Y.Z first.')",
    "}",
    "",
    "current_release <- trimws(readLines(current_release_path, warn = FALSE, n = 1L))",
    "if (!nzchar(current_release)) {",
    "  stop('current_release.txt is empty: ', current_release_path)",
    "}",
    "",
    "app_dir <- file.path(releases_dir, current_release)",
    "if (!dir.exists(app_dir)) {",
    "  stop('Active release directory was not found: ', app_dir)",
    "}",
    "",
    "ensure_launcher_library <- function() {",
    "  user_lib <- Sys.getenv('R_LIBS_USER', unset = '')",
    "  if (!nzchar(user_lib)) {",
    "    version_parts <- strsplit(as.character(getRversion()), '[.]')[[1]]",
    "    major_minor <- paste(version_parts[1], version_parts[2], sep = '.')",
    "    local_appdata <- Sys.getenv('LOCALAPPDATA', unset = '')",
    "    if (nzchar(local_appdata)) {",
    "      user_lib <- file.path(local_appdata, 'R', 'win-library', major_minor)",
    "    } else {",
    "      user_lib <- file.path(path.expand('~'), 'AppData', 'Local', 'R', 'win-library', major_minor)",
    "    }",
    "  }",
    "  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)",
    "  .libPaths(unique(c(normalizePath(user_lib, winslash = '/', mustWork = FALSE), .libPaths())))",
    "  invisible(user_lib)",
    "}",
    "",
    "ensure_launcher_library()",
    "if (!file.exists(file.path(app_dir, 'R', 'dependencies.R'))) {",
    "  stop('R/dependencies.R was not found in the active release folder: ', app_dir)",
    "}",
    "message('Preparing RIDS packages for ', current_release)",
    "source(file.path(app_dir, 'R', 'dependencies.R'), local = FALSE)",
    "message('RIDS package preparation complete for ', current_release)"
  )

  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

write_launcher_bat <- function(path, launcher_r_path, app_port, wait_seconds = 120L) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  lines <- c(
    "@echo off",
    "setlocal",
    "set \"SCRIPT_DIR=%~dp0\"",
    sprintf("set \"LAUNCH_R=%s\"", normalizePath(launcher_r_path, winslash = "\\", mustWork = FALSE)),
    "set \"LAUNCH_LOG=%SCRIPT_DIR%launch_rids.log\"",
    "",
    "if not exist \"%LAUNCH_R%\" (",
    "  echo Launch script not found: %LAUNCH_R%",
    "  pause",
    "  exit /b 1",
    ")",
    "",
    "set \"RSCRIPT_EXE=\"",
    "for %%D in (\"%ProgramFiles%\\R\" \"%ProgramFiles(x86)%\\R\") do (",
    "  if exist \"%%~D\" (",
    "    for /f \"delims=\" %%V in ('dir /b /ad-h /o-n \"%%~D\\R-*\" 2^>nul') do (",
    "      if exist \"%%~D\\%%V\\bin\\Rscript.exe\" (",
    "        set \"RSCRIPT_EXE=%%~D\\%%V\\bin\\Rscript.exe\"",
    "        goto :found_rscript",
    "      )",
    "    )",
    "  )",
    ")",
    "",
    ":found_rscript",
    "if not defined RSCRIPT_EXE (",
    "  echo R was not found on this laptop.",
    "  echo Please install R and try again.",
    "  pause",
    "  exit /b 1",
    ")",
    "",
    "echo Starting RIDS... > \"%LAUNCH_LOG%\"",
    "echo Starting RIDS...",
    "echo Checking active release...",
    "echo Launch log: %LAUNCH_LOG%",
    "start \"RIDS\" /min cmd /c \"\"%RSCRIPT_EXE%\" --vanilla \"%LAUNCH_R%\" >> \"%LAUNCH_LOG%\" 2>&1\"",
    "set \"APP_READY=\"",
    "echo Waiting for RIDS to start...",
    sprintf("for /l %%%%I in (1,1,%s) do (", as.integer(wait_seconds)),
    "  if %%I==15 echo Still waiting for RIDS to respond...",
    "  if %%I==30 echo RIDS is still starting. Please wait...",
    "  if %%I==60 echo RIDS is still starting. This can take longer on some laptops...",
    "  if %%I==90 echo RIDS is taking longer than usual. Checking again...",
    sprintf("  powershell -NoProfile -Command \"try { $r = Invoke-WebRequest -UseBasicParsing -Uri 'http://127.0.0.1:%s' -TimeoutSec 2; exit 0 } catch { exit 1 }\" >nul 2>&1", as.integer(app_port)),
    "  if not errorlevel 1 (",
    "    set \"APP_READY=1\"",
    "    goto :open_browser",
    "  )",
    "  timeout /t 1 /nobreak >nul",
    ")",
    "",
    "echo RIDS did not respond on time.",
    "echo The app may still be starting or this laptop may need package preparation.",
    "echo Check the launcher log for details:",
    "echo %LAUNCH_LOG%",
    "pause",
    "exit /b 1",
    "",
    ":open_browser",
    "echo Opening browser...",
    sprintf("start \"\" \"http://127.0.0.1:%s\"", as.integer(app_port)),
    "endlocal"
  )

  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

write_prepare_bat <- function(path, prep_r_path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  lines <- c(
    "@echo off",
    "setlocal",
    "set \"SCRIPT_DIR=%~dp0\"",
    sprintf("set \"PREP_R=%s\"", normalizePath(prep_r_path, winslash = "\\", mustWork = FALSE)),
    "set \"PREP_LOG=%SCRIPT_DIR%prepare_rids.log\"",
    "",
    "if not exist \"%PREP_R%\" (",
    "  echo Preparation script not found: %PREP_R%",
    "  pause",
    "  exit /b 1",
    ")",
    "",
    "set \"RSCRIPT_EXE=\"",
    "for %%D in (\"%ProgramFiles%\\R\" \"%ProgramFiles(x86)%\\R\") do (",
    "  if exist \"%%~D\" (",
    "    for /f \"delims=\" %%V in ('dir /b /ad-h /o-n \"%%~D\\R-*\" 2^>nul') do (",
    "      if exist \"%%~D\\%%V\\bin\\Rscript.exe\" (",
    "        set \"RSCRIPT_EXE=%%~D\\%%V\\bin\\Rscript.exe\"",
    "        goto :found_rscript",
    "      )",
    "    )",
    "  )",
    ")",
    "",
    ":found_rscript",
    "if not defined RSCRIPT_EXE (",
    "  echo R was not found on this laptop.",
    "  echo Please install R and try again.",
    "  pause",
    "  exit /b 1",
    ")",
    "",
    "echo Preparing RIDS on this laptop... > \"%PREP_LOG%\"",
    "echo Preparing RIDS packages for this laptop...",
    "echo This may take several minutes the first time.",
    "echo Preparation log: %PREP_LOG%",
    "\"%RSCRIPT_EXE%\" --vanilla \"%PREP_R%\" >> \"%PREP_LOG%\" 2>&1",
    "if errorlevel 1 (",
    "  echo RIDS preparation failed.",
    "  echo Check the preparation log for details:",
    "  echo %PREP_LOG%",
    "  pause",
    "  exit /b 1",
    ")",
    "",
    "echo RIDS preparation complete.",
    "echo You can now use Launch RIDS.bat.",
    "pause",
    "endlocal"
  )

  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}
