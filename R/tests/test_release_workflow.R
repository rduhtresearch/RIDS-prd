# ==============================================================================
# tests/test_release_workflow.R
#
# Focused regression tests for the working-tree-only release workflow.
# ======================================================================

.release_passed <- 0L
.release_failed <- 0L

.release_expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .release_passed <<- .release_passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .release_failed <<- .release_failed + 1L
  }
}

run_release_workflow_tests <- function() {
  cat("\n=== release workflow tests ===\n\n")
  .release_passed <<- 0L
  .release_failed <<- 0L

  repo_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  rscript_path <- Sys.which("Rscript")
  if (!nzchar(rscript_path)) {
    stop("Rscript was not found. These release workflow tests require Rscript.")
  }

  temp_root <- tempfile("rids_release_workflow_")
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)

  temp_repo <- file.path(temp_root, "repo")
  export_working_tree_snapshot(repo_dir, temp_repo, overwrite = TRUE)

  shared_dir <- file.path(temp_repo, "shared")
  releases_dir <- file.path(temp_repo, "releases")
  config_path <- file.path(shared_dir, "deployment_config.R")
  current_release_path <- file.path(shared_dir, "current_release.txt")
  script_path <- file.path(temp_repo, "R", "SETUP", "release_publish.R")

  dir.create(shared_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(releases_dir, recursive = TRUE, showWarnings = FALSE)

  config <- list(
    storage_mode = "duckdb",
    db_dir = normalizePath(file.path(shared_dir, "data", "RIDS.duckdb"), winslash = "/", mustWork = FALSE),
    ict_upload_dir = normalizePath(file.path(shared_dir, "uploads"), winslash = "/", mustWork = FALSE),
    edge_output_dir = normalizePath(file.path(shared_dir, "outputs"), winslash = "/", mustWork = FALSE),
    credential_secret = paste(rep("release-workflow-secret", 2), collapse = "-"),
    app_log_dir = normalizePath(file.path(shared_dir, "logs"), winslash = "/", mustWork = FALSE),
    app_host = "127.0.0.1",
    app_port = 3838L
  )

  dir.create(dirname(config$db_dir), recursive = TRUE, showWarnings = FALSE)
  dir.create(config$ict_upload_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$edge_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$app_log_dir, recursive = TRUE, showWarnings = FALSE)
  write_deployment_config(config_path, config)

  run_release_command <- function(args) {
    original_wd <- getwd()
    on.exit(setwd(original_wd), add = TRUE)
    setwd(temp_repo)

    output <- suppressWarnings(system2(
      rscript_path,
      args = c(script_path, args),
      stdout = TRUE,
      stderr = TRUE
    ))

    list(
      status = attr(output, "status") %||% 0L,
      output = output
    )
  }

  cat("[ publish mode rejected ]\n")
  publish_result <- run_release_command(c("publish", "--version", "v0.5.0"))
  publish_output <- paste(publish_result$output, collapse = "\n")
  .release_expect("publish mode exits non-zero", publish_result$status != 0L)
  .release_expect(
    "publish mode explains manual labels",
    grepl("manual labels, not Git tags", publish_output, fixed = TRUE)
  )

  cat("\n[ publish-local and rollback ]\n")
  v041_result <- run_release_command(c("publish-local", "--version", "v0.4.1"))
  v050_result <- run_release_command(c("publish-local", "--version", "v0.5.0"))
  rollback_result <- run_release_command(c("rollback", "--version", "v0.4.1"))

  .release_expect("publish-local v0.4.1 succeeds", identical(v041_result$status, 0L))
  .release_expect("publish-local v0.5.0 succeeds", identical(v050_result$status, 0L))
  .release_expect("rollback succeeds", identical(rollback_result$status, 0L))
  .release_expect("v0.4.1 release folder created", dir.exists(file.path(releases_dir, "v0.4.1")))
  .release_expect("v0.5.0 release folder created", dir.exists(file.path(releases_dir, "v0.5.0")))
  .release_expect(
    "rollback repoints current release",
    identical(read_release_pointer(current_release_path), "v0.4.1")
  )

  deploy_log_path <- file.path(shared_dir, "deploy_log.tsv")
  deploy_log <- if (file.exists(deploy_log_path)) readLines(deploy_log_path, warn = FALSE) else character()
  .release_expect(
    "deploy log records working-tree publish",
    any(grepl("publish-local\tv0.5.0\tsuccess", deploy_log, fixed = TRUE))
  )
  .release_expect(
    "deploy log records rollback",
    any(grepl("rollback\tv0.4.1\tsuccess", deploy_log, fixed = TRUE))
  )

  cat("\n[ local cached release sync ]\n")
  local_cache_root <- file.path(temp_root, "local-cache")
  v041_sync <- sync_release_to_local_cache(
    releases_dir = releases_dir,
    current_release = "v0.4.1",
    local_cache_root = local_cache_root
  )
  v041_sync_again <- sync_release_to_local_cache(
    releases_dir = releases_dir,
    current_release = "v0.4.1",
    local_cache_root = local_cache_root
  )
  v050_sync <- sync_release_to_local_cache(
    releases_dir = releases_dir,
    current_release = "v0.5.0",
    local_cache_root = local_cache_root
  )
  rollback_sync <- sync_release_to_local_cache(
    releases_dir = releases_dir,
    current_release = "v0.4.1",
    local_cache_root = local_cache_root
  )

  .release_expect("initial local sync copies v0.4.1", isTRUE(v041_sync$synced))
  .release_expect("warm local sync skips recopy", !isTRUE(v041_sync_again$synced))
  .release_expect("upgrade sync copies v0.5.0", isTRUE(v050_sync$synced))
  .release_expect("rollback reuses cached v0.4.1", !isTRUE(rollback_sync$synced))
  .release_expect("v0.4.1 cached locally", dir.exists(file.path(local_cache_root, "releases", "v0.4.1")))
  .release_expect("v0.5.0 cached locally", dir.exists(file.path(local_cache_root, "releases", "v0.5.0")))

  cat("\n[ forced republish refreshes matching cache ]\n")
  app_path <- file.path(temp_repo, "app.R")
  app_lines <- readLines(app_path, warn = FALSE)
  app_lines <- c(app_lines, "", "# release workflow refresh marker")
  writeLines(app_lines, app_path, useBytes = TRUE)

  refresh_result <- run_release_command(c("publish-local", "--version", "v0.5.0", "--force"))
  refreshed_sync <- sync_release_to_local_cache(
    releases_dir = releases_dir,
    current_release = "v0.5.0",
    local_cache_root = local_cache_root
  )
  refreshed_app_lines <- readLines(file.path(refreshed_sync$app_dir, "app.R"), warn = FALSE)

  .release_expect("forced publish-local v0.5.0 succeeds", identical(refresh_result$status, 0L))
  .release_expect("forced republish refreshes local cache", isTRUE(refreshed_sync$synced))
  .release_expect(
    "refreshed local cache includes rebuilt release contents",
    any(grepl("release workflow refresh marker", refreshed_app_lines, fixed = TRUE))
  )

  cat("\n[ invalid release does not replace good cache ]\n")
  broken_release_dir <- file.path(releases_dir, "v9.9.9")
  dir.create(broken_release_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(releases_dir, "v0.4.1", "global.R"), file.path(broken_release_dir, "global.R"), overwrite = TRUE)
  dir.create(file.path(broken_release_dir, "R"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(broken_release_dir, "www"), recursive = TRUE, showWarnings = FALSE)

  broken_sync_error <- tryCatch({
    sync_release_to_local_cache(
      releases_dir = releases_dir,
      current_release = "v9.9.9",
      local_cache_root = local_cache_root
    )
    NULL
  }, error = function(e) e)

  .release_expect("broken release sync fails", inherits(broken_sync_error, "error"))
  .release_expect(
    "good cached release remains available after failed sync",
    dir.exists(file.path(local_cache_root, "releases", "v0.5.0"))
  )

  cat("\n[ generated launchers use active release directly ]\n")
  launcher_path <- file.path(temp_root, "launch_app.R")
  prepare_path <- file.path(temp_root, "prepare_app.R")
  write_launcher_r_script(
    path = launcher_path,
    releases_dir = releases_dir,
    current_release_path = current_release_path,
    config_path = config_path,
    app_host = "127.0.0.1",
    app_port = 3838L
  )
  write_prepare_r_script(
    path = prepare_path,
    releases_dir = releases_dir,
    current_release_path = current_release_path
  )

  launcher_lines <- readLines(launcher_path, warn = FALSE)
  prepare_lines <- readLines(prepare_path, warn = FALSE)
  .release_expect(
    "launcher script does not sync a local cache",
    !any(grepl("sync_release_to_local_cache", launcher_lines, fixed = TRUE))
  )
  .release_expect(
    "prepare script does not sync a local cache",
    !any(grepl("sync_release_to_local_cache", prepare_lines, fixed = TRUE))
  )
  .release_expect(
    "launcher script resolves the active release folder directly",
    any(grepl("app_dir <- file.path\\(releases_dir, current_release\\)", launcher_lines))
  )
  .release_expect(
    "prepare script resolves the active release folder directly",
    any(grepl("app_dir <- file.path\\(releases_dir, current_release\\)", prepare_lines))
  )

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .release_passed, "    FAILED: ", .release_failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  invisible(list(passed = .release_passed, failed = .release_failed))
}
