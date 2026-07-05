# Deployment Guide

## Overview

RIDS is currently deployed with a manual process.

The release path is:

1. Make and merge a code change into `main`.
2. Pull the latest `main` into the shared-drive clone.
3. Run local R checks.
4. Publish the checked-out code as a named release.
5. User laptops sync the active release to a local cache and launch it from `C:`.
6. Launch the app and verify it.
7. Roll back to the previous release if needed.

This guide is for maintainers who manage the shared-drive deployment.

## Prerequisites

You need:

- a cloned repo on the shared drive
- `Git` installed
- `R` and `Rscript` installed
- permission to read and write the shared-drive repo

The shared-drive clone is the deployment repo. It should contain:

- `.git`
- `shared/`
- `releases/`
- `deployment/`

Example path:

```text
P:\RESEARCH SYSTEMS\RIDS_\RIDS-v1
```

User laptops keep a local cache of the active release under `%LOCALAPPDATA%\RIDS\releases\`.
The shared drive still remains the source of truth for:

- `releases/`
- `shared/current_release.txt`
- `shared/deployment_config.R`
- the DuckDB file and other runtime folders referenced by that config

## First-Time Maintainer Setup

### 1. Open the shared-drive repo

In PowerShell:

```powershell
cd "P:\RESEARCH SYSTEMS\RIDS_\RIDS-v1"
git status
```

If you see `fatal: not a git repository`, you are in the wrong folder. Move into the actual repo folder and try again.

### 2. Run setup if this machine has not been prepared yet

In `R` or `RStudio`:

```r
source("R/SETUP/new_setup.R")
```

## Daily Manual Backup

At the end of the day, from the live shared-drive RIDS folder in `R` or `RStudio`, run:

```r
source("R/SETUP/manual_backup.R")
```

What it does:

1. Reads the live deployment config and finds the production DuckDB file
2. Folds any leftover write-ahead log (`RIDS.duckdb.wal`) back into the DuckDB file so the backup is complete and consistent
3. Copies the full DuckDB file into `P:\RESEARCH SYSTEMS\RIDS_BACKUP\<timestamp>\`
4. Exports every database table to `csv\` inside the same backup folder
5. Writes a `backup_manifest.txt` file with the source DB and exported tables
6. Deletes older backup runs so only the newest 2 successful backups remain

Keep `shared/deployment_config.R` safe alongside the deployment. It contains the stable `CREDENTIAL_SECRET` used to decrypt saved user API keys after a DB restore.

Ask users to close RIDS before running the backup. Step 2 needs exclusive access to the DuckDB file; if RIDS is still running the backup stops with a clear error rather than copying stale data. (A `.wal` file is normal — it is left behind whenever the app is closed and is consolidated automatically here.)

## Manual Restore

If you need to restore a backup:

1. Ask users to close RIDS
2. Open `R/SETUP/manual_restore.R`
3. Set `RESTORE_BACKUP_TIMESTAMP <- "YYYY-MM-DD_HHMMSS"` to the backup folder you want
4. In `R` or `RStudio`, run:

```r
source("R/SETUP/manual_restore.R")
```

What it does:

1. Reads the live deployment config and finds the production DuckDB file
2. Folds any leftover write-ahead log into the current live DB so the safety copy below captures the full current state
3. Creates a safety copy of the current live DB under `P:\RESEARCH SYSTEMS\RIDS_BACKUP\pre_restore_safety\`
4. Copies the selected backup `RIDS.duckdb` into the live DB path
5. Removes any stale `RIDS.duckdb.wal` beside the live DB so the restored file is not corrupted on the next open
6. Verifies the restored file can be opened and lists its tables
7. Writes a `restore_manifest.txt` file in the safety folder

After restore, reopen RIDS and verify the data you expected to recover.
If the restored deployment keeps the same `shared/deployment_config.R`, saved user API keys will continue to work.

## Closing RIDS

On live deployments the app now shuts itself down cleanly once the last browser tab/window is closed (after a short grace period that tolerates page refreshes). When it shuts down, DuckDB checkpoints and the launcher terminal window closes on its own — users no longer need to close the terminal manually.

Notes:

- A brief delay between closing the browser and the terminal closing is expected (the grace period). It can be tuned with the `RIDS_SHUTDOWN_GRACE_SECONDS` environment variable.
- This behaviour only applies when `APP_STATUS` is `live` in `shared/deployment_config.R`. In `dev`/`test` the app stays running after the browser closes.
- **Logout** is unchanged: it returns to the login screen with the app still running, so logging back in is instant.

### 3. Prepare the Windows laptop for first use

Run the preparation launcher once on each Windows laptop:

```text
deployment\Prepare RIDS.bat
```

What it does:

1. Finds `Rscript.exe`
2. Locates the active release
3. Syncs the active release into the local cache if needed
4. Creates or reuses the user R library
5. Installs missing packages from the local cached release's `R/dependencies.R`

The first run can take several minutes.

## Standard Update Flow

### 1. Make and merge the code change

From your development machine:

```bash
git checkout -b codex/my-change
```

Make the change, then run local checks:

```bash
Rscript R/CI/run_ci_checks.R
```

Commit and push:

```bash
git add .
git commit -m "Describe the change"
git push -u origin codex/my-change
```

Open a pull request and merge it into `main`.

### 2. Update the shared-drive clone

On the maintainer machine, in the shared-drive repo:

```powershell
cd "P:\RESEARCH SYSTEMS\RIDS_\RIDS-v1"
git checkout main
git pull origin main
```

### 3. Run local checks in the shared-drive clone

```powershell
Rscript R/CI/run_ci_checks.R
```

If `Rscript` is not recognized:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "R/CI/run_ci_checks.R"
```

Do not publish if this step fails.

### 4. Publish the new live version

Choose a version name, for example `v0.5.0`.

Then run:

```powershell
Rscript R/SETUP/release_publish.R publish-local --version v0.5.0
```

Windows example with a full `Rscript.exe` path:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "R/SETUP/release_publish.R" publish-local --version v0.5.0
```

What this does:

1. Copies the current checked-out code into `releases/v0.5.0/`
2. Runs the release smoke check
3. Updates `shared/current_release.txt`
4. Writes a line to `shared/deploy_log.tsv`
5. Leaves user laptops to pick up the new release automatically on the next launch

The version name is a manual release label. It does not need to match a Git tag.

### 5. Prepare the laptop if this release adds new packages

If the release introduces new R package dependencies, rerun:

```text
deployment\Prepare RIDS.bat
```

This is also a good support step on any new Windows laptop before the user launches the app.

### 6. Verify the release

Check the active release pointer:

```powershell
Get-Content "shared\current_release.txt"
```

If needed, prepare the machine first:

```text
deployment\Prepare RIDS.bat
```

Then launch:

```text
deployment\Launch RIDS.bat
```

Confirm:

- the app opens successfully
- the visible change is present
- the footer version pill matches the release version
- `deployment\launch_rids.log` shows the release synced to the local cache when needed

## Rebuilding an Existing Release Folder

If a release folder already exists and you want to rebuild it, use `--force`:

```powershell
Rscript R/SETUP/release_publish.R publish-local --version v0.5.0 --force
```

Windows example with a full `Rscript.exe` path:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "R/SETUP/release_publish.R" publish-local --version v0.5.0 --force
```

Use this when a previous attempt created a partial release folder.

## Rollback

If a release causes problems, switch the live pointer back to an earlier release.

Run:

```powershell
Rscript R/SETUP/release_publish.R rollback --version v0.4.1
```

Windows example with a full `Rscript.exe` path:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "R/SETUP/release_publish.R" rollback --version v0.4.1
```

What rollback does:

1. Verifies `releases/v0.4.1/` exists
2. Updates `shared/current_release.txt`
3. Writes a line to `shared/deploy_log.tsv`
4. Leaves user laptops to resync that older release on the next launch

After rollback:

1. If the older release has different package requirements, run `deployment\Prepare RIDS.bat`
2. Open `deployment/Launch RIDS.bat`
3. Confirm the previous version is now live
4. Confirm the issue is no longer present

## Troubleshooting

### `fatal: not a git repository`

You are in the wrong folder.

Move into the actual shared-drive repo:

```powershell
cd "P:\RESEARCH SYSTEMS\RIDS_\RIDS-v1"
git status
```

### `there is no package called '...'`

Run the Windows preparation launcher:

```text
deployment\Prepare RIDS.bat
```

Then retry `deployment\Launch RIDS.bat`.

### `Release folder already exists and is not empty`

Either:

- use a new version name, or
- rebuild that folder with `--force`

Example:

```powershell
Rscript R/SETUP/release_publish.R publish-local --version v0.5.0 --force
```

### Shared-drive path issues

Make sure:

- the drive is connected
- you are working inside the shared-drive repo clone
- the repo still contains `shared/`, `releases/`, and `deployment/`

### Rebuild the local cache

If a single laptop has a stale or damaged local cache:

1. Close RIDS
2. Delete `%LOCALAPPDATA%\RIDS\`
3. Run `deployment\Prepare RIDS.bat`
4. Retry `deployment\Launch RIDS.bat`

### `RIDS did not respond on time`

Check:

1. whether `deployment\Prepare RIDS.bat` has been run on that laptop
2. whether the app eventually started anyway
3. `deployment\launch_rids.log` for startup details
4. whether `%LOCALAPPDATA%\RIDS\releases\` was created on the laptop

If the laptop has never been prepared, run `deployment\Prepare RIDS.bat` first, then retry the normal launcher.

### Release smoke check failed

The script stops before changing the live version if the release does not validate.

Common causes:

- the shared config file is missing
- required app files are missing
- dependencies are not installed on the machine
- the checked-out code does not load cleanly

Recommended response:

1. Read the error message carefully
2. Fix the code or machine setup issue
3. Re-run local checks
4. Re-run the publish command
