# RIDS-v1

## Overview

RIDS currently uses a lightweight manual deployment process.

- GitHub is the source of truth for code.
- The live app runs from versioned folders under `releases/`.
- Shared runtime state lives under `shared/`.
- User laptops sync the active release into a local cache before launch.
- Maintainers run local R checks, pull the latest code into the shared-drive clone, and publish a named release with one R script.

Users still launch the app by opening `deployment/Launch RIDS.bat`.
Maintainers should run `deployment/Prepare RIDS.bat` once on each Windows laptop before daily use.

## Shared Drive Layout

After setup, the shared deployment folder looks like this:

```text
RIDS-v1/
├── app source files
├── deployment/
│   ├── Prepare RIDS.bat
│   ├── Launch RIDS.bat
│   ├── prepare_app.R
│   └── launch_app.R
├── releases/
│   ├── v0.5.0/
│   └── v0.5.1/
└── shared/
    ├── current_release.txt
    ├── deploy_log.tsv
    ├── deployment_config.R
    ├── data/
    ├── logs/
    ├── outputs/
    └── uploads/
```

Important points:

- `releases/` contains code only.
- `shared/` contains runtime state only.
- User laptops run the active release from a local cache under `%LOCALAPPDATA%\RIDS\releases\`.
- `current_release.txt` tells the launcher which version is live.
- `deploy_log.tsv` records publish and rollback activity.
- `shared/deployment_config.R` now also contains the stable `CREDENTIAL_SECRET` used to decrypt saved user API keys after backup and restore.

## First-Time Setup

Run these steps once when creating or rebuilding the shared deployment location.

### What you need first

1. Put the RIDS repo in its shared-drive location.
2. Make sure `R`, `Rscript`, and `Git` are installed on the maintainer machine.
3. Make sure end-user laptops can access the shared drive.

### Setup steps

1. Open the project in `RStudio`, or open a terminal in the repo root.
2. Run [R/SETUP/new_setup.R](/Users/tategraham/Documents/NHS/RIDS-v1/R/SETUP/new_setup.R).

Example:

```r
source("R/SETUP/new_setup.R")
```

What setup does:

1. Installs missing R packages.
2. Creates `releases/`, `shared/`, and `deployment/`.
3. Creates `shared/deployment_config.R`.
4. Creates `deployment/Prepare RIDS.bat` and `deployment/Launch RIDS.bat`.
5. Creates the shared DuckDB database if needed.
6. Bootstraps the first live release automatically.
7. Creates that first live release from the current working tree so the launcher works immediately.
8. Generates or preserves the stable `CREDENTIAL_SECRET` used for encrypted user API keys.

## Local Cache Model

Users no longer run the Shiny app code directly from the shared drive.

- The shared drive still stores `releases/` and all `shared/` runtime state.
- `deployment/Launch RIDS.bat` reads `shared/current_release.txt`, syncs that release into the user's local cache, and starts RIDS from the local cached copy.
- `deployment/Prepare RIDS.bat` now prepares packages from the local cached active release.
- The default cache location is `%LOCALAPPDATA%\RIDS\releases\`.
- Support can rebuild the cache by deleting `%LOCALAPPDATA%\RIDS\` on the user's laptop, then rerunning `deployment/Prepare RIDS.bat` or `deployment/Launch RIDS.bat`.

## Development and Release Flow

The current workflow is:

1. Make a change on a branch.
2. Run local checks:

```bash
Rscript R/CI/run_ci_checks.R
```

3. Open and merge a pull request into `main`.
4. On the shared-drive clone, pull the latest `main`.
5. Publish the checked-out code as a named release.
6. Launch and verify the app.
7. Roll back if needed.

For the step-by-step operator runbook, see [DEPLOYMENT.md](/Users/tategraham/Documents/NHS/RIDS-v1/DEPLOYMENT.md:1).

## User Launch Steps

Normal users only need these steps:

1. Make sure a maintainer has already run `deployment/Prepare RIDS.bat` on that laptop.
2. Open `deployment/Launch RIDS.bat`.
3. Wait a few seconds.
4. Let the launcher sync the current release to the local cache if needed.
5. Let the browser open automatically.
6. Sign in.

## Operator Reference Commands

### Run first-time setup

```r
source("R/SETUP/new_setup.R")
```

### Manual backup and restore

Close RIDS first if possible.

To create the daily backup:

```r
source("R/SETUP/manual_backup.R")
```

To restore from a backup:

1. Open `R/SETUP/manual_restore.R`
2. Set `RESTORE_BACKUP_TIMESTAMP <- "YYYY-MM-DD_HHMMSS"`
3. Run:

```r
source("R/SETUP/manual_restore.R")
```

After restore, reopen RIDS and check the key data you expected to recover.
Saved user API keys continue to work after restore as long as the restored installation still uses the same `shared/deployment_config.R` secret.

### Run local checks

```bash
Rscript R/CI/run_ci_checks.R
```

### Publish the current working tree

```bash
Rscript R/SETUP/release_publish.R publish-local --version v0.5.0
```

### Prepare a Windows laptop for first use

```text
deployment/Prepare RIDS.bat
```

### Rebuild an existing release folder

```bash
Rscript R/SETUP/release_publish.R publish-local --version v0.5.0 --force
```

### Roll back to an earlier release

```bash
Rscript R/SETUP/release_publish.R rollback --version v0.4.1
```

## Notes

- `main` is the only permanent branch.
- Each live deployment maps to a manually named working-tree snapshot under `releases/`.
- Shared runtime folders should not be committed to Git.
- The current process is intentionally simple: local checks validate code, and one R script promotes or rolls back versions.
