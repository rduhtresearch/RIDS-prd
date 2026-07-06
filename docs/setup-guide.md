# RIDS setup and deployment guide

A step-by-step operator's guide, from nothing to a running, hosted RIDS
instance. The shorter [`DEPLOYMENT.md`](../DEPLOYMENT.md) is the reference
summary; this document walks through each path in detail. Architecture
background lives in [`docs/architecture.md`](architecture.md).

Contents:

1. [How RIDS runs](#1-how-rids-runs)
2. [Prerequisites](#2-prerequisites)
3. [Configuration reference](#3-configuration-reference)
4. [Path A — local development with Docker](#4-path-a--local-development-with-docker)
5. [Path B — local development without Docker](#5-path-b--local-development-without-docker)
6. [Path C — production with Supabase](#6-path-c--production-with-supabase)
7. [Path D — production with self-hosted PostgreSQL](#7-path-d--production-with-self-hosted-postgresql)
8. [First run: admin account and MFA](#8-first-run-admin-account-and-mfa)
9. [Day-to-day administration](#9-day-to-day-administration)
10. [Migrating data from the old Windows deployment](#10-migrating-data-from-the-old-windows-deployment)
11. [Backups and restore](#11-backups-and-restore)
12. [Upgrades](#12-upgrades)
13. [Running the test suite](#13-running-the-test-suite)
14. [Troubleshooting](#14-troubleshooting)

---

## 1. How RIDS runs

RIDS is a single Shiny application process, packaged as a Docker image.
Everything it needs is provided through environment variables — there is no
setup wizard, no config file to hand-edit, no shared drive.

On every boot the app:

1. Reads its configuration from `RIDS_*` environment variables.
2. Connects to the configured database (PostgreSQL or DuckDB).
3. Applies any pending schema migrations
   (`R/persistence/migrations/<dialect>/`), tracked in the
   `schema_migrations` table. A failed migration fails the boot — the app
   never runs against a half-migrated schema.
4. Seeds idempotent reference data (finance rules, default settings,
   specialities, custom-activity reference names). Existing rows are never
   overwritten, so admin edits survive restarts.
5. Serves the app on `RIDS_APP_HOST:RIDS_APP_PORT` (default `0.0.0.0:3838`
   in the container).

Two storage modes:

| Mode | When to use | What you provide |
|---|---|---|
| `postgres` | Hosted / production, or dev with prod parity | `RIDS_DATABASE_URL` (Supabase or any PostgreSQL 13+) |
| `duckdb` | Zero-infrastructure local development | `RIDS_DB_DIR`, a path for the single database file |

The application code is identical in both modes; the persistence layer
handles the dialect differences.

## 2. Prerequisites

**For Docker paths (A, C, D)** — the recommended way to run RIDS:

- Docker Engine 24+ and Docker Compose v2 (`docker compose version`).
- Outbound HTTPS from the build machine to
  `packagemanager.posit.co` (R package binaries) and Docker Hub
  (base images) — build-time only; the running container needs no
  outbound access except to its database.

**For the bare-R path (B)**:

- R >= 4.4 and a C toolchain (Windows: Rtools; macOS: Xcode CLT;
  Linux: `build-essential` plus `libsodium-dev libpq-dev libssl-dev`).
- The packages listed in `DESCRIPTION` (install command in section 5).

**For production (C, D)**:

- A container host: a VM with Docker, or a platform (Kubernetes, ECS,
  Cloud Run, Fly.io, Azure Container Apps, ...).
- A PostgreSQL 13+ database (Supabase project or self-hosted).
- A way to terminate TLS in front of the container (reverse proxy such as
  Caddy/nginx/Traefik, or your platform's ingress). **RIDS must only be
  reached over HTTPS in production** — the session cookie is an opaque
  bearer token.

## 3. Configuration reference

Copy `.env.example` to `.env` for compose, or set these in your host's
environment/secret manager. Environment variables always win; the legacy
`deployment_config.R` file format is still read as a fallback if
`RIDS_CONFIG_PATH` points at one (useful only during migration from the old
deployment).

### Required

| Variable | Meaning | Notes |
|---|---|---|
| `RIDS_CREDENTIAL_SECRET` | Key that encrypts stored secrets at rest (user API keys, TOTP secrets) | 16+ characters. Generate once with `openssl rand -hex 32`. Treat like a database password: store in a secret manager, never commit. **If lost, stored API keys and MFA enrollments cannot be decrypted** — users would re-enter API keys and re-enroll MFA. Changing it has the same effect as losing it. |
| `RIDS_STORAGE_MODE` | `postgres` or `duckdb` | |
| `RIDS_DATABASE_URL` | (postgres mode) `postgres://USER:PASSWORD@HOST:5432/DBNAME?sslmode=require` | Alternatively set the standard `PGHOST`/`PGPORT`/`PGDATABASE`/`PGUSER`/`PGPASSWORD` variables and omit the URL. |
| `RIDS_DB_DIR` | (duckdb mode) path to the `.duckdb` file | Parent directory must exist and be writable. |

### Recommended

| Variable | Default | Meaning |
|---|---|---|
| `RIDS_APP_STATUS` | `live` | `dev` / `test` / `live`. `dev` shows the dev banner; `live` enables the clean-shutdown-when-idle behavior (the process exits after the last browser session ends plus a grace period — appropriate under a supervisor/orchestrator that restarts it). For an always-on hosted container, `test` or `dev` avoids idle shutdowns; keep `live` only if your platform restarts exited containers. |
| `RIDS_ICT_UPLOAD_DIR` | `/app/uploads` (image default) | Where uploaded ICT workbooks are stored. Persist with a volume if uploads must survive container replacement. |
| `RIDS_EDGE_OUTPUT_DIR` | `/app/outputs` (image default) | Where generated EDGE template ZIPs are written. Persist likewise. |
| `RIDS_APP_LOG_DIR` | `/app/logs` (image default) | Per-run text logs (pruned after 24h). Container stdout carries the same lines. |

### Optional

| Variable | Default | Meaning |
|---|---|---|
| `RIDS_APP_HOST` / `RIDS_APP_PORT` | `0.0.0.0` / `3838` | Bind address/port inside the container |
| `RIDS_AUTH_SESSION_HOURS` | `10` | Login session lifetime |
| `RIDS_APP_VERSION` / `RIDS_APP_LAST_UPDATED` | `v1.0.0` / build date | Version labels shown in the UI |
| `POSTGRES_PASSWORD` | `rids-dev-password` | Only used by the local compose `db` service |

## 4. Path A — local development with Docker

The default compose stack runs the app plus a local PostgreSQL 16 with a
persistent volume.

```bash
git clone <repo> rids && cd rids
cp .env.example .env
# edit .env: set RIDS_CREDENTIAL_SECRET (openssl rand -hex 32)

docker compose up --build
```

First build takes several minutes (R package binaries); rebuilds are
cached. When the log shows `Starting RIDS on 0.0.0.0:3838`, open
<http://localhost:3838> and continue with [section 8](#8-first-run-admin-account-and-mfa).

**DuckDB mode instead** (no database service, data in a named volume):

```bash
RIDS_STORAGE_MODE=duckdb RIDS_DB_DIR=/app/data/RIDS.duckdb docker compose up --build app
```

Useful commands:

```bash
docker compose logs -f app              # app logs
docker compose exec db psql -U rids     # SQL shell into the dev database
docker compose down                     # stop (volumes/data preserved)
docker compose down -v                  # stop AND delete all data
```

## 5. Path B — local development without Docker

```bash
# 1. Install dependencies (one-time)
Rscript -e 'install.packages(c(
  "DBI","duckdb","RPostgres","sodium","digest",
  "shiny","bs4Dash","waiter","shinyFeedback","shinyjs",
  "reactable","DT","jsonlite","zip","scales",
  "dplyr","tidyr","stringr","purrr","readr","openxlsx","later"
))'

# 2. Configure and run (DuckDB mode shown)
export RIDS_STORAGE_MODE=duckdb
export RIDS_DB_DIR=./data/RIDS.duckdb
export RIDS_ICT_UPLOAD_DIR=./uploads
export RIDS_EDGE_OUTPUT_DIR=./outputs
export RIDS_APP_LOG_DIR=./logs
export RIDS_CREDENTIAL_SECRET=$(openssl rand -hex 32)   # or a fixed dev value
export RIDS_APP_STATUS=dev
mkdir -p data uploads outputs logs

Rscript -e "shiny::runApp('.', port = 3838)"
```

To develop against PostgreSQL instead, set `RIDS_STORAGE_MODE=postgres` and
`RIDS_DATABASE_URL` to any reachable database (a `docker run postgres:16`
works fine).

Note: with a fixed `RIDS_CREDENTIAL_SECRET` your dev MFA enrollments
survive restarts; with a random one per shell you'll re-enroll each time
the secret changes.

## 6. Path C — production with Supabase

RIDS uses Supabase **strictly as managed PostgreSQL** — no Supabase SDK,
no auth integration, no lock-in. Any PostgreSQL provider can replace it
later by swapping the connection string.

1. **Create the project**: Supabase dashboard → New project. Choose a
   strong database password and a region close to your users.
2. **Get the connection string**: Project Settings → Database →
   Connection string. Use the **session mode** string (direct connection or
   the session pooler on port 5432), *not* the transaction pooler (port
   6543) — RIDS holds a persistent connection and uses session-scoped
   features (`currval`). It looks like:
   ```
   postgres://postgres.<ref>:<PASSWORD>@aws-0-<region>.pooler.supabase.com:5432/postgres?sslmode=require
   ```
3. **(Recommended) dedicated database user**: in the SQL editor create a
   role for RIDS instead of using `postgres`:
   ```sql
   CREATE ROLE rids LOGIN PASSWORD '<strong-password>';
   GRANT ALL ON SCHEMA public TO rids;
   ALTER ROLE rids SET search_path = public;
   ```
   and use that user in the connection string. RIDS creates and owns all of
   its tables in `public` on first boot.
4. **Build and push the image**:
   ```bash
   docker build -t <registry>/rids:v2.0.0 .
   docker push <registry>/rids:v2.0.0
   ```
5. **Run the container** on your host with:
   ```
   RIDS_STORAGE_MODE=postgres
   RIDS_DATABASE_URL=<the connection string>
   RIDS_CREDENTIAL_SECRET=<from your secret manager>
   RIDS_APP_STATUS=test        # or live, if your platform restarts exited containers
   ```
   plus volumes for `/app/uploads` and `/app/outputs` if those files must
   persist. Expose port 3838 to your TLS-terminating proxy only.
6. **Verify**: container logs should show migrations applying
   (`schema_migrations` rows appear in Supabase's table editor) and
   `Starting RIDS on 0.0.0.0:3838`. Open the app over HTTPS and complete
   [first run](#8-first-run-admin-account-and-mfa).
7. **Backups**: Supabase runs daily automated backups (retention depends on
   plan); enable Point-in-Time Recovery if the plan allows. Nothing to
   configure on the RIDS side.

## 7. Path D — production with self-hosted PostgreSQL

1. Provision PostgreSQL 13+ (a managed instance or your own). Create a
   database and user:
   ```sql
   CREATE DATABASE rids;
   CREATE ROLE rids LOGIN PASSWORD '<strong-password>';
   GRANT ALL PRIVILEGES ON DATABASE rids TO rids;
   ```
2. Ensure the app host can reach it (firewall/security group), ideally
   with TLS (`?sslmode=require`).
3. Build/push/run the image exactly as in Path C, with
   `RIDS_DATABASE_URL=postgres://rids:<password>@<host>:5432/rids?sslmode=require`.

**Example: single VM with compose + Caddy for TLS** — a minimal,
production-reasonable setup for a small internal app:

```yaml
# docker-compose.prod.yml
services:
  app:
    image: <registry>/rids:v2.0.0
    restart: unless-stopped
    environment:
      RIDS_STORAGE_MODE: postgres
      RIDS_DATABASE_URL: ${RIDS_DATABASE_URL}
      RIDS_CREDENTIAL_SECRET: ${RIDS_CREDENTIAL_SECRET}
      RIDS_APP_STATUS: test
    volumes:
      - rids_uploads:/app/uploads
      - rids_outputs:/app/outputs

  caddy:
    image: caddy:2
    restart: unless-stopped
    ports: ["80:80", "443:443"]
    volumes:
      - ./Caddyfile:/etc/caddy/Caddyfile
      - caddy_data:/data

volumes:
  rids_uploads:
  rids_outputs:
  caddy_data:
```

```
# Caddyfile — automatic HTTPS for your domain
rids.example.org {
    reverse_proxy app:3838
}
```

```bash
RIDS_DATABASE_URL=... RIDS_CREDENTIAL_SECRET=... \
  docker compose -f docker-compose.prod.yml up -d
```

Schedule backups with `pg_dump` (see section 11).

## 8. First run: admin account and MFA

On an empty database the app opens on the **first-use setup** screen:

1. **Create the admin account**: full name, username, password (minimum 8
   characters).
2. **Enroll two-factor authentication** (mandatory for every account): the
   screen shows a *setup key* and an `otpauth://` URL. In any authenticator
   app (Google Authenticator, Authy, 1Password, Microsoft Authenticator...)
   choose "enter a setup key" and type the key (account name: your
   username, issuer: RIDS), or add by URL if your app supports it. Enter
   the current 6-digit code to activate.
3. **Save the recovery codes**: 8 one-time codes are shown **once**. Store
   them in a password manager or printed in a safe place. Each code can
   substitute for an authenticator code exactly once (sign-in or password
   reset).
4. You land on the dashboard, signed in.

Subsequent sign-ins: username + password, then a 6-digit authenticator
code (or an unused recovery code). Sessions persist across page refreshes
for `RIDS_AUTH_SESSION_HOURS` (default 10).

**Self-service password reset** ("Forgot password?"): requires username +
a valid authenticator/recovery code + the new password. Users without a
working authenticator and no recovery codes left need an admin (below).

## 9. Day-to-day administration

The **Admin** tab (visible to `admin` role only):

- **Create users**: name, username, optional email, role (`user` or
  `admin`), temporary password. The user must change the password at first
  sign-in and then enroll MFA.
- **Edit / deactivate**: deactivating a user immediately revokes their
  open sessions.
- **Reset Password**: sets a temporary password (shown once — share it
  securely); revokes the user's sessions and forces a change at next
  sign-in.
- **Reset MFA**: clears the selected user's authenticator enrollment and
  recovery codes; they re-enroll at their next sign-in. This is the
  recovery path when someone loses both their device and their recovery
  codes. Every reset is written to the auth audit log.
- **Settings**: upload/output directories, cost-centre matrix file, log
  retention.
- **Logs**: query the in-app audit/application logs and download the
  per-run log files.

Security-relevant events (logins, failures, resets, MFA enrollment and
resets, session revocations) are recorded in the `auth_audit_log` table.

## 10. Migrating data from the old Windows deployment

The pre-refactor deployment kept a DuckDB file on the shared drive
(`shared/data/RIDS.duckdb`). The historical migrations adopt such files in
place — including the one-time legacy fixups (column additions, credential
FK removal, NUL scrub) and the developer→admin role mapping.

**Option 1 — keep DuckDB (quickest):**

1. Stop anything using the old file; copy `RIDS.duckdb` (and `RIDS.duckdb.wal`
   if present) off the share.
2. Run the container in duckdb mode with the file mounted:
   ```bash
   docker run -d -p 3838:3838 \
     -v /srv/rids/data:/app/data \
     -e RIDS_STORAGE_MODE=duckdb -e RIDS_DB_DIR=/app/data/RIDS.duckdb \
     -e RIDS_CREDENTIAL_SECRET='<THE OLD SECRET>' \
     <registry>/rids:v2.0.0
   ```
3. **Use the old `CREDENTIAL_SECRET`** from the old
   `shared/deployment_config.R` — otherwise stored user API keys cannot be
   decrypted. Passwords are unaffected either way (hashes, not encrypted).
4. First boot applies the pending migrations; users keep their accounts and
   passwords, and will be prompted to enroll MFA at next sign-in.

**Option 2 — move to PostgreSQL:** stand up the Postgres instance first
(Path C/D), then copy the data table-by-table. There is no automated
DuckDB→PostgreSQL transfer script; for the handful of tables involved,
DuckDB's own `postgres` extension is the practical tool
(`ATTACH 'postgres:<url>' AS pg; INSERT INTO pg.users SELECT * FROM users;`
etc., after booting RIDS once against the empty Postgres so the schema
exists). Do a row-count comparison per table afterwards. Keep the old
`CREDENTIAL_SECRET` for the same reason as above.

Old `saved_file_path` / `edge_zip_path` values in `meta_data` point at
shared-drive paths; copy the referenced upload/output files into the new
volumes if you need the download buttons for historical studies to work.

## 11. Backups and restore

**PostgreSQL** (production): use the provider's mechanism.

- Supabase: automated backups / PITR — nothing to run.
- Self-hosted: schedule `pg_dump`:
  ```bash
  pg_dump "$RIDS_DATABASE_URL" -Fc -f rids_$(date +%F).dump   # nightly cron
  pg_restore -d "$RIDS_DATABASE_URL" --clean rids_2026-07-06.dump  # restore
  ```

**Uploaded/generated files**: back up the `/app/uploads` and `/app/outputs`
volumes with your normal file backup tooling.

**DuckDB** (dev): the database is one file — stop the app and copy it.

**Also back up `RIDS_CREDENTIAL_SECRET` itself** (in your secret manager).
A database restore without the matching secret leaves API keys and MFA
enrollments undecryptable.

## 12. Upgrades

1. Build the new image (bump the tag; bump `RIDS_CRAN_SNAPSHOT` in the
   Dockerfile only when you deliberately want newer R packages, then run
   the test suite).
2. Take/verify a database backup.
3. Deploy the new image. Pending migrations apply at boot inside
   transactions; on failure the container exits with the error in its logs
   and the schema is left at the last completed migration.
4. Rollback = redeploy the previous image tag. Migrations are
   forward-only; if a bad migration was already applied, restore the
   database backup from step 2.

## 13. Running the test suite

```bash
Rscript tests/testthat.R            # everything (PG/browser tests skip if unavailable)
```

Optional integration layers:

```bash
# PostgreSQL integration (schema equivalence, full flows) — DISPOSABLE db:
# its public schema is dropped and recreated.
RIDS_TEST_PG_URL=postgres://user:pass@host:5432/rids_test Rscript tests/testthat.R

# Browser smoke test (first-run auth flow in headless Chrome):
# runs automatically when the chromote package + Chrome/Chromium are
# installed; point CHROMOTE_CHROME at the binary if it's not on PATH.
```

`Rscript R/CI/run_ci_checks.R` is an equivalent CI entry point.

## 14. Troubleshooting

| Symptom | Likely cause / fix |
|---|---|
| Boot fails: `Missing credential secret` | Set `RIDS_CREDENTIAL_SECRET` (16+ chars). |
| Boot fails: `Storage mode 'postgres' needs RIDS_DATABASE_URL` | Set the URL or the `PG*` variables. |
| Boot fails: `Migrations directory not found` | The container must run with the repo at `/app` (the image does this); if running bare R, start from the repo root. |
| `connection ... failed: Connection refused` | Database not reachable: check host/port/firewall; in compose, the app waits for the `db` healthcheck — check `docker compose logs db`. |
| Supabase: connection drops or `prepared statement` errors | You're on the transaction pooler (port 6543). Use the session/direct string on port 5432. |
| `Legacy auth table 'tokens' was found` at boot | Deliberate guard for very old databases: back up, remove/migrate the legacy `tokens` table manually, then start again. |
| User locked out of MFA | Admin tab → select the user → **Reset MFA**; they re-enroll at next sign-in. Lost admin MFA with no recovery codes and no second admin: delete that user's rows from `mfa_factors` and `mfa_recovery_codes` directly in the database. |
| Uploads/EDGE ZIP downloads 404 after redeploy | The upload/output dirs weren't on persistent volumes. Mount volumes over `/app/uploads` and `/app/outputs`. |
| App exits a while after everyone logs out | `RIDS_APP_STATUS=live` enables clean-shutdown-when-idle. Use `test`/`dev`, or run under a supervisor that restarts the container. |
| DuckDB: `.wal` file left after a crash | Harmless — replayed and folded in automatically on next open. |
