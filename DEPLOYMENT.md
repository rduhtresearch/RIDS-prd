# Deploying RIDS

RIDS runs as a single container configured by environment variables. There
are no shared drives, launcher scripts, or manual release folders — build
the image, point it at a database, run it.

## Configuration

Every setting is a `RIDS_*` environment variable; `.env.example` documents
all of them. The required ones:

| Variable | Purpose |
|---|---|
| `RIDS_CREDENTIAL_SECRET` | Encryption key for stored secrets (user API keys, TOTP secrets). 16+ chars, generate with `openssl rand -hex 32`. **Losing it makes stored secrets unrecoverable; changing it invalidates them.** |
| `RIDS_STORAGE_MODE` | `postgres` (hosted) or `duckdb` (single-file dev) |
| `RIDS_DATABASE_URL` | postgres mode: `postgres://user:pass@host:5432/dbname` (or set the standard `PG*` variables) |
| `RIDS_DB_DIR` | duckdb mode: path to the database file |

Optional: `RIDS_APP_STATUS` (`dev`/`test`/`live` — `live` enables
clean-shutdown-when-idle), `RIDS_APP_PORT`, `RIDS_AUTH_SESSION_HOURS`,
`RIDS_ICT_UPLOAD_DIR`, `RIDS_EDGE_OUTPUT_DIR`, `RIDS_APP_LOG_DIR`,
`RIDS_APP_VERSION`, `RIDS_APP_LAST_UPDATED`.

The legacy `deployment_config.R` file format still works as a fallback
(point `RIDS_CONFIG_PATH` at it); environment variables override it
key by key.

## Local development

```bash
cp .env.example .env          # set RIDS_CREDENTIAL_SECRET
docker compose up --build     # app + local PostgreSQL 16
```

DuckDB mode (no database service):

```bash
RIDS_STORAGE_MODE=duckdb docker compose up app
```

## Hosted deployment

Any container host works (a VM with compose, Kubernetes, Cloud Run, ECS,
Fly.io, ...). The essentials:

1. **Build and push the image**: `docker build -t <registry>/rids:<tag> .`
   Package versions come from a date-pinned Posit Package Manager snapshot
   (`RIDS_CRAN_SNAPSHOT` build arg) — bump it deliberately and re-run the
   test suite.
2. **Provision PostgreSQL** and set `RIDS_DATABASE_URL`.
   - **Supabase**: use the connection string from Project Settings →
     Database (`sslmode=require`). RIDS uses Supabase strictly as managed
     PostgreSQL — no Supabase SDK, no lock-in; any PostgreSQL provider is
     interchangeable.
   - Schema migrations run automatically at boot (tracked in
     `schema_migrations`); a failed migration fails the boot loudly.
3. **Set secrets** via your host's secret manager: `RIDS_CREDENTIAL_SECRET`
   at minimum.
4. **Persistent files**: mount volumes (or otherwise persist)
   `RIDS_ICT_UPLOAD_DIR` and `RIDS_EDGE_OUTPUT_DIR` if uploaded workbooks
   and generated EDGE ZIPs must survive container replacement.
5. **TLS**: terminate HTTPS in front of the container (reverse proxy /
   platform ingress). The auth session cookie is an opaque token — serve
   the app over HTTPS only.
6. **Backups**: use the database provider's native mechanism (Supabase
   automated backups, `pg_dump` on a schedule for self-hosted PostgreSQL).
   There is no app-level backup machinery to run.

## First run

On an empty database the app walks through creating the initial admin
account, including mandatory authenticator (TOTP) enrollment and one-time
recovery codes. Store the recovery codes safely.

Admin panel capabilities: create/edit users (roles `user`/`admin`),
activate/deactivate, reset passwords, and **Reset MFA** (clears a user's
authenticator enrollment so they re-enroll at next sign-in — the recovery
path when someone loses their device and their recovery codes).

## Switching or adding database backends

The application core is SQL-backend agnostic; backends plug in at the
persistence boundary. Supported today: DuckDB and PostgreSQL. SQL Server is
designed for but not implemented — `docs/sql-server-adapter.md` specifies
exactly what an implementation entails. Moving between PostgreSQL providers
(e.g. Supabase → internal) is a data migration plus a new
`RIDS_DATABASE_URL`; no code changes.

## Upgrades

Deploy the new image; migrations bring the schema forward at boot. For
DuckDB, the historical migrations also adopt pre-migration-era database
files in place (including databases from the old Windows shared-drive
deployment — point `RIDS_DB_DIR` at the copied `.duckdb` file).
