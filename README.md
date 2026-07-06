# RIDS

Research Income Distribution System: a Shiny application for ICT costing
workbook ingestion, posting-plan generation, EDGE template export, and
finance rule administration.

- **Runtime**: Docker container (R 4.4 + Shiny), configured entirely via
  environment variables.
- **Database**: PostgreSQL (incl. Supabase) for hosted deployments, DuckDB
  for zero-infrastructure local development. All SQL sits behind a
  repository layer; the schema is managed by versioned migrations applied
  at boot.
- **Auth**: username/password with mandatory TOTP two-factor authentication
  (any authenticator app), one-time recovery codes, MFA-gated self-service
  password reset, and admin-managed accounts (roles: `user` / `admin`).

See [`docs/architecture.md`](docs/architecture.md) for the code layout and
rules, and [`DEPLOYMENT.md`](DEPLOYMENT.md) for running and hosting it.
[`REFACTOR_LOG.md`](REFACTOR_LOG.md) records how the codebase got here.

## Quick start (Docker)

```bash
cp .env.example .env
# set RIDS_CREDENTIAL_SECRET in .env:  openssl rand -hex 32
docker compose up --build
# open http://localhost:3838 — the first visit creates the initial admin
# account and walks through authenticator enrollment
```

## Run the tests

```bash
Rscript tests/testthat.R
# with PostgreSQL integration tests:
RIDS_TEST_PG_URL=postgres://user:pass@host:5432/disposable_db Rscript tests/testthat.R
```

## Development without Docker

Install R (>= 4.4) and the packages listed in `DESCRIPTION`, then:

```bash
export RIDS_STORAGE_MODE=duckdb
export RIDS_DB_DIR=./data/RIDS.duckdb
export RIDS_ICT_UPLOAD_DIR=./uploads
export RIDS_EDGE_OUTPUT_DIR=./outputs
export RIDS_CREDENTIAL_SECRET=$(openssl rand -hex 32)
export RIDS_APP_STATUS=dev
Rscript -e "shiny::runApp('.', port = 3838)"
```
