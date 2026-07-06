# RIDS architecture

Shiny application for research income distribution: ICT costing workbook
ingestion, posting-plan generation, EDGE template export, and finance rule
administration.

## Layers

```
app.R / global.R          boot: config, DB connection, schema, source manifest
R/load_app.R              single ordered manifest of app source files
R/config/                 runtime config (RIDS_* env vars first, legacy file fallback)
R/modules/                Shiny UI/server modules (xxxUI/xxxServer)
R/utils/                  domain logic (posting engine, pipeline, templates)
                          + auth orchestration (auth.r) + logging
R/auth/                   auth provider boundary: TOTP MFA, MFA-gated reset,
                          build_auth_provider() — the only auth surface the UI touches
R/persistence/            the persistence boundary:
  db_helpers.R              dialect differences (placeholders, identifier case)
  connection.R              build_repositories(), rids_repos(), read connections
  repositories/             one repository per aggregate — ALL SQL lives here
  migrate.R                 versioned migration runner (schema_migrations table)
  migrations/duckdb/        DuckDB schema history (incl. legacy fixups)
  migrations/postgres/      PostgreSQL schema (fresh DBs start current)
R/addons/custom_activities/ self-contained addon; ca_queries.R is its repository
tests/testthat/           the test suite (wraps the legacy R/tests suites too)
docker/ + Dockerfile      container runtime; docker-compose.yml for local dev
```

## Rules

- **All SQL lives in `R/persistence/repositories/`** (plus the addon's
  `ca_queries.R`). Modules and services never call DBI directly.
- **Dialect differences live in `db_helpers.R` and per-dialect migration
  directories.** Application code is SQL-backend agnostic; supported
  backends are DuckDB (dev) and PostgreSQL/Supabase (hosted), with SQL
  Server designed for (see `sql-server-adapter.md`).
- **The UI's only auth surface is `build_auth_provider()`.** Passwords are
  sodium-hashed; sessions are opaque cookie tokens (contract with
  `www/app-shell.js` — hash stored, token not); MFA is mandatory TOTP with
  hashed one-time recovery codes; self-service password reset requires an
  MFA code. Roles: `user` / `admin`.
- **Schema changes are migrations** (`R/persistence/migrations/<dialect>/
  NNNN_*.sql|R`), applied automatically at boot inside transactions and
  tracked in `schema_migrations`. Write each change for both dialect
  directories.
- **Configuration is environment variables** (`RIDS_*`, see `.env.example`);
  the legacy `deployment_config.R` file format still works as a fallback.

## Startup sequence

`app.R` sources `R/setup.r` (guards + seed definitions) and `global.R`.
`global.R` sources everything via the manifest, resolves config
(`load_app_config()`), connects (`connect_primary_database()`), and runs
`db_main()`: legacy-schema guards → migrations → idempotent seeds (finance
rules, settings defaults, specialities, custom-activity reference data).
A failed migration fails the boot.

## Testing

`Rscript tests/testthat.R` (or `R/CI/run_ci_checks.R`). The suite wraps the
legacy `R/tests/` suites (every original assertion still runs) plus native
tests: migration runner, runtime config, TOTP (RFC 6238 vectors), MFA flows,
auth characterization, app-startup smoke (config-file, env-only, and
PostgreSQL variants), DuckDB WAL consolidation. Set `RIDS_TEST_PG_URL` to a
disposable PostgreSQL database to enable the PostgreSQL integration tests
(schema equivalence with DuckDB, full flows).
