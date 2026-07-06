# RIDS Refactor Log

Running record of the phased refactor: what was removed or moved, why, and
how behavior preservation was verified. Newest entries last.

## Phase 0 — Baseline safety net

Goal: establish a regression safety net before any structural change.
No app behavior changed in this phase.

### Test infrastructure added

- `tests/testthat.R` + `tests/testthat/` — testthat (edition 3) harness.
  Every legacy suite in `R/tests/` still runs, wrapped by
  `tests/testthat/test-legacy-*.R` via `helper-legacy.R`; a wrapped suite
  fails the testthat run if any of its internal checks fail. The legacy
  runner `R/CI/run_ci_checks.R` continues to work unchanged and both were
  verified green.
- `tests/testthat/test-characterization-auth.R` — new native tests pinning
  current auth behavior ahead of the Phase 3 auth changes: session-restore
  branches (missing/invalid/revoked/expired/inactive/ok), the current
  three-role model (`user`/`admin`/`developer` — developer approved for
  removal), no user-enumeration on login, admin reset semantics,
  change-password semantics, and the current insecure username-only reset
  (approved for replacement; its test documents the behavior that will be
  deliberately removed).
- `tests/testthat/test-app-startup.R` — smoke test that sources
  `R/setup.r` + `global.R` in a fresh R process against a throwaway
  config/DuckDB and asserts the schema bootstrap completes.
- `tests/testthat/test-legacy-00-release-smoke.R` — the two bootstrap/release
  smoke checks from `R/CI/run_ci_checks.R`, expressed as testthat tests using
  the same underlying functions.

### Orphaned tests wired in

`R/tests/test_custom_activity_module_validation.R` and
`R/tests/test_mff_split_posting_plan.R` existed but were never invoked by
`R/CI/run_ci_checks.R`. Both were run standalone, still pass (13 and 26
checks), and are now wired into the testthat suite. The only change to them:
the validation suite now returns its pass/fail counters like every other
suite (it previously returned nothing).

### Files removed (proven dead)

| File | Evidence | Rationale |
|---|---|---|
| `R/utils/cc_join.r` (133 lines) | No `source()` or reference anywhere in the repo (grep-verified) | Personal scratch script: hardcoded `/Users/.../rules_test.csv` path, `View()` call; defines an obsolete duplicate of `add_cost_centres` (live version is `R/utils/add_cost_centres.r`) |
| `R/utils/cc_join_new.r` (132 lines) | No reference anywhere (grep-verified) | Same scratch lineage: hardcoded `~/Downloads/Book*.csv` paths, commented-out alternates |
| `R/utils/pipeline_test.r` (98 lines) | Nothing sources it (grep-verified; it sourced others, nothing sourced it) | Abandoned early iteration of the pipeline, superseded by the live `R/utils/pipeline_fixed.r` |

### Files renamed

| From | To | Why |
|---|---|---|
| `R/utils/posting_test.r` | `R/utils/posting_engine.r` | Live production code (sourced by `global.R`) misleadingly named like a test file. Rename only — zero logic change; `global.R` and two comment references updated. |

### Verification

- Legacy CI (`Rscript R/CI/run_ci_checks.R`): all checks pass, 0 failures —
  before and after the changes.
- testthat suite (`Rscript tests/testthat.R`): green, wrapping all 18 legacy
  suites plus the new characterization/startup tests.

## Phase 1 — Project structure + env-var config

Goal: dependency declaration, a single source manifest, and
environment-variable-first configuration. No business logic touched.

### Added

- `DESCRIPTION` — declares all runtime dependencies with minimum versions
  (pinned from a verified working environment, R 4.4). Used by tooling
  (renv, Docker builds); the app itself is still a sourced Shiny app, not an
  installed package. A full `renv.lock` is deferred to the Docker phase
  (Phase 4), where it can be generated against CRAN during the image build.
- `R/load_app.R` — the single manifest of app source files in load order.
  `global.R` calls `rids_source_utils()`; `app.R` calls
  `rids_source_modules()`. Replaces 20+ scattered `source()` lines in
  `global.R` and 18 in `app.R`. Order preserved exactly.
- `R/config/runtime_config.R` — `load_app_config()`: every config key reads
  from a `RIDS_*` environment variable first, then falls back to the legacy
  `deployment_config.R` file (located by the existing candidate search),
  then a safe default. Validation rules identical to the legacy reader.
  A container can now configure the app entirely via environment variables;
  the existing shared-drive file path keeps working unchanged.
- `tests/testthat/test-runtime-config.R` — env-only resolution, file
  fallback parity with the legacy reader, per-key env override, and
  validation-rule equivalence.
- App-startup smoke test now runs twice: legacy config file path and pure
  env-var path.

### Removed (vestigial, never functional)

- `SQL_SERVER` / `SQL_DATABASE` / `SQL_DRIVER` config keys: written and
  re-read by config plumbing but never consumed by any code path
  (`connect_primary_database()` rejects any non-duckdb mode before they
  could matter). Removed from `R/utils/deployment_config.R` (reader +
  writer), `R/SETUP/new_setup.R`, `R/CI/run_ci_checks.R`, and four test
  files. Old config files containing these lines still parse fine — the
  reader simply ignores them. Real SQL Server support arrives via the
  Phase 2+ persistence adapters, not these keys.

### Behavior preservation

- `load_app_config()` output shape matches the legacy reader (minus the
  dead sql_* keys); parity is asserted by test.
- Source order preserved verbatim in the manifest.
- Both suites green after the change.

## Phase 2a — Versioned schema migrations

Goal: move all DDL out of boot-time R code into versioned, reproducible
migration files, per dialect. First step of the persistence abstraction.

### Added

- `R/persistence/migrate.R` — small migration runner (no new dependency):
  ordered `NNNN_*.sql` / `NNNN_*.R` files per dialect, applied versions
  tracked in a `schema_migrations` table, each migration in a transaction.
- `R/persistence/migrations/duckdb/`:
  - `0001_initial_schema.sql` — faithful transcription of every sequence,
    table, and index previously created inline by `R/setup.r`,
    `ca_schema.R`, and `ca_ref_activities.R`. All `IF NOT EXISTS`, so an
    existing production DuckDB file adopts versioning without modification.
  - `0002_legacy_column_fixups.R` — the ALTER-if-missing column adds that
    previously ran on every boot, now applied once.
  - `0003_user_api_credentials_drop_fk.R` — the FK-removal rebuild that
    previously ran ad hoc inside `user_tables()`, now versioned.
  - `0004_scrub_meta_data_nul_bytes.R` — the NUL-byte data scrub,
    previously best-effort on every boot, now once (new writes are
    sanitized at the application layer).
- `tests/testthat/test-migrations.R` — runner behavior: full fresh-DB
  schema, no-op second run, transactional rollback of a failing migration,
  filename ordering across .sql and .R steps.

### Changed

- `R/setup.r` (876 → 412 lines): now holds only the startup guards
  (`check_legacy_auth_schema` — the `tokens`-table and unexpected-users-column
  checks that intentionally block boot, unchanged), the idempotent seed data
  (finance rules, settings defaults, specialities), and `db_main()`. All
  per-table entry points (`ict_table`, `user_tables`, `settings_table`, ...)
  survive for existing callers and delegate DDL to the migration runner.
- `ca_init_table()` / `ca_init_ref_activities()` delegate DDL to migrations;
  ref-activity seeding logic unchanged (the redundant identical if/else
  seed branches collapsed to one loop — same effect).

### Removed

- **The `dbo.app_logs` T-SQL fossil branch** in `app_logs_table()`: dead
  code for a `storage_mode == "sqlserver"` path that `connect_primary_database()`
  made unreachable, written in T-SQL that DuckDB could never execute. Real
  SQL Server support arrives as a dialect adapter + its own migration
  directory, per the target architecture.

### Behavior notes

- Legacy fixups (column adds, FK rebuild, NUL scrub) now run once per
  database instead of on every boot — same end state, tracked in
  `schema_migrations`.
- On a database with the legacy `tokens` table, startup now stops before
  applying ANY schema work (previously ict/meta DDL ran first) — strictly
  safer in an already-fatal path.
- Verified: full testthat suite green (124 pass / 0 fail) including the
  legacy-DB upgrade scenarios in `test_setup_migrations.R`, legacy CI green,
  app-startup smoke tests green both config paths.

## Phase 2b — Repository layer (persistence boundary)

Goal: every SQL statement the app issues lives in one repository file per
aggregate under `R/persistence/repositories/`; no `dbGetQuery`/`dbExecute`
outside the persistence layer. Wrapping is verbatim — same SQL, same
transactions, same error-handling shape at call sites.

### Repositories added

`settings`, `app_logs`, `api_credentials`, `users`, `sessions`,
`auth_audit`, `studies` (meta_data + the transactional cross-table cascade
delete of a study run), `ict_costing` (incl. the staging-table atomic
replace used by workbook ingest and the step-2 atomic save), `posting_lines`
(incl. step 4's atomic replace), `rules` (ruleset bundle reads +
posting-line-type list), `specialities`.

`R/persistence/connection.R` provides `build_repositories(con)`,
`with_read_connection(db_path, fn)` (absorbs the posting engine's
DuckDB-specific short-lived read-only connections), and `rids_repos()` — the
transitional accessor binding repositories to the global `CON` until
services receive them by injection (retired with the global CON in Phase 4).

`R/addons/custom_activities/ca_queries.R` already met the repository
contract (injectable `con`, focused portable SQL) and is designated the
custom-activities repository in place, preserving the addon's
self-containment.

### Call sites cut over

`auth.r`, `logging.R`, `user_credentials.R`, `global.R`, `admin_mod.r`,
`add_cost_centres.r`, `study_deletion.R`, `pipeline_fixed.r`,
`posting_engine.r`, `posting_lines.r`, `step1-4_mod.R`, `library_mod.R`,
`study_workspace_mod.R`. Remaining direct SQL: DuckDB WAL `CHECKPOINT` in
`deployment_config.R` (backup infra, deleted in Phase 4) and test fixtures.

### Also removed

- The unreachable SQL-Server `TOP(n)` branch in `query_app_logs` and its
  `current_storage_mode()` helper.

### Verification

Both suites green after each of the two cutover commits (testthat 124
pass / 0 fail; legacy CI all checks passed), including the step-2/step-4
atomic-save, contract-cost source-of-truth, study-deletion, and custom-
activities suites that exercise the moved SQL directly.
