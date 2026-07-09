# SQL Server adapter — design (not yet implemented)

The persistence architecture supports SQL Server through the same
boundaries PostgreSQL uses. `storage_mode = "sqlserver"` currently fails
fast with a pointer to this document. Implementing it means the four steps
below — no application, service, or UI code changes.

## What already isolates the dialect

| Boundary | File | What SQL Server needs |
|---|---|---|
| Connection factory | `R/utils/deployment_config.R` (`connect_primary_database`) | An `open_sqlserver_connection()` using `odbc::dbConnect(odbc::odbc(), ...)` driven by env vars (`RIDS_SQLSERVER_*` or a connection string) |
| Placeholder style | `R/persistence/db_helpers.R` (`rids_translate_placeholders`) | odbc uses `?` natively — likely a no-op branch |
| Identifier case | `R/persistence/db_helpers.R` (`rids_canonicalize_names`, `rids_prepare_append`) | SQL Server preserves case (default collations are case-insensitive) — likely no-ops, verify against the chosen collation |
| Migrations | `R/persistence/migrations/<dialect>/` + `rids_dialect_for()` | A `migrations/sqlserver/0001_initial_schema.sql` in T-SQL, plus a `"Microsoft SQL Server"` branch in `rids_dialect_for()` (dispatch on `odbc::dbGetInfo(con)$dbms.name`) |

## T-SQL dialect differences to handle in `migrations/sqlserver/0001`

- **Sequences / identity**: `CREATE SEQUENCE` exists in SQL Server 2012+;
  `nextval('seq')` becomes `NEXT VALUE FOR seq` in column defaults, and the
  `currval` idiom used by `user_repository$insert` / `session_repository$insert`
  / `study_repository$last_upload_id` has no direct equivalent — use
  `OUTPUT INSERTED.<id>` on the INSERT (a small per-dialect override of those
  three repository methods, or switch all dialects to `INSERT ... RETURNING`-style
  helpers in `db_helpers.R`).
- **Types**: `TEXT` → `NVARCHAR(MAX)` (T-SQL `TEXT` is deprecated),
  `DOUBLE PRECISION` → `FLOAT(53)`, `BOOLEAN` → `BIT` (and R logicals map to
  0/1 — verify `isTRUE(row$active[[1]])` comparisons still hold via odbc's
  bit→logical mapping), `TIMESTAMP` → `DATETIME2` (T-SQL `TIMESTAMP` is a
  rowversion, not a time).
- **Defaults**: `CURRENT_TIMESTAMP` works; prefer `SYSUTCDATETIME()` if the
  deployment standardizes on UTC.
- **LIMIT**: `LIMIT n` → `TOP (n)` / `OFFSET ... FETCH`. Used by the
  repositories' `LIMIT 1` lookups and `app_log_repository$query` — either a
  translation rule in `db_helpers.R` or per-dialect query text.
- **Upsert**: the seeds use `INSERT ... ON CONFLICT DO NOTHING` (specialities,
  ref_custom_activities) → `MERGE` or `IF NOT EXISTS(...) INSERT`.
- **`ON CONFLICT`-free paths**: the rules seeding (`upsert_rule_row`) already
  does check-then-insert and is portable as-is.

## Suggested implementation order

1. `migrations/sqlserver/0001_initial_schema.sql` (T-SQL transcription of
   `migrations/postgres/0001`).
2. `rids_dialect_for()` + placeholder/case/append branches in
   `db_helpers.R`; add a `LIMIT` translation rule.
3. `open_sqlserver_connection()` + config keys.
4. Per-dialect overrides for the three `currval` call sites.
5. Point the existing integration suite at it: generalize
   `test-postgres-integration.R` into a backend-parameterized suite and run
   with `RIDS_TEST_SQLSERVER_URL` — the same schema-equivalence, auth/MFA,
   and round-trip tests define "done".

## Non-goals

Encryption, MFA, session logic, business rules, and the UI are backend-
independent already; nothing there changes for SQL Server.
