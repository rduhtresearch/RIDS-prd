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
