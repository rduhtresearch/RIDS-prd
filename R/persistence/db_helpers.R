# Dialect helpers shared by all repositories.
#
# Queries are written once in standard SQL with `?` placeholders and
# unquoted identifiers. The two real DuckDB/PostgreSQL differences are
# absorbed here:
#   1. Placeholders: RPostgres wants $1..$n; DuckDB wants ?.
#   2. Identifier case: PostgreSQL folds unquoted identifiers to lowercase,
#      so result sets lose the app's canonical mixed-case column names
#      (CPMS_ID, Study_Arm, ...). rids_canonicalize_names() restores them
#      after fetch; rids_prepare_append() lowercases data-frame names before
#      appending on PostgreSQL. Both are no-ops on DuckDB.

rids_is_postgres <- function(con) {
  inherits(con, "PqConnection")
}

rids_dialect_for <- function(con) {
  if (rids_is_postgres(con)) "postgres" else "duckdb"
}

rids_translate_placeholders <- function(con, sql) {
  if (!rids_is_postgres(con) || !grepl("?", sql, fixed = TRUE)) {
    return(sql)
  }

  parts <- strsplit(sql, "?", fixed = TRUE)[[1]]
  n_placeholders <- lengths(regmatches(sql, gregexpr("?", sql, fixed = TRUE)))
  out <- parts[1]
  for (i in seq_len(n_placeholders)) {
    out <- paste0(out, "$", i, if (i + 1 <= length(parts)) parts[i + 1] else "")
  }
  out
}

rids_dbGetQuery <- function(con, sql, params = NULL) {
  sql <- rids_translate_placeholders(con, sql)
  if (is.null(params)) {
    DBI::dbGetQuery(con, sql)
  } else {
    DBI::dbGetQuery(con, sql, params = params)
  }
}

rids_dbExecute <- function(con, sql, params = NULL) {
  sql <- rids_translate_placeholders(con, sql)
  if (is.null(params)) {
    DBI::dbExecute(con, sql)
  } else {
    DBI::dbExecute(con, sql, params = params)
  }
}

# Canonical (mixed-case) column names for the tables that have them.
# DuckDB preserves these; PostgreSQL folds them to lowercase, and fetches
# are renamed back so application code sees one consistent shape.
RIDS_CANONICAL_COLUMNS <- list(
  ict_costing_tbl = c(
    "CPMS_ID", "study_site", "scenario_id", "Study", "Visit_Number",
    "Study_Arm", "Visit_Label", "Activity_Name", "ICT_Cost", "Contract_Cost",
    "activity_occurrence_id", "staff_group"
  ),
  posting_lines = c(
    "row_id", "scenario_id", "row_category_auto", "calc_tag", "row_category",
    "is_medic", "cpms_id", "study_site", "study_name", "Study_Arm",
    "Activity", "Visit", "posting_line_type_id", "posting_amount",
    "destination_bucket", "destination_entity", "cost_code", "sheet_name",
    "Visit_Label", "staff_group", "contract_cost", "Department",
    "Staff_Role", "activity_type", "time_required", "contract_price",
    "base_sum", "multiplier", "adjusted_amount", "residual",
    "is_residual_row", "adjusted_sum_check", "diff_check", "edge_key"
  ),
  addon_custom_activities = c(
    "id", "custom_activity_id", "cpms_id", "study_site", "study_name",
    "scenario_id", "Study_Arm", "Activity", "mode", "slot_num",
    "cost_centre", "amount", "created_by", "created_at"
  )
)

rids_canonicalize_names <- function(df, table) {
  canonical <- RIDS_CANONICAL_COLUMNS[[table]]
  if (is.null(canonical) || nrow(df) == 0 && ncol(df) == 0) {
    return(df)
  }

  lookup <- stats::setNames(canonical, tolower(canonical))
  current <- names(df)
  hit <- tolower(current) %in% names(lookup) & !(current %in% canonical)
  names(df)[hit] <- unname(lookup[tolower(current[hit])])
  df
}

rids_prepare_append <- function(con, df) {
  if (rids_is_postgres(con)) {
    names(df) <- tolower(names(df))
  }
  df
}

# NUL-byte scrub expression for legacy DuckDB data. PostgreSQL text cannot
# contain NUL bytes at all (and chr(0) errors there), so the scrub is a
# plain column reference on that dialect.
rids_scrub_nul_expr <- function(con, column, alias) {
  if (rids_is_postgres(con)) {
    sprintf("%s AS %s", column, alias)
  } else {
    sprintf("REPLACE(%s, chr(0), '') AS %s", column, alias)
  }
}
