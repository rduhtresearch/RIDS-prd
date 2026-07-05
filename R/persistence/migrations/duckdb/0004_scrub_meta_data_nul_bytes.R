# One-time scrub of NUL bytes from historical meta_data rows. New writes are
# sanitized at the application layer (sanitize_text_value in R/utils/auth.r),
# so this only needs to run once. Errors are ignored, matching the original
# boot-time behavior (the scrub was best-effort inside a tryCatch).

migrate <- function(con) {
  tryCatch({
    DBI::dbExecute(con, "
      UPDATE meta_data
      SET
        cpms_id = REPLACE(cpms_id, chr(0), ''),
        study_site = REPLACE(study_site, chr(0), ''),
        scenario_id = REPLACE(scenario_id, chr(0), ''),
        edge_id = REPLACE(edge_id, chr(0), ''),
        study_name = REPLACE(study_name, chr(0), ''),
        notes = REPLACE(notes, chr(0), ''),
        uploaded_by = REPLACE(uploaded_by, chr(0), ''),
        original_filename = REPLACE(original_filename, chr(0), ''),
        saved_file_path = REPLACE(saved_file_path, chr(0), ''),
        edge_zip_path = REPLACE(edge_zip_path, chr(0), '')
      WHERE
        strpos(cpms_id, chr(0)) > 0 OR
        strpos(study_site, chr(0)) > 0 OR
        strpos(scenario_id, chr(0)) > 0 OR
        strpos(edge_id, chr(0)) > 0 OR
        strpos(study_name, chr(0)) > 0 OR
        strpos(notes, chr(0)) > 0 OR
        strpos(uploaded_by, chr(0)) > 0 OR
        strpos(original_filename, chr(0)) > 0 OR
        strpos(saved_file_path, chr(0)) > 0 OR
        strpos(edge_zip_path, chr(0)) > 0;
    ")
  }, error = function(e) NULL)

  invisible(TRUE)
}
