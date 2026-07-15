CREATE SEQUENCE IF NOT EXISTS template_version_id_seq START 1;

CREATE TABLE IF NOT EXISTS template_versions (
  version_id          INTEGER PRIMARY KEY DEFAULT nextval('template_version_id_seq'),
  study_id            INTEGER NOT NULL,
  version_number      INTEGER NOT NULL,
  version_type        VARCHAR NOT NULL CHECK (
    version_type IN ('baseline', 'substantial_amendment', 'distribution_amendment')
  ),
  effective_from_date DATE,
  status              VARCHAR NOT NULL DEFAULT 'processing' CHECK (
    status IN ('processing', 'active', 'archived')
  ),
  uploaded_by         VARCHAR,
  upload_timestamp    TIMESTAMP DEFAULT current_timestamp,
  notes               VARCHAR,
  original_filename   VARCHAR,
  saved_file_path     VARCHAR,
  edge_zip_path       VARCHAR,
  CHECK (
    (version_type = 'baseline' AND effective_from_date IS NULL) OR
    (version_type <> 'baseline' AND effective_from_date IS NOT NULL)
  )
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_template_versions_study_number
  ON template_versions (study_id, version_number);

CREATE INDEX IF NOT EXISTS idx_template_versions_effective_date
  ON template_versions (study_id, status, effective_from_date);

INSERT INTO template_versions (
  study_id, version_number, version_type, effective_from_date, status,
  uploaded_by, upload_timestamp, notes, original_filename, saved_file_path,
  edge_zip_path
)
SELECT
  m.id, 1, 'baseline', NULL, 'active', m.uploaded_by, m.upload_timestamp,
  m.notes, m.original_filename, m.saved_file_path, m.edge_zip_path
FROM meta_data m
WHERE NOT EXISTS (
  SELECT 1 FROM template_versions tv WHERE tv.study_id = m.id
);

ALTER TABLE ict_costing_tbl ADD COLUMN IF NOT EXISTS version_id INTEGER;
ALTER TABLE posting_lines ADD COLUMN IF NOT EXISTS version_id INTEGER;
ALTER TABLE addon_custom_activities ADD COLUMN IF NOT EXISTS version_id INTEGER;

UPDATE ict_costing_tbl ict
SET version_id = tv.version_id
FROM meta_data m
JOIN template_versions tv ON tv.study_id = m.id AND tv.version_type = 'baseline'
WHERE ict.version_id IS NULL
  AND ict.CPMS_ID = m.cpms_id
  AND ict.study_site = m.study_site
  AND ict.scenario_id = m.scenario_id;

UPDATE posting_lines pl
SET version_id = tv.version_id
FROM meta_data m
JOIN template_versions tv ON tv.study_id = m.id AND tv.version_type = 'baseline'
WHERE pl.version_id IS NULL
  AND pl.cpms_id = m.cpms_id
  AND pl.study_site = m.study_site
  AND pl.scenario_id = m.scenario_id;

UPDATE addon_custom_activities ca
SET version_id = tv.version_id
FROM meta_data m
JOIN template_versions tv ON tv.study_id = m.id AND tv.version_type = 'baseline'
WHERE ca.version_id IS NULL
  AND ca.cpms_id = m.cpms_id
  AND ca.study_site = m.study_site
  AND ca.scenario_id = m.scenario_id;

CREATE INDEX IF NOT EXISTS idx_ict_costing_version ON ict_costing_tbl (version_id);
CREATE INDEX IF NOT EXISTS idx_posting_lines_version ON posting_lines (version_id);
CREATE INDEX IF NOT EXISTS idx_addon_ca_version ON addon_custom_activities (version_id);
