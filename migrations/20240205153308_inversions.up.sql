CREATE TABLE IF NOT EXISTS inversions (
  inversion_id TEXT PRIMARY KEY,

  program_id TEXT NOT NULL,
  created TIMESTAMP NOT NULL,

  download TIMESTAMP,
  download_task_id TEXT,

  preprocess TIMESTAMP,
  preprocess_software TEXT,

  upload TIMESTAMP,
  upload_task_id TEXT,

  inversion TIMESTAMP,
  inversion_software TEXT,

  generate TIMESTAMP,

  publish TIMESTAMP
);

CREATE INDEX idx_inversions_program ON inversions(program_id);
