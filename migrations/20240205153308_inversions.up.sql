CREATE TABLE IF NOT EXISTS inversions (
  inversion_id TEXT PRIMARY KEY,

  program_id TEXT NOT NULL,
  created TIMESTAMP NOT NULL,

  download TIMESTAMP,
  download_task_id TEXT,

  calibration TIMESTAMP,
  calibration_software TEXT,

  inversion TIMESTAMP,
  inversion_software TEXT,

  post_process TIMESTAMP,

  publish TIMESTAMP
);

CREATE INDEX idx_inversions_program ON inversions(program_id);
