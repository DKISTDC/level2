CREATE TABLE IF NOT EXISTS inversions (
  inversion_id TEXT PRIMARY KEY,

  program_id TEXT NOT NULL,
  created TIMESTAMP NOT NULL,

  download TIMESTAMP,

  calibration TIMESTAMP,
  calibration_url TEXT,

  inversion TIMESTAMP,
  inversion_software TEXT,

  post_process TIMESTAMP,

  publish TIMESTAMP
);

CREATE INDEX idx_inversions_program ON inversions(program_id);
