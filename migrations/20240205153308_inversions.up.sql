CREATE TABLE IF NOT EXISTS inversions (
  inversion_id TEXT PRIMARY KEY,

  program_id TEXT NOT NULL,
  created TIMESTAMP NOT NULL,
  updated TIMESTAMP NOT NULL,

  download TIMESTAMP,
  download_task_id TEXT,
  download_datasets TEXT[],

  preprocess TIMESTAMP,
  preprocess_software TEXT,

  upload TIMESTAMP,
  upload_task_id TEXT,

  inversion TIMESTAMP,
  inversion_software TEXT,

  generate TIMESTAMP,
  generate_task_id TEXT,
  generate_task_completed TIMESTAMP,
  generate_l1_frame_dir TEXT,

  publish TIMESTAMP
);

CREATE INDEX idx_inversions_program ON inversions(program_id);
