CREATE TABLE IF NOT EXISTS datasets (
  dataset_id TEXT PRIMARY KEY,
  observing_program_execution_id TEXT NOT NULL,
  instrument_program_execution_id TEXT NOT NULL,
  instrument TEXT NOT NULL,
  stokes_parameters TEXT[] NOT NULL,
  create_date TIMESTAMP NOT NULL,
  wavelength_min REAL NOT NULL,
  wavelength_max REAL NOT NULL,
  scan_date TIMESTAMP NOT NULL,
  start_time TIMESTAMP NOT NULL,
  end_time TIMESTAMP NOT NULL,
  frame_count INTEGER NOT NULL,
  primary_experiment_id TEXT NOT NULL,
  primary_proposal_id TEXT NOT NULL,
  input_observe_frames_id TEXT NOT NULL,
  experiment_description TEXT NOT NULL,
  exposure_time REAL NOT NULL,
  bounding_box TEXT
);

-- CREATE INDEX idx_datasets_program_id ON datasets(observing_program_id)
