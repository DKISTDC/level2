CREATE TABLE IF NOT EXISTS datasets (
  _id SERIAL PRIMARY KEY,

  dataset_id TEXT NOT NULL,
  latest BOOL NOT NULL,
  scan_date TIMESTAMP NOT NULL,

  observing_program_id TEXT NOT NULL,
  instrument_program_id TEXT NOT NULL,
  instrument TEXT NOT NULL,
  stokes_parameters TEXT[] NOT NULL,
  create_date TIMESTAMP NOT NULL,
  update_date TIMESTAMP NOT NULL,
  wavelength_min REAL NOT NULL,
  wavelength_max REAL NOT NULL,
  start_time TIMESTAMP NOT NULL,
  end_time TIMESTAMP NOT NULL,
  frame_count INTEGER NOT NULL,
  primary_experiment_id TEXT NOT NULL,
  primary_proposal_id TEXT NOT NULL,
  experiment_description TEXT NOT NULL,
  exposure_time REAL NOT NULL,
  bounding_box TEXT
  -- health JSON NOT NULL,
  -- gos_status JSON NOT NULL,
  -- ao_locked INTEGER NOT NULL
);

CREATE INDEX idx_datasets_latest ON datasets(latest);
CREATE INDEX idx_datasets_dataset_id ON datasets(dataset_id);
CREATE INDEX idx_datasets_program_id ON datasets(instrument_program_id);
