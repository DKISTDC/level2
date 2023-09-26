CREATE TABLE IF NOT EXISTS datasets (
  dataset_id serial PRIMARY KEY,
  observing_program_id TEXT NOT NULL,
  stokes_parameters TEXT[] NOT NULL,
  create_date TIMESTAMP NOT NULL,
  wavelength_min REAL NOT NULL,
  wavelength_max REAL NOT NULL
);

CREATE INDEX idx_datasets_program_id ON datasets(observing_program_id)
