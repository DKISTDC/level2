CREATE TABLE IF NOT EXISTS provenance_queued (
  _id SERIAL PRIMARY KEY,

  program_id TEXT NOT NULL,
  completed TIMESTAMP NOT NULL
);

CREATE INDEX idx_prov_queued_program ON provenance_queued(program_id);

CREATE TABLE IF NOT EXISTS provenance_inverted (
  _id SERIAL PRIMARY KEY,

  program_id TEXT NOT NULL,
  completed TIMESTAMP NOT NULL
);

CREATE INDEX idx_prov_inverted_program ON provenance_inverted(program_id);

