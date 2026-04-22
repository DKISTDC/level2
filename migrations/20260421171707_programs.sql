CREATE TABLE programs (
    _id SERIAL PRIMARY KEY,
    program_id text NOT NULL UNIQUE,
    experiment_id text NOT NULL,
    proposal_id text NOT NULL,

    instrument text NOT NULL,
    create_date timestamp without time zone NOT NULL,
    start_time timestamp without time zone NOT NULL,
    stokes text[] NOT NULL,
    spectral_lines text[] NOT NULL DEFAULT ARRAY[]::text[],
    embargo boolean NOT NULL DEFAULT false,

    status text NOT NULL
);

-- Indices -------------------------------------------------------

CREATE INDEX idx_programs_id ON programs(instrument_program_id);
CREATE INDEX idx_programs_proposal ON programs(proposal_id);
