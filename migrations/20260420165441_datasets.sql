CREATE TABLE datasets (
    _id SERIAL PRIMARY KEY,

    dataset_id text NOT NULL UNIQUE,
    product_id text NOT NULL,
    experiment_id text NOT NULL,
    proposal_id text NOT NULL,
    observing_program_id text NOT NULL,
    instrument_program_id text NOT NULL,

    scan_date timestamp without time zone NOT NULL,
    instrument text NOT NULL,
    stokes_parameters text[] NOT NULL,
    create_date timestamp without time zone NOT NULL,
    update_date timestamp without time zone NOT NULL,
    wavelength_min real NOT NULL,
    wavelength_max real NOT NULL,
    start_time timestamp without time zone NOT NULL,
    end_time timestamp without time zone NOT NULL,
    frame_count integer NOT NULL,
    experiment_description text NOT NULL,
    embargo timestamp without time zone,
    exposure_time real NOT NULL,
    bounding_box text,
    ao_locked integer NOT NULL,
    health json NOT NULL,
    gos_status json NOT NULL,
    polarimetric_accuracy json NOT NULL,
    fried_parameter json,
    light_level json NOT NULL,
    bucket text NOT NULL,
    spectral_lines text[] NOT NULL DEFAULT ARRAY[]::text[],
    browse_movie_url text NOT NULL
);

CREATE INDEX idx_datasets_dataset_id ON datasets(dataset_id);
CREATE INDEX idx_datasets_product_id ON datasets(product_id);
CREATE INDEX idx_datasets_program_id ON datasets(instrument_program_id);
