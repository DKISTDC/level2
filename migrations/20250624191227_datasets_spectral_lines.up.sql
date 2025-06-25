ALTER TABLE datasets
ADD COLUMN spectral_lines TEXT[] NOT NULL DEFAULT ARRAY[]::text[];

ALTER TABLE datasets
ADD CONSTRAINT unique_id UNIQUE (dataset_id);
