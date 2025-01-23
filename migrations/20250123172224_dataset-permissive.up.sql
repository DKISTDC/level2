DELETE FROM datasets WHERE latest = FALSE;

ALTER TABLE datasets
ALTER COLUMN fried_parameter DROP NOT NULL;

ALTER TABLE datasets
DROP COLUMN latest;
