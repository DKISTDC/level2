DELETE FROM datasets WHERE fried_parameter IS NULL;

ALTER TABLE datasets
ALTER COLUMN fried_parameter SET NOT NULL;

ALTER TABLE datasets
ADD COLUMN latest BOOLEAN DEFAULT TRUE;
