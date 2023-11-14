ALTER TABLE datasets
  DROP COLUMN IF EXISTS health,
  DROP COLUMN IF EXISTS gos_status,
  DROP COLUMN IF EXISTS ao_locked,
  DROP COLUMN IF EXISTS polarimetric_accuracy,
  DROP COLUMN IF EXISTS fried_parameter,
  DROP COLUMN IF EXISTS light_level;
