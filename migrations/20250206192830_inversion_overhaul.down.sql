ALTER TABLE inversions
ADD COLUMN download TIMESTAMP DEFAULT NOW(),
ADD COLUMN download_task_id TEXT DEFAULT "migration-fake-task",
DROP COLUMN upload_profile_fit,
DROP COLUMN upload_profile_orig,
DROP COLUMN upload_quantities,
DROP COLUMN notes,
ADD COLUMN inversion TIMESTAMP DEFAULT NOW(),
ADD COLUMN publish_task_id DEFAULT "migration-fake-task",
ADD COLUMN generate_task_id DEFAULT "migration-fake-task";
