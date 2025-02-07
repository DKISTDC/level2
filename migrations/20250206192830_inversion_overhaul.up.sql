-- Add UP migration script here
ALTER TABLE inversions
DROP COLUMN download,
DROP COLUMN download_task_id,
ADD COLUMN upload_profile_fit TIMESTAMP,
ADD COLUMN upload_profile_orig TIMESTAMP,
ADD COLUMN upload_quantities TIMESTAMP,
ADD COLUMN notes TEXT NOT NULL DEFAULT "",
DROP COLUMN inversion,
DROP COLUMN publish_task_id,
DROP COLUMN generate_task_id;
