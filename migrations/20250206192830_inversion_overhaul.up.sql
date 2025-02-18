-- Add UP migration script here
ALTER TABLE inversions
DROP COLUMN download,
DROP COLUMN download_task_id,
DROP COLUMN upload,
DROP COLUMN upload_task_id,
ADD COLUMN notes TEXT NOT NULL DEFAULT '',
DROP COLUMN inversion,
DROP COLUMN publish_task_id,
DROP COLUMN generate_task_id,
DROP COLUMN preprocess,
DROP COLUMN preprocess_software;

/*
ADD COLUMN upload_profile_fit TIMESTAMP,
ADD COLUMN upload_profile_orig TIMESTAMP,
ADD COLUMN upload_quantities TIMESTAMP,
*/
