DELETE FROM inversions WHERE download is NULL;
DELETE FROM inversions WHERE upload is NULL;

-- Add UP migration script here
ALTER TABLE inversions
ADD COLUMN notes TEXT NOT NULL DEFAULT '',
ADD COLUMN deleted TIMESTAMP,
DROP COLUMN download,
DROP COLUMN download_task_id,
DROP COLUMN upload,
DROP COLUMN upload_task_id,
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
