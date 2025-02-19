ALTER TABLE inversions
DROP COLUMN notes,
DROP COLUMN deleted,
ADD COLUMN download TIMESTAMP DEFAULT NOW(),
ADD COLUMN download_task_id TEXT DEFAULT 'migration-fake-task',
ADD COLUMN upload TIMESTAMP DEFAULT NOW(),
ADD COLUMN upload_task_id TEXT DEFAULT 'migration-fake-task',
ADD COLUMN inversion TIMESTAMP DEFAULT NOW(),
ADD COLUMN publish_task_id TEXT DEFAULT 'migration-fake-task',
ADD COLUMN generate_task_id TEXT DEFAULT 'migration-fake-task',
ADD COLUMN preprocess TIMESTAMP DEFAULT NOW(),
ADD COLUMN preprocess_software TEXT DEFAULT 'fake-preprocess-commit';

/*DROP COLUMN upload_profile_fit,
DROP COLUMN upload_profile_orig,
DROP COLUMN upload_quantities,
*/
