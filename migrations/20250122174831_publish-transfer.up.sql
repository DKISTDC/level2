-- Add up migration script here
ALTER TABLE inversions
ADD COLUMN publish_task_id TEXT;

