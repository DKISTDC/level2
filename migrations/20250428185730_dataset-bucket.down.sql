-- Add down migration script here
ALTER TABLE datasets
DROP COLUMN bucket;
