-- Add up migration script here
ALTER TABLE datasets
ADD COLUMN bucket TEXT NOT NULL DEFAULT 'public';
