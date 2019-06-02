-- Revert guix-data-service:dates_to_load_new_guix_revision_jobs from pg

BEGIN;

ALTER TABLE load_new_guix_revision_jobs DROP COLUMN created_at;
ALTER TABLE load_new_guix_revision_jobs DROP COLUMN succeeded_at;

COMMIT;
