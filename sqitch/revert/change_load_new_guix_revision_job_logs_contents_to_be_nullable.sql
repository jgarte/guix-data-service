-- Revert guix-data-service:change_load_new_guix_revision_job_logs_contents_to_be_nullable from pg

BEGIN;

ALTER TABLE load_new_guix_revision_job_logs ALTER contents SET NOT NULL;

COMMIT;
