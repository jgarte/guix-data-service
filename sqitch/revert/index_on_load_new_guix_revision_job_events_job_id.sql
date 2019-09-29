-- Revert guix-data-service:index_on_load_new_guix_revision_job_events_job_id from pg

BEGIN;

DROP INDEX load_new_guix_revision_job_events_job_id;

COMMIT;
