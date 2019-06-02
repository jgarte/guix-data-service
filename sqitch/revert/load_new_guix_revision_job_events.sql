-- Revert guix-data-service:load_new_guix_revision_job_events from pg

BEGIN;

DROP TABLE load_new_guix_revision_job_events;

ALTER TABLE load_new_guix_revision_jobs
  DROP CONSTRAINT load_new_guix_revision_jobs_id;

DROP TYPE IF EXISTS job_event;

COMMIT;
