-- Verify guix-data-service:load_new_guix_revision_job_events on pg

BEGIN;

SELECT id, job_id, event, occurred_at
  FROM load_new_guix_revision_job_events WHERE FALSE;

ROLLBACK;
