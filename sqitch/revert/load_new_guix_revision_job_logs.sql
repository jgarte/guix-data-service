-- Revert guix-data-service:load_new_guix_revision_job_logs from pg

BEGIN;

DROP TABLE load_new_guix_revision_job_log_parts;
DROP TABLE load_new_guix_revision_job_logs;

COMMIT;
