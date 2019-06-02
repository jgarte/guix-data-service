-- Deploy guix-data-service:dates_to_load_new_guix_revision_jobs to pg
-- requires: initial_import

BEGIN;

ALTER TABLE load_new_guix_revision_jobs
  ADD COLUMN created_at timestamp without time zone DEFAULT clock_timestamp();

ALTER TABLE load_new_guix_revision_jobs
  ADD COLUMN succeeded_at timestamp without time zone;

COMMIT;
