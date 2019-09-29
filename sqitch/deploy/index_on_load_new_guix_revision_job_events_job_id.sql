-- Deploy guix-data-service:index_on_load_new_guix_revision_job_events_job_id to pg

BEGIN;

CREATE INDEX load_new_guix_revision_job_events_job_id ON load_new_guix_revision_job_events USING btree (job_id);

COMMIT;
