-- Deploy guix-data-service:load_new_guix_revision_job_logs to pg

BEGIN;

CREATE TABLE load_new_guix_revision_job_log_parts (
    id integer NOT NULL,
    job_id integer NOT NULL,
    contents text NOT NULL
);

CREATE TABLE load_new_guix_revision_job_logs (
    job_id integer PRIMARY KEY REFERENCES load_new_guix_revision_jobs (id),
    contents text NOT NULL
);

COMMIT;
