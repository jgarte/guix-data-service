-- Deploy guix-data-service:some_indexes to pg

BEGIN;

CREATE INDEX nar_urls_nar_id ON nar_urls (nar_id);

CREATE INDEX load_new_guix_revision_jobs_unprocessed
  ON load_new_guix_revision_jobs (id) WHERE succeeded_at IS NULL;

CREATE INDEX guix_revision_lint_warnings_guix_revision_id
  ON guix_revision_lint_warnings (guix_revision_id);

COMMIT;
