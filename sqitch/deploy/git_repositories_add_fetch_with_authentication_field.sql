-- Deploy guix-data-service:git_repositories_add_fetch_with_authentication_field to pg

BEGIN;

ALTER TABLE git_repositories
  ADD COLUMN fetch_with_authentication boolean NOT NULL DEFAULT TRUE;

COMMIT;
