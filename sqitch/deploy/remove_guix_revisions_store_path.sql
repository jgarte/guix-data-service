-- Deploy guix-data-service:remove_guix_revisions_store_path to pg

BEGIN;

ALTER TABLE guix_revisions DROP COLUMN store_path;

COMMIT;
