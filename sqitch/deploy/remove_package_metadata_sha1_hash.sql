-- Deploy guix-data-service:remove_package_metadata_sha1_hash to pg

BEGIN;

ALTER TABLE package_metadata DROP COLUMN sha1_hash;

COMMIT;
