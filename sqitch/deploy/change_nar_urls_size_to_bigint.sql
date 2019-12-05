-- Deploy guix-data-service:change_nar_urls_size_to_bigint to pg

BEGIN;

ALTER TABLE nar_urls ALTER COLUMN file_size TYPE bigint USING file_size::bigint;

COMMIT;
