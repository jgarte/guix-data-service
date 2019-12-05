-- Deploy guix-data-service:change_nars_size_to_bigint to pg

BEGIN;

ALTER TABLE nars ALTER COLUMN size TYPE bigint USING size::bigint;

COMMIT;
