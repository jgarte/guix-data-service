-- Deploy guix-data-service:build_status_nullable_timestamp to pg

BEGIN;

ALTER TABLE build_status ALTER COLUMN timestamp DROP NOT NULl;

COMMIT;
