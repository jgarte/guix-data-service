-- Revert guix-data-service:appschema from pg

BEGIN;

DROP SCHEMA guix_data_service;

COMMIT;
