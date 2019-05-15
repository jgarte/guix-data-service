-- Revert guix-data-service:license_support from pg

BEGIN;

DROP TABLE licenses;

DROP TABLE license_sets;

COMMIT;
