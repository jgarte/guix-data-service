-- Revert guix-data-service:system_test_tables from pg

BEGIN;

DROP TABLE guix_revision_system_test_derivations;
DROP TABLE system_tests;

COMMIT;
