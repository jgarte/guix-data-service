-- Verify guix-data-service:appschema on pg

BEGIN;

SELECT pg_catalog.has_schema_privilege('guix_data_service', 'usage');

ROLLBACK;
