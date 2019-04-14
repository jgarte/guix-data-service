-- Verify guix-data-service:buildstatus_enum on pg

BEGIN;

SELECT pg_catalog.has_type_privilege('guix_data_service.buildstatus', 'usage');

ROLLBACK;
