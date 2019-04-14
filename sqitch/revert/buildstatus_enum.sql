-- Revert guix-data-service:buildstatus_enum from pg

BEGIN;

DROP TYPE guix_data_service.buildstatus;

COMMIT;
