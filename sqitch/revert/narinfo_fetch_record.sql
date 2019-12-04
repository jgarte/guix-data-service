-- Revert guix-data-service:narinfo_fetch_record from pg

BEGIN;

DROP TABLE narinfo_fetch_record;

COMMIT;
