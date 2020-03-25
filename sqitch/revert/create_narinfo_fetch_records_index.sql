-- Revert guix-data-service:create_narinfo_fetch_records_index from pg

BEGIN;

DROP INDEX narinfo_fetch_records_narinfo_signature_data_id;

COMMIT;
