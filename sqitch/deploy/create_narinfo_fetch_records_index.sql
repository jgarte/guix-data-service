-- Deploy guix-data-service:create_narinfo_fetch_records_index to pg

BEGIN;

CREATE INDEX narinfo_fetch_records_narinfo_signature_data_id ON
  narinfo_fetch_records (narinfo_signature_data_id);

COMMIT;
