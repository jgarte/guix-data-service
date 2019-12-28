-- Revert guix-data-service:add_derivation_source_file_nars from pg

BEGIN;

DROP TABLE derivation_source_file_nars;

COMMIT;
