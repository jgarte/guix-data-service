-- Revert guix-data-service:derivation_source_files_store_path_hash_index from pg

BEGIN;

DROP INDEX derivation_source_files_hash;

COMMIT;
