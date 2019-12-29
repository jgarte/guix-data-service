-- Deploy guix-data-service:derivation_source_files_store_path_hash_index to pg

BEGIN;

CREATE INDEX derivation_source_files_hash
  ON derivation_source_files (substring(store_path from 12 for 32));

COMMIT;
