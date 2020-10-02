-- Deploy guix-data-service:add_derivation_sources_derivation_source_file_id_index to pg

BEGIN;

CREATE INDEX derivation_sources_derivation_source_file_id_idx
ON derivation_sources (derivation_source_file_id);

COMMIT;
