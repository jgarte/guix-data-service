-- Deploy guix-data-service:package_derivations_not_null_hash_index to pg

BEGIN;

CREATE INDEX derivation_output_details_id_hash_not_null_idx ON derivation_output_details (id) WHERE hash IS NOT NULL;

COMMIT;
