-- Deploy guix-data-service:derivation_output_details_sets_derivation_output_details_ids_index to pg

BEGIN;

CREATE INDEX derivation_output_details_sets_gin_idx
  ON derivation_output_details_sets
  USING GIN (derivation_output_details_ids);

COMMIT;
