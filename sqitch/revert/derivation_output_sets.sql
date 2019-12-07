-- Revert guix-data-service:derivation_output_sets from pg

BEGIN;

DROP TABLE derivations_by_output_details_set;
DROP TABLE derivation_output_details_sets;

COMMIT;
