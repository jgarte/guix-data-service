-- Deploy guix-data-service:increase_derivation_inputs_statistics_targets to pg

BEGIN;

ALTER TABLE derivation_inputs ALTER COLUMN derivation_id SET STATISTICS 10000;
ALTER TABLE derivation_inputs ALTER COLUMN derivation_output_id SET STATISTICS 10000;

COMMIT;
