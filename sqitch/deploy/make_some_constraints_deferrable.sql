-- Deploy guix-data-service:make_some_constraints_deferrable to pg

BEGIN;

ALTER TABLE derivations_by_output_details_set
    ALTER CONSTRAINT derivations_by_output_details_set_derivation_id_fkey DEFERRABLE INITIALLY IMMEDIATE;

COMMIT;
