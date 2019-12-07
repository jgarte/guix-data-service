-- Deploy guix-data-service:builds_add_derivation_output_details_set_id to pg

BEGIN;

ALTER TABLE builds
  ADD COLUMN derivation_output_details_set_id integer
  NULL
  DEFAULT NULL
  REFERENCES derivation_output_details_sets (id);

UPDATE builds SET derivation_output_details_set_id = (
  SELECT derivations_by_output_details_set.derivation_output_details_set_id
  FROM derivations_by_output_details_set
  INNER JOIN derivations
    ON derivations.file_name = builds.derivation_file_name
  WHERE derivations_by_output_details_set.derivation_id = derivations.id
);

CREATE INDEX builds_derivation_output_details_set_id ON
  builds (derivation_output_details_set_id);

COMMIT;
