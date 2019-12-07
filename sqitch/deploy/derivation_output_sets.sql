-- Deploy guix-data-service:derivation_output_sets to pg

BEGIN;

CREATE TABLE derivation_output_details_sets (
    id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    derivation_output_details_ids integer[] NOT NULL,
    UNIQUE (derivation_output_details_ids)
);

CREATE TABLE derivations_by_output_details_set (
    derivation_id integer REFERENCES derivations (id),
    derivation_output_details_set_id integer REFERENCES derivation_output_details_sets (id),
    PRIMARY KEY (derivation_id, derivation_output_details_set_id),
    UNIQUE (derivation_id)
);

CREATE INDEX derivations_by_output_details_set_id_idx
  ON derivations_by_output_details_set (derivation_output_details_set_id);

INSERT INTO derivation_output_details_sets (derivation_output_details_ids) (
  SELECT DISTINCT ARRAY_AGG(
                    derivation_output_details_id
                    ORDER BY derivation_output_details_id
                  )
  FROM derivation_outputs
  GROUP BY derivation_id
);

INSERT INTO derivations_by_output_details_set (
  SELECT derivation_id, derivation_output_details_sets.id
  FROM (
    SELECT derivation_id,
           derivation_output_details_ids
    FROM (
      SELECT derivation_id,
             ARRAY_AGG(
               derivation_output_details_id
               ORDER BY derivation_output_details_id
             ) AS derivation_output_details_ids
      FROM derivation_outputs
      GROUP BY derivation_id
    ) AS derivation_output_groups
  ) data
  INNER JOIN derivation_output_details_sets
    ON data.derivation_output_details_ids =
       derivation_output_details_sets.derivation_output_details_ids
);

COMMIT;
