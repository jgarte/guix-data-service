-- Deploy guix-data-service:add_some_database_indexes to pg

BEGIN;

CREATE INDEX ON derivation_outputs (derivation_id);
CREATE INDEX ON derivation_outputs (derivation_output_details_id);
CREATE INDEX ON nars (store_path);
CREATE INDEX ON package_derivations (package_id);

COMMIT;
