-- Deploy guix-data-service:add_index_on_builds_derivation_file_name to pg

BEGIN;

CREATE INDEX builds_derivation_file_name ON builds(derivation_file_name);

COMMIT;
