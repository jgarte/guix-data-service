-- Revert guix-data-service:add_index_on_builds_derivation_file_name from pg

BEGIN;

DROP INDEX builds_derivation_file_name;

COMMIT;
