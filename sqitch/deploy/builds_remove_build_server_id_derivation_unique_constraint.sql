-- Deploy guix-data-service:builds_remove_build_server_id_derivation_unique_constraint to pg

BEGIN;

DROP INDEX builds_build_server_id_derivation_file_name_idx;

COMMIT;
