-- Revert guix-data-service:sort_out_duplicate_builds from pg

BEGIN;

DROP INDEX builds_build_server_id_derivation_file_name_idx;

COMMIT;
