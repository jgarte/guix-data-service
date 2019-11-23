-- Revert guix-data-service:build_server_token_seeds from pg

BEGIN;

DROP TABLE build_server_token_seeds;

COMMIT;
