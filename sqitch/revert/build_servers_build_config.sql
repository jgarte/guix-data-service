-- Revert guix-data-service:build_servers_build_config from pg

BEGIN;

DROP TABLE build_servers_build_config;

COMMIT;
