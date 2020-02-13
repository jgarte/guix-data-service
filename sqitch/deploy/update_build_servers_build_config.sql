-- Deploy guix-data-service:update_build_servers_build_config to pg

BEGIN;

UPDATE build_servers_build_config SET target = '' WHERE system = target;

COMMIT;
