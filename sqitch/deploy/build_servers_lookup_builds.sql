-- Deploy guix-data-service:build_servers_lookup_builds to pg

BEGIN;

ALTER TABLE build_servers ADD COLUMN lookup_builds boolean NOT NULL DEFAULT TRUE;

COMMIT;
