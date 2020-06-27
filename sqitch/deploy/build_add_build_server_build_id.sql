-- Deploy guix-data-service:build_add_build_server_build_id to pg

BEGIN;

ALTER TABLE builds ADD COLUMN build_server_build_id varchar;

COMMIT;
