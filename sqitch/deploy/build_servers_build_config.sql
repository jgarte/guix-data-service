-- Deploy guix-data-service:build_servers_build_config to pg

BEGIN;

CREATE TABLE build_servers_build_config (
    build_server_id integer REFERENCES build_servers(id),
    system varchar NOT NULL,
    target varchar NOT NULL,
    PRIMARY KEY (build_server_id, system, target)
);

COMMIT;
