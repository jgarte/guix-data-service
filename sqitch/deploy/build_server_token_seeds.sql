-- Deploy guix-data-service:build_server_token_seeds to pg

BEGIN;

CREATE TABLE build_server_token_seeds (
    build_server_id integer REFERENCES build_servers(id),
    token_seed varchar NOT NULL,
    PRIMARY KEY (build_server_id, token_seed)
);

COMMIT;
