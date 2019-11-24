-- Deploy guix-data-service:rework_builds to pg

BEGIN;

DROP TABLE build_status;
DROP TABLE builds;

CREATE TABLE builds (
    id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    build_server_id integer NOT NULL REFERENCES build_servers(id),
    derivation_file_name varchar NOT NULL
);

CREATE TABLE build_status (
    id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    build_id integer NOT NULL REFERENCES builds(id),
    "timestamp" timestamp without time zone DEFAULT clock_timestamp() NOT NULL,
    status guix_data_service.buildstatus NOT NULL
);

COMMIT;
