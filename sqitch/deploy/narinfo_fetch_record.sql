-- Deploy guix-data-service:narinfo_fetch_record to pg

BEGIN;

CREATE TABLE narinfo_fetch_records (
    id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    narinfo_signature_data_id integer NOT NULL REFERENCES narinfo_signature_data(id),
    build_server_id integer NOT NULL REFERENCES build_servers(id),
    fetched_at timestamp without time zone DEFAULT clock_timestamp()
);

COMMIT;
