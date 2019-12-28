-- Deploy guix-data-service:add_derivation_source_file_nars to pg

BEGIN;

CREATE TABLE derivation_source_file_nars (
    derivation_source_file_id integer PRIMARY KEY REFERENCES derivation_source_files (id),
    compression varchar NOT NULL,
    hash_algorithm varchar NOT NULL,
    hash varchar NOT NULL,
    uncompressed_size integer NOT NULL,
    data bytea NOT NULL
);

COMMIT;
