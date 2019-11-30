-- Deploy guix-data-service:nar_related_tables to pg

BEGIN;

CREATE TABLE nars (
    id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    store_path varchar NOT NULL,
    hash_algorithm varchar NOT NULL,
    hash varchar NOT NULL,
    size integer NOT NULL,
    system varchar,
    deriver varchar
);

CREATE TABLE nar_urls (
    nar_id integer NOT NULL REFERENCES nars(id),
    url varchar PRIMARY KEY,
    compression varchar NOT NULL,
    file_size integer NOT NULL
);

CREATE TABLE nar_references (
    nar_id integer NOT NULL REFERENCES nars(id),
    reference varchar NOT NULL
);

CREATE TABLE narinfo_signature_public_keys (
    id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    sexp_json jsonb NOT NULL,
    UNIQUE (sexp_json)
);

CREATE TABLE narinfo_signature_data (
    id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    version integer NOT NULL,
    host_name varchar NOT NULL,
    data_hash varchar NOT NULL,
    data_hash_algorithm varchar NOT NULL,
    data_json jsonb NOT NULL,
    sig_val_json jsonb NOT NULL,
    narinfo_signature_public_key_id integer NOT NULL REFERENCES narinfo_signature_public_keys(id),
    narinfo_body varchar NOT NULL,
    narinfo_signature_line varchar NOT NULL,
    UNIQUE (narinfo_signature_line)
);

CREATE TABLE narinfo_signatures (
    nar_id integer NOT NULL REFERENCES nars(id),
    narinfo_signature_data_id integer NOT NULL REFERENCES narinfo_signature_data(id),
    UNIQUE (nar_id, narinfo_signature_data_id)
);

COMMIT;
