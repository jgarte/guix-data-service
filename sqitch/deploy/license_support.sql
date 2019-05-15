-- Deploy guix-data-service:license_support to pg

BEGIN;

CREATE TABLE licenses (
    id integer NOT NULL GENERATED ALWAYS AS IDENTITY,
    name character varying NOT NULL,
    uri character varying,
    comment character varying,
    PRIMARY KEY(id),
    UNIQUE (name, uri, comment)
);

CREATE TABLE license_sets (
    id integer GENERATED ALWAYS AS IDENTITY,
    license_ids integer[] NOT NULL,
    PRIMARY KEY(license_ids),
    UNIQUE (id)
);

ALTER TABLE package_metadata ADD COLUMN license_set_id integer REFERENCES license_sets(id);

ALTER TABLE package_metadata DROP CONSTRAINT synopsis_description_home_page_location_id;

ALTER TABLE package_metadata ADD CONSTRAINT package_metadata_unique_fields UNIQUE (synopsis, description, home_page, location_id, license_set_id);

COMMIT;
