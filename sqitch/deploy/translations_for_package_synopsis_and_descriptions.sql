-- Deploy guix-data-service:translations_for_package_synopsis_and_descriptions to pg

BEGIN;

CREATE TABLE package_descriptions (
  id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  locale varchar NOT NULL,
  description varchar NOT NULL,
  UNIQUE (locale, description)
);

CREATE TABLE package_description_sets (
  id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  description_ids integer[] NOT NULL,
  UNIQUE (description_ids)
);

CREATE TABLE package_synopsis (
  id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  locale varchar NOT NULL,
  synopsis varchar NOT NULL,
  UNIQUE (locale, synopsis)
);

CREATE TABLE package_synopsis_sets (
  id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  synopsis_ids integer[] NOT NULL,
  UNIQUE (synopsis_ids)
);

ALTER TABLE package_metadata ADD COLUMN package_description_set_id integer REFERENCES package_description_sets (id);

ALTER TABLE package_metadata ADD COLUMN package_synopsis_set_id integer REFERENCES package_synopsis_sets (id);

INSERT INTO package_descriptions (locale, description)
SELECT DISTINCT 'en_US.utf8', description
  FROM package_metadata;

INSERT INTO package_description_sets (description_ids)
SELECT ARRAY[id] FROM package_descriptions;

INSERT INTO package_synopsis (locale, synopsis)
SELECT DISTINCT 'en_US.utf8', synopsis
  FROM package_metadata;

INSERT INTO package_synopsis_sets (synopsis_ids)
SELECT ARRAY[id] FROM package_synopsis;

UPDATE package_metadata
   SET package_description_set_id =
       package_description_sets.id
       FROM package_description_sets
       INNER JOIN package_descriptions
       ON package_description_sets.description_ids[1] = package_descriptions.id
       WHERE package_descriptions.description = package_metadata.description;

UPDATE package_metadata
   SET package_synopsis_set_id =
       package_synopsis_sets.id
       FROM package_synopsis_sets
       INNER JOIN package_synopsis
       ON package_synopsis_sets.synopsis_ids[1] = package_synopsis.id
       WHERE package_synopsis.synopsis = package_metadata.synopsis;

ALTER TABLE package_metadata DROP COLUMN description;

ALTER TABLE package_metadata DROP COLUMN synopsis;

COMMIT;
