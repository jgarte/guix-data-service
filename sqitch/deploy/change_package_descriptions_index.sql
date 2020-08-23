-- Deploy guix-data-service:change_package_descriptions_index to pg

BEGIN;

ALTER TABLE package_descriptions DROP CONSTRAINT
  package_descriptions_locale_description_key;

CREATE UNIQUE INDEX package_descriptions_locale_description_key
  ON package_descriptions USING btree (locale, MD5(description));

COMMIT;
