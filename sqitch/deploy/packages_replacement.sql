-- Deploy guix-data-service:packages_replacement to pg

BEGIN;

ALTER TABLE packages
  ADD COLUMN replacement_package_id integer REFERENCES packages (id);

ALTER TABLE packages DROP CONSTRAINT packages_pkey;
ALTER TABLE packages ADD PRIMARY KEY (id);

CREATE UNIQUE INDEX packages_not_null_replacement_package_id_idx
  ON packages (name, version, package_metadata_id, replacement_package_id);
CREATE UNIQUE INDEX packages_null_replacement_package_id_idx
  ON packages (name, version, package_metadata_id) WHERE replacement_package_id IS NULL;

COMMIT;
