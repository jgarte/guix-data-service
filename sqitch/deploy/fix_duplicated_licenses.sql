-- Deploy guix-data-service:fix_duplicated_licenses to pg

BEGIN;

SET CONSTRAINTS ALL DEFERRED;

-- Remove unique constraint from license_sets

ALTER TABLE license_sets DROP CONSTRAINT license_sets_pkey;

-- Change all license sets to refer to canonical licenses

UPDATE license_sets AS master SET license_ids = ARRAY(
  SELECT new_licenses_2.id FROM (
    SELECT a.elem AS id, a.nr AS index
    FROM license_sets, unnest(license_sets.license_ids)
    WITH ORDINALITY a(elem, nr)
    WHERE id = master.id
  ) AS old_licenses
  INNER JOIN (
    SELECT licenses.id AS old_id, new_licenses.*
    FROM licenses INNER JOIN (
      SELECT MIN(id) AS id, name, uri, comment
      FROM licenses
      GROUP BY name, uri, comment
      ORDER BY name
    ) AS new_licenses
    ON licenses.name = new_licenses.name AND
      (licenses.uri = new_licenses.uri OR (
        licenses.uri IS NULL AND new_licenses.uri IS NULL
      )) AND (
        licenses.comment = new_licenses.comment OR (
      licenses.comment IS NULL AND new_licenses.comment IS NULL
      ))
  ) AS new_licenses_2
  ON old_licenses.id = new_licenses_2.old_id
  ORDER BY old_licenses.index);

-- Remove unique constraint from package_metadata

ALTER TABLE package_metadata DROP CONSTRAINT package_metadata_unique_fields;

-- Update package_metadata to refer to canonical license_sets

WITH data AS (
  SELECT MIN(id) AS id, ARRAY_AGG(id) AS old_ids
  FROM license_sets
  GROUP BY license_ids
)
UPDATE package_metadata AS master
SET license_set_id = data.id
FROM data
WHERE license_set_id = ANY(data.old_ids);

-- Remove unique constraint from packages

ALTER TABLE packages DROP CONSTRAINT packages_pkey;

-- Update packages to refer to canonical package_metadata entries

WITH data AS (
  SELECT MIN(package_metadata.id) AS id, ARRAY_AGG(package_metadata.id) AS old_ids
  FROM package_metadata
  GROUP BY package_metadata.synopsis, package_metadata.description,
  package_metadata.home_page, package_metadata.location_id,
  package_metadata.license_set_id
  HAVING COUNT(package_metadata.id) > 1
)
UPDATE packages SET package_metadata_id = data.id
FROM data
WHERE package_metadata_id = ANY(data.old_ids);

-- Remove unique constraint from package_derivations

ALTER TABLE package_derivations DROP CONSTRAINT package_derivations_pkey;

-- Update package_derivations to refer to canonical packages entries

WITH data AS (
  SELECT unnest(old_ids) AS old, id FROM (
    SELECT MIN(packages.id) AS id, ARRAY_AGG(packages.id) AS old_ids
    FROM packages
    GROUP BY name, version, package_metadata_id
    HAVING COUNT(id) > 1
  ) AS d2
)
UPDATE package_derivations SET package_id = data.id
FROM data
WHERE package_id = data.old;

-- Update guix_revision_package_derivations to refer to canonical
-- package_derivations entries

WITH data AS (
  SELECT unnest(old_ids) AS old, id FROM (
    SELECT MIN(package_derivations.id) AS id, ARRAY_AGG(package_derivations.id) AS old_ids
    FROM package_derivations
    GROUP BY package_id, derivation_id, system, target
    HAVING COUNT(id) > 1
  ) AS d2
)
UPDATE guix_revision_package_derivations SET package_derivation_id = data.id
FROM data
WHERE package_derivation_id = data.old;

-- Drop the foreign key constraint as an attempt to speed up deleting from
-- package_derivations.

ALTER TABLE guix_revision_package_derivations
  DROP CONSTRAINT guix_revision_package_derivations_package_derivation_id_fkey;

-- Delete non-canonical package_dervations entries

DELETE FROM package_derivations AS pd WHERE id NOT IN (
  SELECT MIN(id)
  FROM package_derivations
  GROUP BY (
    package_id,
    derivation_id,
    system,
    target
  )
);

-- Reinstate the deleted constraint

ALTER TABLE guix_revision_package_derivations
  ADD CONSTRAINT guix_revision_package_derivations_package_derivation_id_fkey
  FOREIGN KEY (package_derivation_id) REFERENCES package_derivations(id);

-- Delete non-canonical packages entries

DELETE FROM packages AS p WHERE id NOT IN (
  SELECT MIN(id)
  FROM packages
  GROUP BY (name, version, package_metadata_id)
);

-- Add referential constraints

ALTER TABLE package_derivations
  ADD CONSTRAINT package_derivations_package_id_fkey
  FOREIGN KEY (package_id) REFERENCES packages (id);

ALTER TABLE package_derivations
  ADD CONSTRAINT package_derivations_derivation_id_fkey
  FOREIGN KEY (derivation_id) REFERENCES derivations (id);

-- Delete non-canonical package_metadata entries

ALTER TABLE packages DROP CONSTRAINT package_metadata_id;

DELETE FROM package_metadata AS pm WHERE id NOT IN (
  SELECT MIN(id)
  FROM package_metadata
  GROUP BY (synopsis, description, home_page, location_id, license_set_id)
);

ALTER TABLE packages ADD CONSTRAINT package_metadata_id
  FOREIGN KEY (package_metadata_id) REFERENCES package_metadata(id);

-- Delete non-canonical license_sets entries

ALTER TABLE package_metadata DROP CONSTRAINT package_metadata_license_set_id_fkey;

DELETE FROM license_sets AS ls WHERE id NOT IN (
  SELECT MIN(id)
  FROM license_sets
  GROUP BY license_ids
);

ALTER TABLE package_metadata ADD CONSTRAINT package_metadata_license_set_id_fkey
  FOREIGN KEY (license_set_id) REFERENCES license_sets(id);

-- Delete non-canonical licenses entries

DELETE FROM licenses AS l WHERE id NOT IN (
  SELECT MIN(id)
  FROM licenses
  GROUP BY (name, uri, comment)
);

-- Restore unique constraints

CREATE UNIQUE INDEX ON licenses (name)
  WHERE uri IS NULL AND comment IS NULL;
CREATE UNIQUE INDEX ON licenses (name, uri)
  WHERE uri IS NOT NULL AND comment IS NULL;
CREATE UNIQUE INDEX ON licenses (name, comment)
  WHERE uri IS NULL AND comment IS NOT NULL;
CREATE UNIQUE INDEX ON licenses (name, uri, comment)
  WHERE uri IS NOT NULL AND comment IS NOT NULL;

ALTER TABLE license_sets ADD PRIMARY KEY (license_ids);

ALTER TABLE package_metadata ALTER synopsis SET NOT NULL;
ALTER TABLE package_metadata ALTER description SET NOT NULL;

CREATE UNIQUE INDEX ON package_metadata (
  synopsis,
  description,
  coalesce(location_id, -1),
  coalesce(license_set_id, -1)
) WHERE home_page IS NULL;

CREATE UNIQUE INDEX ON package_metadata (
  synopsis,
  description,
  home_page,
  coalesce(location_id, -1),
  coalesce(license_set_id, -1)
) WHERE home_page IS NOT NULL;

ALTER TABLE packages ADD PRIMARY KEY (name, version, package_metadata_id);

ALTER TABLE package_derivations ADD PRIMARY KEY (package_id, derivation_id, system, target);

COMMIT;
