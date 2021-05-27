-- Deploy guix-data-service:package_metadata_location_id_index to pg

BEGIN;

CREATE INDEX package_metadata_location_id ON package_metadata (location_id);

COMMIT;
