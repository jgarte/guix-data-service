-- Deploy guix-data-service:drop_package_versions_by_guix_revision_range to pg

BEGIN;

DROP TABLE package_versions_by_guix_revision_range;

COMMIT;
