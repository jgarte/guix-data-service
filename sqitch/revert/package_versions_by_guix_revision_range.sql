-- Revert guix-data-service:package_versions_by_guix_revision_range from pg

BEGIN;

DROP TABLE package_versions_by_guix_revision_range;

COMMIT;
