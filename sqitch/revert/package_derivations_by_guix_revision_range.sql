-- Revert guix-data-service:package_derivations_by_guix_revision_range from pg

BEGIN;

DROP TABLE package_derivations_by_guix_revision_range;

COMMIT;
