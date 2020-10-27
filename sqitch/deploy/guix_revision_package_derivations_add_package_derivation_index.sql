-- Deploy guix-data-service:guix_revision_package_derivations_add_package_derivation_index to pg

BEGIN;

CREATE INDEX guix_revision_package_derivations_package_derivation_id_idx
  ON guix_revision_package_derivations (package_derivation_id)
  WITH (fillfactor='100');

COMMIT;
