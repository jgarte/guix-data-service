-- Deploy guix-data-service:remove_odd_package_derivations to pg

BEGIN;

DELETE FROM guix_revision_package_derivations
WHERE package_derivation_id IN (
  SELECT package_derivations.id
  FROM package_derivations
  INNER JOIN derivations
    ON package_derivations.derivation_id = derivations.id
  WHERE package_derivations.system != derivations.system
);

DELETE FROM package_derivations
WHERE id IN (
  SELECT package_derivations.id
  FROM package_derivations
  INNER JOIN derivations
    ON package_derivations.derivation_id = derivations.id
  WHERE package_derivations.system != derivations.system
);

COMMIT;
