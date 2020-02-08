-- Deploy guix-data-service:change_package_derivations_target to pg

BEGIN;

UPDATE package_derivations SET target = '' WHERE system = target;

COMMIT;
