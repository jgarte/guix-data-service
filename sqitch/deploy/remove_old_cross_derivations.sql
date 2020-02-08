-- Deploy guix-data-service:remove_old_cross_derivations to pg

BEGIN;

DELETE FROM guix_revision_package_derivations WHERE package_derivation_id IN (
  SELECT id
  FROM package_derivations
  WHERE target IN (
    'aarch64-linux',
    'armhf-linux',
    'i686-linux',
    'mips64el-linux',
    'x86_64-linux'
  )
);

-- Checking this constraint is expensive, so drop it, then re-create it
ALTER TABLE guix_revision_package_derivations DROP CONSTRAINT guix_revision_package_derivations_package_derivation_id_fkey;

DELETE FROM package_derivations WHERE target IN (
  'aarch64-linux',
  'armhf-linux',
  'i686-linux',
  'mips64el-linux',
  'x86_64-linux'
);

ALTER TABLE guix_revision_package_derivations ADD CONSTRAINT "guix_revision_package_derivations_package_derivation_id_fkey" FOREIGN KEY (package_derivation_id) REFERENCES package_derivations(id);

COMMIT;
