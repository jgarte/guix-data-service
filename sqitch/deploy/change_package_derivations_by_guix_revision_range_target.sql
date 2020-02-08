-- Deploy guix-data-service:change_package_derivations_by_guix_revision_range_target to pg

BEGIN;

UPDATE package_derivations_by_guix_revision_range SET target = '' WHERE system = target;

DELETE FROM package_derivations_by_guix_revision_range WHERE target IN (
  'aarch64-linux',
  'armhf-linux',
  'i686-linux',
  'mips64el-linux',
  'x86_64-linux'
);

COMMIT;
