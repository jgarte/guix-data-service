-- Deploy guix-data-service:guix_revision_system_test_derivations_add_system to pg

BEGIN;

ALTER TABLE guix_revision_system_test_derivations ADD COLUMN system varchar;

-- Assume that existing values are for 'x86_64-linux'
UPDATE guix_revision_system_test_derivations SET system = 'x86_64-linux';

ALTER TABLE guix_revision_system_test_derivations ALTER system SET NOT NULL;

ALTER TABLE guix_revision_system_test_derivations
  DROP CONSTRAINT guix_revision_system_test_derivations_pkey;

ALTER TABLE guix_revision_system_test_derivations
  ADD CONSTRAINT guix_revision_system_test_derivations_pkey PRIMARY KEY (guix_revision_id, system_test_id, system, derivation_id);

COMMIT;
