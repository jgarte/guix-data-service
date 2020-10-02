-- Deploy guix-data-service:change_derivation_source_file_nars_constraint to pg

BEGIN;

ALTER TABLE derivation_source_file_nars
  DROP CONSTRAINT derivation_source_file_nars_derivation_source_file_id_fkey;

ALTER TABLE derivation_source_file_nars
  ADD CONSTRAINT derivation_source_file_nars_derivation_source_file_id_fkey
  FOREIGN KEY (derivation_source_file_id) REFERENCES derivation_source_files(id)
  ON DELETE CASCADE;

COMMIT;
