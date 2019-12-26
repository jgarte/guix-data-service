-- Deploy guix-data-service:derivations_hash_index to pg

BEGIN;

CREATE INDEX derivations_hash ON derivations (substring(file_name from 12 for 32));

COMMIT;
