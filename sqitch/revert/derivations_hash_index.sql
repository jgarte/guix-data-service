-- Revert guix-data-service:derivations_hash_index from pg

BEGIN;

DROP INDEX derivations_hash;

COMMIT;
