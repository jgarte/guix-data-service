-- Revert guix-data-service:channel_instance_derivations from pg

BEGIN;

DROP TABLE channel_instances;

COMMIT;
