-- Revert guix-data-service:git_branches from pg

BEGIN;

DROP TABLE git_branches;

COMMIT;
