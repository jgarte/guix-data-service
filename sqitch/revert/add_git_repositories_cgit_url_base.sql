-- Revert guix-data-service:add_git_repositories_cgit_url_base from pg

BEGIN;

ALTER TABLE git_repositories DROP COLUMN cgit_url_base;

COMMIT;
