-- Deploy guix-data-service:add_git_repositories_cgit_url_base to pg

BEGIN;

ALTER TABLE git_repositories ADD COLUMN cgit_url_base character varying;

COMMIT;
