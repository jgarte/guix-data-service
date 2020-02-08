-- Deploy guix-data-service:allow_including_and_excluding_branches_for_repositories to pg

BEGIN;

ALTER TABLE git_repositories ADD COLUMN included_branches varchar[];
ALTER TABLE git_repositories ADD COLUMN excluded_branches varchar[];

COMMIT;
