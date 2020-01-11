-- Deploy guix-data-service:git-repositories-x-git-repo-header to pg

BEGIN;

ALTER TABLE git_repositories ADD COLUMN x_git_repo_header varchar;

COMMIT;
