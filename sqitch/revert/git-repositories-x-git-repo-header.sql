-- Revert guix-data-service:git-repositories-x-git-repo-header from pg

BEGIN;

ALTER TABLE git_repositories DROP COLUMN x_git_repo_header;

COMMIT;
