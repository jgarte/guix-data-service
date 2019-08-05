-- Deploy guix-data-service:change_git_branches_primary_key to pg

BEGIN;

ALTER TABLE git_branches DROP CONSTRAINT name_commit;

ALTER TABLE git_branches ADD PRIMARY KEY (name, commit, git_repository_id);

COMMIT;
