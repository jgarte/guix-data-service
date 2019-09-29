-- Deploy guix-data-service:fix_null_values_in_git_branches to pg

BEGIN;

ALTER TABLE git_branches DROP CONSTRAINT git_branches_pkey;

UPDATE git_branches SET commit = '' WHERE commit = 'NULL';

ALTER TABLE git_branches ADD PRIMARY KEY (name, commit, git_repository_id, datetime);

COMMIT;
