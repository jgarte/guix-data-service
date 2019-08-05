-- Revert guix-data-service:change_git_branches_primary_key from pg

BEGIN;

ALTER TABLE git_branches DROP CONSTRAINT git_branches_pkey;

ALTER TABLE git_branches ADD PRIMARY KEY (name, commit);

COMMIT;
