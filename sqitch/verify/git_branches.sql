-- Verify guix-data-service:git_branches on pg

BEGIN;

SELECT name, commit, git_repository_id, datetime
  FROM git_branches WHERE FALSE;

ROLLBACK;
