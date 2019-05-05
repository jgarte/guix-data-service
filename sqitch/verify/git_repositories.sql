-- Verify guix-data-service:git_repositories on pg

BEGIN;

SELECT id, label, url
  FROM git_repositories WHERE FALSE;

ROLLBACK;
