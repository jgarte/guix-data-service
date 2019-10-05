-- Deploy guix-data-service:remove_guix_revision_duplicates to pg

BEGIN;

WITH data AS (
  SELECT unnest(old_ids) AS old, id FROM (
    SELECT MIN(id) AS id, ARRAY_AGG(id) AS old_ids
    FROM guix_revisions
    GROUP BY commit, git_repository_id
    HAVING COUNT(id) > 1
  ) AS d2
)
DELETE FROM guix_revision_package_derivations
WHERE revision_id IN (SELECT old FROM data WHERE old != id);

WITH data AS (
  SELECT unnest(old_ids) AS old, id FROM (
    SELECT MIN(id) AS id, ARRAY_AGG(id) AS old_ids
    FROM guix_revisions
    GROUP BY commit, git_repository_id
    HAVING COUNT(id) > 1
  ) AS d2
)
UPDATE package_versions_by_guix_revision_range
SET first_guix_revision_id = data.id
FROM data
WHERE first_guix_revision_id = data.old;

DELETE FROM guix_revisions AS g WHERE id NOT IN (
  SELECT MIN(id)
  FROM guix_revisions
  GROUP BY (commit, git_repository_id)
);

CREATE UNIQUE INDEX ON guix_revisions (commit, git_repository_id);

COMMIT;
