-- Deploy guix-data-service:load_new_guix_revision_jobs_make_commits_unique to pg

BEGIN;

DROP INDEX load_new_guix_revision_jobs_commit_git_repository_id_idx;

DELETE FROM load_new_guix_revision_job_events
WHERE job_id NOT IN (
  SELECT DISTINCT ON (commit) id
  FROM load_new_guix_revision_jobs
  ORDER BY commit, succeeded_at ASC NULLS LAST, created_at ASC
)

DELETE FROM load_new_guix_revision_job_logs
WHERE job_id NOT IN (
  SELECT DISTINCT ON (commit) id
  FROM load_new_guix_revision_jobs
  ORDER BY commit, succeeded_at ASC NULLS LAST, created_at ASC
);

DELETE FROM load_new_guix_revision_jobs
WHERE id NOT IN (
  SELECT DISTINCT ON (commit) id
  FROM load_new_guix_revision_jobs
  ORDER BY commit, succeeded_at ASC NULLS LAST, created_at ASC
);

CREATE UNIQUE INDEX load_new_guix_revision_jobs_commit_idx
  ON load_new_guix_revision_jobs (commit);

COMMIT;
