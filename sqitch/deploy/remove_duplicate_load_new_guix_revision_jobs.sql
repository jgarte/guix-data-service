-- Deploy guix-data-service:remove_duplicate_load_new_guix_revision_jobs to pg

BEGIN;

DELETE FROM load_new_guix_revision_job_events WHERE job_id NOT IN (
  SELECT MIN(id) FROM load_new_guix_revision_jobs
  GROUP BY commit, git_repository_id
);

DELETE FROM load_new_guix_revision_job_logs WHERE job_id NOT IN (
  SELECT MIN(id) FROM load_new_guix_revision_jobs
  GROUP BY commit, git_repository_id
);

DELETE FROM load_new_guix_revision_jobs WHERE id NOT IN (
  SELECT MIN(id) FROM load_new_guix_revision_jobs
  GROUP BY commit, git_repository_id
);

CREATE UNIQUE INDEX ON load_new_guix_revision_jobs (
  commit,
  git_repository_id
);

COMMIT;
