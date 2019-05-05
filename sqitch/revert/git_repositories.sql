-- Revert guix-data-service:git_repositories from pg

BEGIN;

ALTER TABLE guix_revisions ADD COLUMN url character varying;

UPDATE guix_revisions SET url = (
  SELECT url FROM git_repositories WHERE guix_revisions.git_repository_id = git_repositories.id
);

ALTER TABLE guix_revisions ALTER COLUMN url SET NOT NULL;

ALTER TABLE guix_revisions DROP COLUMN git_repository_id;

ALTER TABLE load_new_guix_revision_jobs ADD COLUMN url character varying;

UPDATE load_new_guix_revision_jobs SET url = (
  SELECT url FROM git_repositories WHERE load_new_guix_revision_jobs.git_repository_id = git_repositories.id
);

ALTER TABLE load_new_guix_revision_jobs ALTER COLUMN url SET NOT NULL;

ALTER TABLE load_new_guix_revision_jobs DROP COLUMN git_repository_id;

DROP TABLE git_repositories;

COMMIT;
