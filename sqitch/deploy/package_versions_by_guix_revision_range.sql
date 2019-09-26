-- Deploy guix-data-service:package_versions_by_guix_revision_range to pg

BEGIN;

CREATE TABLE package_versions_by_guix_revision_range (
  git_repository_id integer NOT NULL REFERENCES git_repositories (id),
  branch_name varchar NOT NULL,
  package_name varchar NOT NULL,
  package_version varchar NOT NULL,
  first_guix_revision_id integer NOT NULL REFERENCES guix_revisions (id),
  last_guix_revision_id integer NOT NULL REFERENCES guix_revisions (id)
);

COMMIT;
