-- Deploy guix-data-service:package_derivations_by_guix_revision_range to pg

BEGIN;

CREATE TABLE package_derivations_by_guix_revision_range (
  git_repository_id integer NOT NULL REFERENCES git_repositories (id),
  branch_name varchar NOT NULL,
  package_name varchar NOT NULL,
  package_version varchar NOT NULL,
  derivation_id integer NOT NULL,
  system varchar NOT NULL,
  target varchar NOT NULL,
  first_guix_revision_id integer NOT NULL REFERENCES guix_revisions (id),
  last_guix_revision_id integer NOT NULL REFERENCES guix_revisions (id)
);

COMMIT;
