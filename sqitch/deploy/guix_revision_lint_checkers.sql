-- Deploy guix-data-service:guix_revision_lint_checkers to pg

BEGIN;

CREATE TABLE guix_revision_lint_checkers (
  lint_checker_id integer NOT NULL REFERENCES lint_checkers (id),
  guix_revision_id integer NOT NULL REFERENCES guix_revisions (id),
  PRIMARY KEY (lint_checker_id, guix_revision_id)
);

COMMIT;
