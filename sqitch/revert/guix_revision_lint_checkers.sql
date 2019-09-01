-- Revert guix-data-service:guix_revision_lint_checkers from pg

BEGIN;

DROP TABLE guix_revision_lint_checkers;

COMMIT;
