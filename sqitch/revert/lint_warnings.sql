-- Revert guix-data-service:lint_warnings from pg

BEGIN;

DROP TABLE guix_revision_lint_warnings;
DROP TABLE lint_warnings;
DROP TABLE lint_warning_message_sets;
DROP TABLE lint_warning_messages;
DROP TABLE lint_checkers;

COMMIT;
