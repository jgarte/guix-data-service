-- Verify guix-data-service:dates_to_load_new_guix_revision_jobs on pg

BEGIN;

SELECT created_at FROM load_new_guix_revision_jobs WHERE FALSE;
SELECT succeeded_at FROM load_new_guix_revision_jobs WHERE FALSE;

ROLLBACK;
