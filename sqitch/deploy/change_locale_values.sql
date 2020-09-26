-- Deploy guix-data-service:change_locale_values to pg

BEGIN;

UPDATE lint_checker_descriptions SET locale = REPLACE(locale, '.utf8', '.UTF-8');
UPDATE package_metadata_tsvectors SET locale = REPLACE(locale, '.utf8', '.UTF-8');
UPDATE package_descriptions SET locale = REPLACE(locale, '.utf8', '.UTF-8');
UPDATE package_synopsis SET locale = REPLACE(locale, '.utf8', '.UTF-8');
UPDATE lint_warning_messages SET locale = REPLACE(locale, '.utf8', '.UTF-8');

COMMIT;
