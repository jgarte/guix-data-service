-- Deploy guix-data-service:make_nar_urls_file_size_optional to pg

BEGIN;

ALTER TABLE nar_urls ALTER COLUMN file_size DROP NOT NULL;

COMMIT;
