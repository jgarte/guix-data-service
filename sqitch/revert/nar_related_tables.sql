-- Revert guix-data-service:nar_related_tables from pg

BEGIN;

DROP TABLE narinfo_signatures;
DROP TABLE narinfo_signature_data;
DROP TABLE narinfo_signature_public_keys;
DROP TABLE nar_references;
DROP TABLE nar_urls;
DROP TABLE nars;

COMMIT;
