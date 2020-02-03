-- Deploy guix-data-service:system_test_tables to pg

BEGIN;

CREATE TABLE system_tests (
  id integer NOT NULL GENERATED ALWAYS AS IDENTITY,
  name varchar NOT NULL,
  description varchar NOT NULL,
  location_id integer NOT NULL REFERENCES locations (id),
  PRIMARY KEY (name, description, location_id),
  UNIQUE (id)
);

CREATE TABLE guix_revision_system_test_derivations (
  guix_revision_id integer NOT NULL REFERENCES guix_revisions (id),
  system_test_id integer NOT NULL REFERENCES system_tests (id),
  derivation_id integer NOT NULL REFERENCES derivations (id),
  PRIMARY KEY (guix_revision_id, system_test_id, derivation_id)
);

COMMIT;
