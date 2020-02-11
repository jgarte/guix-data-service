-- Deploy guix-data-service:channel_instance_derivations to pg

BEGIN;

CREATE TABLE channel_instances (
  guix_revision_id integer NOT NULL REFERENCES guix_revisions(id),
  system varchar NOT NULL,
  derivation_id integer NOT NULL REFERENCES derivations (id),
  PRIMARY KEY (guix_revision_id, system),
  UNIQUE (derivation_id)
);

COMMIT;
