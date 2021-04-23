-- Deploy guix-data-service:systems_table to pg

BEGIN;

CREATE TABLE systems (
  id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  system character varying UNIQUE NOT NULL
);

INSERT INTO systems (system) SELECT DISTINCT system FROM derivations;

ALTER TABLE derivations
  ADD COLUMN system_id integer REFERENCES systems (id);

UPDATE derivations
  SET system_id = (
    SELECT id FROM systems WHERE systems.system = derivations.system
  );

ALTER TABLE derivations
  ALTER COLUMN system_id SET NOT NULL;

ALTER TABLE derivations DROP COLUMN system;

ALTER TABLE package_derivations
  ADD COLUMN system_id integer REFERENCES systems (id);

UPDATE package_derivations
  SET system_id = (
    SELECT id FROM systems WHERE systems.system = package_derivations.system
  );

ALTER TABLE package_derivations
  ALTER COLUMN system_id SET NOT NULL;

ALTER TABLE package_derivations DROP COLUMN system;

COMMIT;
