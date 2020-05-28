-- Deploy guix-data-service:translations_for_lint_checker_descriptions to pg

BEGIN;

CREATE TABLE lint_checker_descriptions (
  id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  locale varchar NOT NULL,
  description varchar NOT NULL,
  UNIQUE (locale, description)
);

CREATE TABLE lint_checker_description_sets (
  id integer NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  description_ids integer[] NOT NULL,
  UNIQUE (description_ids)
);

ALTER TABLE lint_checkers ADD COLUMN lint_checker_description_set_id integer REFERENCES lint_checker_description_sets(id);

INSERT INTO lint_checker_descriptions(locale, description)
SELECT DISTINCT 'en_US.utf8', description
  FROM lint_checkers;

INSERT INTO lint_checker_description_sets (description_ids)
SELECT DISTINCT ARRAY_AGG(
  id)
  FROM lint_checker_descriptions
 GROUP BY id;

UPDATE lint_checkers
   SET lint_checker_description_set_id =
       lint_checker_description_sets.id
       FROM lint_checker_description_sets
       INNER JOIN lint_checker_descriptions
       ON lint_checker_description_sets.description_ids[1] = lint_checker_descriptions.id
 WHERE lint_checkers.description = lint_checker_descriptions.description;

ALTER TABLE lint_checkers DROP COLUMN description;

COMMIT;
