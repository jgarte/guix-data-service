-- Deploy guix-data-service:lint_warnings to pg

BEGIN;

CREATE TABLE lint_checkers (
  id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  name varchar NOT NULL,
  description varchar NOT NULL,
  network_dependent boolean NOT NULL,
  UNIQUE (name, description, network_dependent)
);

CREATE TABLE lint_warning_messages (
  id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  locale varchar NOT NULL,
  message varchar NOT NULL,
  UNIQUE (locale, message)
);

CREATE TABLE lint_warning_message_sets (
  id integer NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  message_ids integer[] NOT NULL,
  UNIQUE (message_ids)
);

CREATE TABLE lint_warnings (
  id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  lint_checker_id integer NOT NULL,
  package_id integer NOT NULL REFERENCES packages (id),
  location_id integer NOT NULL REFERENCES locations (id),
  lint_warning_message_set_id integer NOT NULL REFERENCES lint_warning_message_sets (id),
  UNIQUE (lint_checker_id, package_id, location_id, lint_warning_message_set_id)
);

CREATE TABLE guix_revision_lint_warnings (
  lint_warning_id integer NOT NULL REFERENCES lint_warnings (id),
  guix_revision_id integer NOT NULL REFERENCES guix_revisions (id),
  PRIMARY KEY (lint_warning_id, guix_revision_id)
);

COMMIT;
