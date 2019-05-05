-- Deploy guix-data-service:git_branches to pg

BEGIN;

CREATE TABLE git_branches (
    name character varying NOT NULL,
    commit character varying,
    git_repository_id integer NOT NULL,
    datetime timestamp without time zone NOT NULL,
    CONSTRAINT name_commit PRIMARY KEY(name, commit)
);

COMMIT;
