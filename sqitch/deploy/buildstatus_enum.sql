-- Deploy guix-data-service:buildstatus_enum to pg
-- requires: appschema

BEGIN;

SET client_min_messages = 'warning';

CREATE TYPE guix_data_service.buildstatus AS ENUM (
    'scheduled',
    'started',
    'succeeded',
    'failed',
    'failed-dependency',
    'failed-other',
    'canceled'
);

COMMIT;
