-- Deploy guix-data-service:add_retry_value_to_job_event_enum to pg

ALTER TYPE job_event ADD VALUE 'retry';

CREATE INDEX git_branches_name_and_datetime ON git_branches (name, datetime DESC);
