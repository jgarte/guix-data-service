-- Deploy guix-data-service:create_latest_build_status to pg

BEGIN;

CREATE TABLE latest_build_status (
    build_id integer PRIMARY KEY NOT NULL REFERENCES builds(id),
    "timestamp" timestamp without time zone DEFAULT clock_timestamp(),
    status guix_data_service.buildstatus NOT NULL
);

INSERT INTO latest_build_status
SELECT DISTINCT build_id,
                first_value(timestamp) OVER rows_for_build AS timestamp,
                first_value(status) OVER rows_for_build AS status
FROM build_status
WINDOW rows_for_build AS (
  PARTITION BY build_id
  ORDER BY
    timestamp DESC,
    CASE WHEN status = 'scheduled' THEN -2
         WHEN status = 'started' THEN -1
         ELSE 0
    END DESC
    RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
);

COMMIT;
