-- Deploy guix-data-service:regenerate_latest_build_status to pg

BEGIN;

DELETE FROM latest_build_status;

INSERT INTO latest_build_status
SELECT DISTINCT build_id,
                first_value(timestamp) OVER rows_for_build AS timestamp,
                first_value(status) OVER rows_for_build AS status
FROM build_status
WINDOW rows_for_build AS (
  PARTITION BY build_id
  ORDER BY
    CASE WHEN status = 'scheduled' THEN -2
         WHEN status = 'started' THEN -1
         ELSE 0
    END DESC,
    timestamp DESC
    RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
);

COMMIT;
