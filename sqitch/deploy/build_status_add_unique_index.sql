-- Deploy guix-data-service:build_status_add_unique_index to pg

BEGIN;

DELETE FROM build_status
WHERE id NOT IN (
  SELECT MIN(id) FROM build_status GROUP BY build_id, timestamp, status
);

ALTER TABLE build_status
ADD CONSTRAINT build_status_build_id_timestamp_status_unique
UNIQUE (build_id, timestamp, status);

CREATE UNIQUE INDEX build_status_build_id_status_unique_idx
  ON build_status (build_id, status)
  WHERE timestamp IS NULL;

COMMIT;
