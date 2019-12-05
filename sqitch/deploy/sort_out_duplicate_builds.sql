-- Deploy guix-data-service:sort_out_duplicate_builds to pg

BEGIN;

DELETE FROM build_status WHERE build_id IN (
  SELECT builds.id
  FROM builds
  INNER JOIN (
    SELECT derivation_file_name, MIN(id) AS id
    FROM builds
    GROUP BY build_server_id, derivation_file_name
    HAVING COUNT(DISTINCT id) > 1
  ) AS min_ids
    ON min_ids.derivation_file_name = builds.derivation_file_name AND
       min_ids.id != builds.id
);

DELETE FROM builds WHERE id IN (
  SELECT builds.id
  FROM builds
  INNER JOIN (
    SELECT derivation_file_name, MIN(id) AS id
    FROM builds
    GROUP BY build_server_id, derivation_file_name
    HAVING COUNT(DISTINCT id) > 1
  ) AS min_ids
    ON min_ids.derivation_file_name = builds.derivation_file_name AND
       min_ids.id != builds.id
);

CREATE UNIQUE INDEX ON builds (build_server_id, derivation_file_name);

COMMIT;
