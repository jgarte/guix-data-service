;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (guix-data-service model package)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (squee)
  #:use-module (guix inferior)
  #:use-module (guix-data-service model utils)
  #:export (select-existing-package-entries
            select-packages-in-revision
            search-packages-in-revision
            count-packages-in-revision
            inferior-packages->package-ids

            select-package-versions-for-revision
            package-versions-for-branch
            package-derivations-for-branch
            package-outputs-for-branch

            any-package-synopsis-or-descriptions-translations?

            branches-by-package-version))

(define (select-existing-package-entries package-entries)
  (string-append
   "
SELECT id, packages.name, packages.version,
       packages.package_metadata_id
FROM packages
JOIN (VALUES "
   (string-join (map (lambda (package-entry)
                       (apply
                        simple-format
                        #f "('~A', '~A', ~A)"
                        package-entry))
                     package-entries)
                ", ")
   "
) AS vals (name, version, package_metadata_id)
  ON packages.name = vals.name
 AND packages.version = vals.version
 AND packages.package_metadata_id = vals.package_metadata_id"))

(define* (select-packages-in-revision conn commit-hash
                                      #:key limit-results
                                      after-name
                                      locale)
  (define query
    (string-append "
WITH data AS (
  SELECT packages.name, packages.version, translated_package_synopsis.synopsis,
    translated_package_synopsis.locale, translated_package_descriptions.description,
    translated_package_descriptions.locale, package_metadata.home_page,
    locations.file, locations.line, locations.column_number,
    (SELECT JSON_AGG((license_data.*))
     FROM (
       SELECT licenses.name, licenses.uri, licenses.comment
       FROM licenses
       INNER JOIN license_sets ON licenses.id = ANY(license_sets.license_ids)
       WHERE license_sets.id = package_metadata.license_set_id
       ORDER BY licenses.name
     ) AS license_data
    ) AS licenses
  FROM packages
  INNER JOIN package_metadata
    ON packages.package_metadata_id = package_metadata.id
  LEFT OUTER JOIN locations
    ON package_metadata.location_id = locations.id
  INNER JOIN (
    SELECT DISTINCT ON (package_synopsis_sets.id) package_synopsis_sets.id,
             package_synopsis.synopsis, package_synopsis.locale
    FROM package_synopsis_sets
    INNER JOIN package_synopsis
      ON package_synopsis.id = ANY (package_synopsis_sets.synopsis_ids)
    ORDER BY package_synopsis_sets.id,
             CASE WHEN package_synopsis.locale = $2 THEN 2
                  WHEN package_synopsis.locale = 'en_US.UTF-8' THEN 1
                  ELSE 0
             END DESC
  ) AS translated_package_synopsis
    ON package_metadata.package_synopsis_set_id = translated_package_synopsis.id
  INNER JOIN (
    SELECT DISTINCT ON (package_description_sets.id) package_description_sets.id,
            package_descriptions.description, package_descriptions.locale
    FROM package_description_sets
    INNER JOIN package_descriptions
      ON package_descriptions.id = ANY (package_description_sets.description_ids)
    ORDER BY package_description_sets.id,
             CASE WHEN package_descriptions.locale = $2 THEN 2
                  WHEN package_descriptions.locale = 'en_US.UTF-8' THEN 1
                  ELSE 0
             END DESC
  ) AS translated_package_descriptions
    ON package_metadata.package_description_set_id = translated_package_descriptions.id
  WHERE packages.id IN (
   SELECT package_derivations.package_id
   FROM package_derivations
   INNER JOIN guix_revision_package_derivations
     ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
   INNER JOIN guix_revisions
     ON guix_revision_package_derivations.revision_id = guix_revisions.id
   WHERE guix_revisions.commit = $1
  )
  ORDER BY packages.name, packages.version
), package_names AS (
  SELECT DISTINCT name
  FROM data"
    (if after-name
        "\nWHERE name > $3\n"
        "")
    "  ORDER BY name"
    (if limit-results
        (string-append " LIMIT " (number->string limit-results))
        "")
")
SELECT data.*
FROM data
WHERE data.name IN (SELECT name FROM package_names);"))

  (exec-query conn query
              `(,commit-hash
                ,locale
                ,@(if after-name
                      (list after-name)
                      '()))))

(define* (search-packages-in-revision conn commit-hash
                                      search-query
                                      #:key
                                      limit-results
                                      locale)
  (define query
    (string-append
"
WITH revision_packages AS (
  SELECT *
  FROM packages
  WHERE packages.id IN (
    SELECT package_derivations.package_id
    FROM package_derivations
    INNER JOIN guix_revision_package_derivations
      ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
    INNER JOIN guix_revisions
      ON guix_revision_package_derivations.revision_id = guix_revisions.id
    WHERE guix_revisions.commit = $1
  )
), search_results AS (
  SELECT DISTINCT ON
           (packages.name, packages.version, packages.replacement_package_id)
         packages.name,
         packages.version, package_synopsis.synopsis,
         package_synopsis.locale AS synopsis_locale,
         package_descriptions.description,
         package_descriptions.locale AS description_locale,
         package_metadata.home_page,
         package_metadata_tsvectors.synopsis_and_description,
         locations.file, locations.line, locations.column_number,
         (SELECT JSON_AGG((license_data.*))
          FROM (
            SELECT licenses.name, licenses.uri, licenses.comment
            FROM licenses
            INNER JOIN license_sets ON licenses.id = ANY(license_sets.license_ids)
            WHERE license_sets.id = package_metadata.license_set_id
            ORDER BY licenses.name
           ) AS license_data
          ) AS licenses
  FROM revision_packages AS packages
  INNER JOIN package_metadata
    ON packages.package_metadata_id = package_metadata.id
  LEFT OUTER JOIN locations
    ON package_metadata.location_id = locations.id
  INNER JOIN package_metadata_tsvectors
    ON package_metadata_tsvectors.package_metadata_id = package_metadata.id
  INNER JOIN package_synopsis
    ON package_metadata_tsvectors.package_synopsis_id = package_synopsis.id
  INNER JOIN package_descriptions
    ON package_metadata_tsvectors.package_description_id = package_descriptions.id
  WHERE (
    to_tsvector(packages.name) @@ (plainto_tsquery($2) || plainto_tsquery(REPLACE($2, '-', ' ')))
    OR
    package_metadata_tsvectors.synopsis_and_description @@ plainto_tsquery($2)
  )
  ORDER BY name, packages.version, packages.replacement_package_id,
    CASE WHEN package_metadata_tsvectors.locale = 'en_US.UTF-8' THEN 2
         WHEN package_metadata_tsvectors.locale = $3 THEN 1
         ELSE 0
    END DESC
)
SELECT name, version, synopsis, synopsis_locale,
       description, description_locale,
       home_page, file, line, column_number, licenses
FROM search_results
ORDER BY (
           ts_rank_cd(
              setweight(to_tsvector(name), 'A'),
              plainto_tsquery($2),
              2 -- divide rank by the document length
           ) * 1.5 +
           ts_rank_cd(
              synopsis_and_description,
              plainto_tsquery($2),
              32 -- divide the rank by itself + 1
           )
         ) DESC,
         name,
         version
"
     (if limit-results
         (string-append "\nLIMIT " (number->string limit-results))
         "")))

  (exec-query conn query
              (list commit-hash search-query locale)))

(define (count-packages-in-revision conn commit-hash)
  (define query
    "
SELECT COUNT(DISTINCT packages.name)
FROM packages
WHERE packages.id IN (
 SELECT package_derivations.package_id
 FROM package_derivations
 INNER JOIN guix_revision_package_derivations
   ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
 INNER JOIN guix_revisions
   ON guix_revision_package_derivations.revision_id = guix_revisions.id
 WHERE guix_revisions.commit = $1
)")

  (exec-query conn query (list commit-hash)))

(define (insert-into-package-entries package-entries)
  (string-append
   "
INSERT INTO packages (name, version, package_metadata_id) VALUES "
   (string-join
    (map
     (match-lambda
       ((name version package_metadata_id)
        (simple-format #f "('~A', '~A', ~A)"
                       name
                       version
                       package_metadata_id)))
     package-entries)
    ",")
   "
RETURNING id"))

(define (inferior-packages->package-ids conn package-entries)
  (insert-missing-data-and-return-all-ids
   conn
   "packages"
   '(name version package_metadata_id replacement_package_id)
   package-entries))

(define (select-package-versions-for-revision conn
                                              commit
                                              package-name)
  (define query "
SELECT DISTINCT version FROM packages
INNER JOIN package_derivations
  ON packages.id = package_derivations.package_id
INNER JOIN guix_revision_package_derivations
  ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
INNER JOIN guix_revisions
  ON guix_revision_package_derivations.revision_id = guix_revisions.id
WHERE guix_revisions.commit = $1 AND packages.name = $2
ORDER BY version")

  (map
   car
   (exec-query conn query (list commit package-name))))

(define (package-versions-for-branch conn
                                     git-repository-id
                                     branch-name
                                     package-name)
  (exec-query
   conn
   "
SELECT DISTINCT
    data1.package_version,
    first_value(first_guix_revision_commit) OVER version_window AS first_guix_revision_commit,
    first_value(first_datetime) OVER version_window AS first_datetime,
    last_value(last_guix_revision_commit) OVER version_window AS last_guix_revision_commit,
    last_value(last_datetime) OVER version_window AS last_datetime
FROM (
  SELECT DISTINCT -- Because of systems and targets, maybe they should
                  -- be parameters?
         package_version,
         first_guix_revisions.commit AS first_guix_revision_commit,
         first_git_branches.datetime AS first_datetime,
         last_guix_revisions.commit AS last_guix_revision_commit,
         last_git_branches.datetime AS last_datetime
  FROM package_derivations_by_guix_revision_range
  INNER JOIN guix_revisions AS first_guix_revisions
    ON first_guix_revision_id = first_guix_revisions.id
  INNER JOIN git_branches AS first_git_branches
    ON package_derivations_by_guix_revision_range.branch_name = first_git_branches.name
   AND first_guix_revisions.git_repository_id = first_git_branches.git_repository_id
   AND first_guix_revisions.commit = first_git_branches.commit
  INNER JOIN guix_revisions AS last_guix_revisions
    ON last_guix_revision_id = last_guix_revisions.id
  INNER JOIN git_branches AS last_git_branches
    ON package_derivations_by_guix_revision_range.branch_name = last_git_branches.name
   AND last_guix_revisions.git_repository_id = last_git_branches.git_repository_id
   AND last_guix_revisions.commit = last_git_branches.commit
  WHERE package_name = $1
  AND package_derivations_by_guix_revision_range.git_repository_id = $2
  AND package_derivations_by_guix_revision_range.branch_name = $3
  ORDER BY package_version DESC, first_git_branches.datetime ASC
) AS data1
WINDOW version_window AS (
  PARTITION BY data1.package_version
  ORDER BY data1.package_version DESC, data1.first_datetime ASC
  RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
)
ORDER BY package_version DESC, first_datetime ASC"
   (list package-name
         (number->string git-repository-id)
         branch-name)))

(define (package-derivations-for-branch conn
                                        git-repository-id
                                        branch-name
                                        system
                                        target
                                        package-name)
  (define query
    "
SELECT package_version,
       derivations.file_name,
       first_guix_revisions.commit AS first_guix_revision_commit,
       first_git_branches.datetime AS first_datetime,
       last_guix_revisions.commit AS last_guix_revision_commit,
       last_git_branches.datetime AS last_datetime,
       JSON_AGG(
         json_build_object(
           'build_server_id', builds.build_server_id,
           'build_server_build_id', builds.build_server_build_id,
           'status',  latest_build_status.status,
           'timestamp',  latest_build_status.timestamp,
           'build_for_equivalent_derivation',
           builds.derivation_file_name != derivations.file_name
         )
         ORDER BY latest_build_status.timestamp
       ) AS builds
FROM package_derivations_by_guix_revision_range
INNER JOIN derivations
  ON package_derivations_by_guix_revision_range.derivation_id = derivations.id
INNER JOIN derivations_by_output_details_set
  ON derivations_by_output_details_set.derivation_id = derivations.id
LEFT OUTER JOIN builds
  ON derivations_by_output_details_set.derivation_output_details_set_id =
     builds.derivation_output_details_set_id
LEFT OUTER JOIN latest_build_status
  ON builds.id = latest_build_status.build_id
INNER JOIN guix_revisions AS first_guix_revisions
  ON first_guix_revision_id = first_guix_revisions.id
INNER JOIN git_branches AS first_git_branches
  ON first_guix_revisions.git_repository_id = first_git_branches.git_repository_id
 AND first_guix_revisions.commit = first_git_branches.commit
INNER JOIN guix_revisions AS last_guix_revisions
  ON last_guix_revision_id = last_guix_revisions.id
INNER JOIN git_branches AS last_git_branches
  ON last_guix_revisions.git_repository_id = last_git_branches.git_repository_id
 AND last_guix_revisions.commit = last_git_branches.commit
WHERE package_name = $1
AND package_derivations_by_guix_revision_range.git_repository_id = $2
AND package_derivations_by_guix_revision_range.branch_name = $3
AND first_git_branches.name = $3
AND last_git_branches.name = $3
AND package_derivations_by_guix_revision_range.system = $4
AND package_derivations_by_guix_revision_range.target = $5
GROUP BY 1, 2, 3, 4, 5, 6
ORDER BY first_datetime DESC, package_version DESC")

  (map (match-lambda
         ((version derivation-file-name
                   first-guix-revision-commit
                   first-datetime
                   last-guix-revision-commit
                   last-datetime
                   builds-json)
          (list version
                derivation-file-name
                first-guix-revision-commit
                first-datetime
                last-guix-revision-commit
                last-datetime
                (if (string-null? builds-json)
                    '()
                    (filter (lambda (build)
                              (number? (assoc-ref build "build_server_id")))
                            (vector->list
                             (json-string->scm builds-json)))))))
       (exec-query
        conn
        query
        (list package-name
              (number->string git-repository-id)
              branch-name
              system
              target))))

(define (package-outputs-for-branch conn
                                    git-repository-id
                                    branch-name
                                    system
                                    target
                                    package-name
                                    output-name)
  (define query
    "
SELECT package_version,
       path,
       first_guix_revision_commit,
       first_datetime,
       last_guix_revision_commit,
       last_datetime,
       JSON_AGG(
         json_build_object(
           'build_server_id', builds.build_server_id,
           'build_server_build_id', builds.build_server_build_id,
           'derivation_file_name', builds.derivation_file_name,
           'status',  latest_build_status.status,
           'timestamp',  latest_build_status.timestamp
         )
         ORDER BY latest_build_status.timestamp
       ) AS builds
FROM (
  SELECT DISTINCT
      first_value(package_version) OVER path_window AS package_version,
      path,
      derivation_output_details_set_id,
      first_value(first_guix_revision_commit) OVER path_window AS first_guix_revision_commit,
      first_value(first_datetime) OVER path_window AS first_datetime,
      last_value(last_guix_revision_commit) OVER path_window AS last_guix_revision_commit,
      last_value(last_datetime) OVER path_window AS last_datetime
  FROM (
    SELECT package_version,
           derivation_output_details.path,
           derivations_by_output_details_set.derivation_output_details_set_id,
           first_guix_revisions.commit AS first_guix_revision_commit,
           first_git_branches.datetime AS first_datetime,
           last_guix_revisions.commit AS last_guix_revision_commit,
           last_git_branches.datetime AS last_datetime
    FROM package_derivations_by_guix_revision_range
    INNER JOIN derivations
      ON package_derivations_by_guix_revision_range.derivation_id = derivations.id
    INNER JOIN derivation_outputs
      ON derivation_outputs.derivation_id = derivations.id
    INNER JOIN derivation_output_details
      ON derivation_outputs.derivation_output_details_id = derivation_output_details.id
    INNER JOIN guix_revisions AS first_guix_revisions
      ON first_guix_revision_id = first_guix_revisions.id
    INNER JOIN derivations_by_output_details_set
      ON derivations_by_output_details_set.derivation_id = derivations.id
    INNER JOIN git_branches AS first_git_branches
      ON first_guix_revisions.git_repository_id = first_git_branches.git_repository_id
     AND first_guix_revisions.commit = first_git_branches.commit
    INNER JOIN guix_revisions AS last_guix_revisions
      ON last_guix_revision_id = last_guix_revisions.id
    INNER JOIN git_branches AS last_git_branches
      ON last_guix_revisions.git_repository_id = last_git_branches.git_repository_id
     AND last_guix_revisions.commit = last_git_branches.commit
    WHERE package_name = $1
    AND package_derivations_by_guix_revision_range.git_repository_id = $2
    AND package_derivations_by_guix_revision_range.branch_name = $3
    AND derivation_outputs.name = $4
    AND first_git_branches.name = $3
    AND last_git_branches.name = $3
    AND package_derivations_by_guix_revision_range.system = $5
    AND package_derivations_by_guix_revision_range.target = $6
  ) AS data1
  WINDOW path_window AS (
    PARTITION BY path
    ORDER BY first_datetime ASC, package_version DESC
    RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
  )
) AS data2
LEFT OUTER JOIN builds
  ON data2.derivation_output_details_set_id =
     builds.derivation_output_details_set_id
LEFT OUTER JOIN latest_build_status
  ON builds.id = latest_build_status.build_id
GROUP BY 1, 2, 3, 4, 5, 6
ORDER BY first_datetime DESC, package_version DESC")

  (map (match-lambda
         ((version path
                   first-guix-revision-commit
                   first-datetime
                   last-guix-revision-commit
                   last-datetime
                   builds-json)
          (list version
                path
                first-guix-revision-commit
                first-datetime
                last-guix-revision-commit
                last-datetime
                (if (string-null? builds-json)
                    '()
                    (filter (lambda (build)
                              (number? (assoc-ref build "build_server_id")))
                            (vector->list
                             (json-string->scm builds-json)))))))
       (exec-query
        conn
        query
        (list package-name
              (number->string git-repository-id)
              branch-name
              output-name
              system
              target))))

(define (any-package-synopsis-or-descriptions-translations? packages locale)
  (any
   (match-lambda
     ((name version synopsis synopsis-locale description description-locale _ _ _ _ _)
      (or (string=? synopsis-locale locale)
          (string=? description-locale locale))))
   packages))

(define (branches-by-package-version conn package-name system target)
  (define query
    "
WITH branches AS (
  SELECT DISTINCT ON (git_repository_id, name) git_repository_id, name
  FROM git_branches
  WHERE commit <> ''
  ORDER BY git_repository_id, name, datetime DESC
)
SELECT packages.version,
       JSON_AGG(
         json_build_object(
           'git_repository_id', branches.git_repository_id,
           'name', branches.name
         )
       )
FROM branches
CROSS JOIN LATERAL (
  SELECT guix_revisions.id
  FROM git_branches
  INNER JOIN guix_revisions
    ON git_branches.commit = guix_revisions.commit
  INNER JOIN load_new_guix_revision_jobs
    ON load_new_guix_revision_jobs.commit = guix_revisions.commit
  INNER JOIN load_new_guix_revision_job_events
    ON job_id = load_new_guix_revision_jobs.id
  WHERE guix_revisions.git_repository_id = branches.git_repository_id
    AND git_branches.git_repository_id = branches.git_repository_id
    AND git_branches.name = branches.name
    AND load_new_guix_revision_job_events.event = 'success'
  ORDER BY datetime DESC
  LIMIT 1
) AS latest_processed_guix_revision
INNER JOIN guix_revision_package_derivations
  ON guix_revision_package_derivations.revision_id =
     latest_processed_guix_revision.id
INNER JOIN package_derivations
  ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
 AND package_derivations.target = $3
INNER JOIN systems
  ON package_derivations.system_id = systems.id
 AND systems.system = $2
INNER JOIN packages
  ON package_derivations.package_id = packages.id
WHERE packages.name = $1
GROUP BY packages.version
ORDER BY packages.version DESC")

  (list->vector
   (map (match-lambda
          ((version
            branches-json)
           `((version . ,version)
             (branches . ,(json-string->scm branches-json)))))
        (exec-query
         conn
         query
         (list package-name system target)))))
