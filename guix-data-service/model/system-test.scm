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

(define-module (guix-data-service model system-test)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (json)
  #:use-module (guix utils)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service model location)
  #:use-module (guix-data-service model derivation)
  #:export (insert-system-tests-for-guix-revision

            select-system-tests-for-guix-revision
            system-test-derivations-for-branch))

(define (insert-system-tests-for-guix-revision conn
                                               guix-revision-id
                                               system-test-data)
  (unless (null? system-test-data)
    (let* ((system-test-ids
            (insert-missing-data-and-return-all-ids
             conn
             "system_tests"
             '(name description location_id)
             (map (match-lambda
                    ((name description derivation-file-names-by-system location-data)
                     (list name
                           description
                           (location->location-id
                            conn
                            (apply location location-data)))))
                  system-test-data)))
           (data
            (append-map
             (lambda (system-test-id derivation-file-names-by-system)
               (let ((systems
                      (map car derivation-file-names-by-system))
                     (derivation-ids
                      (derivation-file-names->derivation-ids
                       conn
                       (map cdr derivation-file-names-by-system))))
                 (map (lambda (system derivation-id)
                        (list guix-revision-id
                              system-test-id
                              derivation-id
                              system))
                      systems
                      derivation-ids)))
             system-test-ids
             (map third system-test-data))))

      (exec-query
       conn
       (string-append
        "
INSERT INTO guix_revision_system_test_derivations
  (guix_revision_id, system_test_id, derivation_id, system)
VALUES "
        (string-join
         (map (lambda (vals)
                (apply simple-format #f "(~A, ~A, ~A, '~A')"
                       vals))
              data)
         ", ")))))
  #t)

(define (select-system-tests-for-guix-revision conn
                                               system
                                               commit-hash)
  (define query
    "
SELECT system_tests.name, system_tests.description,
       locations.file, locations.line, locations.column_number,
       derivations.file_name,
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
FROM system_tests
INNER JOIN guix_revision_system_test_derivations
  ON system_tests.id = guix_revision_system_test_derivations.system_test_id
INNER JOIN locations
  ON locations.id = system_tests.location_id
INNER JOIN derivations
  ON guix_revision_system_test_derivations.derivation_id = derivations.id
INNER JOIN derivations_by_output_details_set
  ON derivations.id = derivations_by_output_details_set.derivation_id
LEFT OUTER JOIN builds
  ON derivations_by_output_details_set.derivation_output_details_set_id =
     builds.derivation_output_details_set_id
LEFT OUTER JOIN latest_build_status
  ON builds.id = latest_build_status.build_id
INNER JOIN guix_revisions
  ON guix_revisions.id = guix_revision_system_test_derivations.guix_revision_id
WHERE guix_revision_system_test_derivations.system = $1 AND
      guix_revisions.commit = $2
GROUP BY system_tests.name, system_tests.description,
         locations.file, locations.line, locations.column_number,
         derivations.file_name
ORDER BY name ASC")

  (map
   (match-lambda
     ((name description
            file line column_number
            derivation_file_name
            builds-json)
      (list name
            description
            file
            (string->number line)
            (string->number column_number)
            derivation_file_name
            (filter (lambda (build)
                      (string? (assoc-ref build "status")))
                    (vector->list
                     (json-string->scm builds-json))))))
   (exec-query conn query (list system commit-hash))))

(define (system-test-derivations-for-branch conn
                                            git-repository-id
                                            branch-name
                                            system
                                            system-test-name)
  (define query
    "
SELECT derivations.file_name,
       first_guix_revisions.commit,
       data2.first_datetime,
       last_guix_revisions.commit,
       data2.last_datetime,
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
FROM (
  SELECT DISTINCT
      derivation_id,
      first_value(guix_revision_id)
        OVER derivation_window AS first_guix_revision_id,
      first_value(datetime)
        OVER derivation_window AS first_datetime,
      last_value(guix_revision_id)
        OVER derivation_window AS last_guix_revision_id,
      last_value(datetime)
        OVER derivation_window AS last_datetime
  FROM (
    SELECT guix_revision_id,
           git_branches.datetime,
           derivation_id
    FROM guix_revision_system_test_derivations
    INNER JOIN system_tests
      ON guix_revision_system_test_derivations.system_test_id = system_tests.id
    INNER JOIN guix_revisions
      ON guix_revisions.id = guix_revision_id
    INNER JOIN git_branches
      ON guix_revisions.git_repository_id = git_branches.git_repository_id
     AND git_branches.commit = guix_revisions.commit
    WHERE system_tests.name = $1
      AND guix_revisions.git_repository_id = $2
      AND git_branches.name = $3
      AND system = $4
  ) AS data1
  WINDOW derivation_window AS (
    PARTITION BY data1.derivation_id
    ORDER BY data1.datetime ASC
    RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
  )
) AS data2
INNER JOIN guix_revisions AS first_guix_revisions
  ON first_guix_revisions.id = data2.first_guix_revision_id
INNER JOIN guix_revisions AS last_guix_revisions
  ON last_guix_revisions.id = data2.last_guix_revision_id
INNER JOIN derivations
  ON derivations.id = data2.derivation_id
INNER JOIN derivations_by_output_details_set
  ON derivations_by_output_details_set.derivation_id = derivations.id
LEFT OUTER JOIN builds
  ON derivations_by_output_details_set.derivation_output_details_set_id =
     builds.derivation_output_details_set_id
LEFT OUTER JOIN latest_build_status
  ON builds.id = latest_build_status.build_id
GROUP BY 1, 2, 3, 4, 5
ORDER BY data2.first_datetime DESC")

  (map (match-lambda
         ((derivation-file-name
           first-guix-revision-commit
           first-datetime
           last-guix-revision-commit
           last-datetime
           builds-json)
          (list derivation-file-name
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
        (list system-test-name
              (number->string git-repository-id)
              branch-name
              system))))
