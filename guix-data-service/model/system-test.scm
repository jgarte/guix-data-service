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
            select-system-tests-for-guix-revision))

(define (insert-system-tests-for-guix-revision conn
                                               guix-revision-id
                                               system-test-data)
  (let ((system-test-ids
         (insert-missing-data-and-return-all-ids
          conn
          "system_tests"
          '(name description location_id)
          (map (match-lambda
                 ((name description derivation-file-name location-data)
                  (list name
                        description
                        (location->location-id
                         conn
                         (apply location location-data)))))
               system-test-data)))
        (derivation-ids
         (derivation-file-names->derivation-ids
          conn
          (map third system-test-data))))

    (exec-query
     conn
     (string-append
      "
INSERT INTO guix_revision_system_test_derivations
  (guix_revision_id, system_test_id, derivation_id)
VALUES "
      (string-join
       (map (lambda (system-test-id derivation-id)
              (simple-format #f "(~A, ~A, ~A)"
                             guix-revision-id
                             system-test-id
                             derivation-id))
            system-test-ids
            derivation-ids)
       ", "))))
  #t)

(define (select-system-tests-for-guix-revision conn
                                               commit-hash)
  (define query
    "
SELECT system_tests.name, system_tests.description,
       locations.file, locations.line, locations.column_number,
       derivations.file_name,
       JSON_AGG(
         json_build_object(
           'build_server_id', builds.build_server_id,
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
LEFT OUTER JOIN (
  SELECT DISTINCT ON (build_id) *
  FROM build_status
  ORDER BY build_id, timestamp DESC
) AS latest_build_status
  ON builds.id = latest_build_status.build_id
INNER JOIN guix_revisions
  ON guix_revisions.id = guix_revision_system_test_derivations.guix_revision_id
WHERE guix_revisions.commit = $1
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
                      (assoc-ref build "status"))
                    (vector->list
                     (json-string->scm builds-json))))))
   (exec-query conn query (list commit-hash))))
