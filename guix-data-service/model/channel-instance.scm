;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2020 Christopher Baines <mail@cbaines.net>
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

(define-module (guix-data-service model channel-instance)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (json)
  #:use-module (guix utils)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service model derivation)
  #:export (insert-channel-instances
            select-channel-instances-for-guix-revision))

(define (insert-channel-instances conn
                                  guix-revision-id
                                  derivations-by-system)
  (let ((derivation-ids
         (derivation-file-names->derivation-ids
          conn
          (map cdr derivations-by-system))))

    (exec-query
     conn
     (string-append
      "
INSERT INTO channel_instances
  (guix_revision_id, system, derivation_id)
VALUES "
      (string-join
       (map (lambda (system derivation-id)
              (simple-format #f "(~A, '~A', ~A)"
                             guix-revision-id
                             system
                             derivation-id))
            (map car derivations-by-system)
            derivation-ids)
       ", "))))
  #t)

(define (select-channel-instances-for-guix-revision conn
                                                    commit-hash)
  (define query
    "
SELECT channel_instances.system,
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
FROM channel_instances
INNER JOIN derivations
  ON channel_instances.derivation_id = derivations.id
INNER JOIN derivations_by_output_details_set
  ON derivations.id = derivations_by_output_details_set.derivation_id
LEFT OUTER JOIN builds
  ON derivations_by_output_details_set.derivation_output_details_set_id =
     builds.derivation_output_details_set_id
LEFT OUTER JOIN latest_build_status
  ON builds.id = latest_build_status.build_id
INNER JOIN guix_revisions
  ON guix_revisions.id = channel_instances.guix_revision_id
WHERE guix_revisions.commit = $1
GROUP BY channel_instances.system, derivations.file_name
ORDER BY channel_instances.system DESC")

  (map
   (match-lambda
     ((system derivation_file_name builds-json)
      (list system
            derivation_file_name
            (filter (lambda (build)
                      (string? (assoc-ref build "status")))
                    (vector->list
                     (json-string->scm builds-json))))))
   (exec-query conn query (list commit-hash))))
