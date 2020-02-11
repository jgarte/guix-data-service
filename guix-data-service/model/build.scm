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

(define-module (guix-data-service model build)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (json)
  #:use-module (guix-data-service model utils)
  #:export (select-build-stats
            select-builds-with-context
            select-builds-with-context-by-derivation-file-name
            select-build-by-build-server-and-derivation-file-name
            select-required-builds-that-failed
            update-builds-derivation-output-details-set-id
            insert-builds
            insert-build
            ensure-build-exists))

(define* (select-build-stats conn build-servers
                             #:key revision-commit
                             system target)
  (define criteria
    `(,@(if build-servers
            (list
             (string-append
              "builds.build_server_id IN ("
              (string-join (map number->string build-servers)
                           ", ")
              ")"))
            '())
      ,@(if revision-commit
            '("guix_revisions.commit = $1")
            '())
      ,@(if system
            '("package_derivations.system = $2")
            '())
      ,@(if target
            '("package_derivations.target = $3")
            '())))

  (define query
    (string-append
     "
SELECT latest_build_status.status AS build_status, build_servers.id, COUNT(*)
FROM derivation_output_details_sets
CROSS JOIN build_servers
"
     (if revision-commit
         "
INNER JOIN derivations_by_output_details_set
  ON derivation_output_details_sets.id =
     derivations_by_output_details_set.derivation_output_details_set_id
INNER JOIN package_derivations
  ON derivations_by_output_details_set.derivation_id = package_derivations.derivation_id
INNER JOIN guix_revision_package_derivations
  ON guix_revision_package_derivations.package_derivation_id = package_derivations.id
INNER JOIN guix_revisions
  ON guix_revision_package_derivations.revision_id = guix_revisions.id"
         "")
     "
LEFT JOIN builds
   ON builds.derivation_output_details_set_id =
        derivation_output_details_sets.id AND
      builds.build_server_id = build_servers.id
LEFT JOIN
(
  SELECT DISTINCT ON (build_id) *
  FROM build_status
  ORDER BY build_id, timestamp DESC
) AS latest_build_status
ON builds.id = latest_build_status.build_id
"
     (if (null? criteria)
         ""
         (string-append
          "WHERE "
          (string-join criteria " AND ")))
     "
GROUP BY latest_build_status.status, build_servers.id
ORDER BY status"))

  (map (match-lambda
         (((build-status) . data)
          (list build-status
                (map (match-lambda
                       ((build-server-id count)
                        (cons (string->number build-server-id)
                              (string->number count))))
                     data))))
       (group-list-by-first-n-fields
        1
        (exec-query conn
                    query
                    `(,@(if revision-commit
                            (list revision-commit)
                            '())
                      ,@(if system
                            (list system)
                            '())
                      ,@(if target
                            (list target)
                            '()))))))

(define* (select-builds-with-context conn build-statuses build-server-ids
                                     #:key revision-commit
                                     system target)
  (define where-conditions
    (filter
     string?
     (list
      (when (list? build-statuses)
        (string-append
         "latest_build_status.status IN ("
         (string-join (map quote-string build-statuses)
                      ",")
         ")"))
      (when (list? build-server-ids)
        (string-append
         "builds.build_server_id IN ("
         (string-join (map number->string build-server-ids)
                      ", ")
         ")"))
      (when revision-commit
        "guix_revisions.commit = $1")
      (when system
        "package_derivations.system = $2")
      (when target
        "package_derivations.target = $3"))))

  (define query
    (string-append
     "
SELECT builds.id, build_servers.url, derivations.file_name,
       latest_build_status.timestamp, latest_build_status.status
FROM builds
INNER JOIN build_servers ON build_servers.id = builds.build_server_id
INNER JOIN derivations ON derivations.file_name = builds.derivation_file_name
"
     (if revision-commit
         "
INNER JOIN derivations_by_output_details_set
  ON builds.derivation_output_details_set_id =
     derivations_by_output_details_set.derivation_output_details_set_id
INNER JOIN package_derivations
  ON derivations_by_output_details_set.derivation_id = package_derivations.derivation_id
INNER JOIN guix_revision_package_derivations
  ON guix_revision_package_derivations.package_derivation_id = package_derivations.id
INNER JOIN guix_revisions
  ON guix_revision_package_derivations.revision_id = guix_revisions.id"
         "")
     "
INNER JOIN
(
  SELECT DISTINCT ON (build_id) *
  FROM build_status
  ORDER BY build_id, timestamp DESC
) AS latest_build_status
ON latest_build_status.build_id = builds.id
"
                   (if (null? where-conditions)
                       ""
                       (string-append
                        "WHERE "
                        (string-join where-conditions " AND ")))
                   "
ORDER BY latest_build_status.timestamp DESC
LIMIT 100"))

  (exec-query conn
              query
              `(,@(if revision-commit
                      (list revision-commit)
                      '())
                ,@(if system
                      (list system)
                      '())
                ,@(if target
                      (list target)
                      '()))))

(define (select-builds-with-context-by-derivation-file-name
         conn derivation-file-name)
  (define query
    "
SELECT build_servers.id,
       build_servers.url,
       latest_build_status.timestamp,
       latest_build_status.status
FROM builds
INNER JOIN build_servers ON build_servers.id = builds.build_server_id
INNER JOIN
(
  SELECT DISTINCT ON (build_id) *
  FROM build_status
  ORDER BY build_id, timestamp DESC
) AS latest_build_status
  ON latest_build_status.build_id = builds.id
INNER JOIN derivations_by_output_details_set
  ON builds.derivation_output_details_set_id =
     derivations_by_output_details_set.derivation_output_details_set_id
INNER JOIN derivations
  ON derivations.id = derivations_by_output_details_set.derivation_id
WHERE derivations.file_name = $1
ORDER BY latest_build_status.timestamp DESC")

  (exec-query conn query (list derivation-file-name)))

(define (select-build-by-build-server-and-derivation-file-name
         conn build-server-id derivation-file-name)
  (define query
    "
SELECT build_servers.url,
       JSON_AGG(
         json_build_object(
           'timestamp', build_status.timestamp,
           'status', build_status.status
         )
         ORDER BY build_status.timestamp
       ) AS statuses
FROM builds
INNER JOIN build_servers
  ON build_servers.id = builds.build_server_id
INNER JOIN build_status
  ON builds.id = build_status.build_id
INNER JOIN derivations_by_output_details_set
  ON builds.derivation_output_details_set_id =
     derivations_by_output_details_set.derivation_output_details_set_id
INNER JOIN derivations
  ON derivations.id = derivations_by_output_details_set.derivation_id
WHERE build_server_id = $1 AND
      derivations.file_name = $2
GROUP BY build_servers.url")

  (match (exec-query conn
                     query
                     (list (number->string build-server-id)
                           derivation-file-name))
    (((build-server-url statuses-json))
     (list build-server-url
           (json-string->scm statuses-json)))
    (()
     #f)))

(define (select-required-builds-that-failed conn build-server-id derivation-file-name)
  (define query
    "
WITH RECURSIVE all_derivations(id, file_name) AS (
    SELECT derivations.id, derivations.file_name
    FROM derivations
    WHERE file_name = $1
  UNION
    SELECT derivations.id, derivations.file_name
    FROM all_derivations
    INNER JOIN derivation_inputs
      ON all_derivations.id = derivation_inputs.derivation_id
    INNER JOIN derivation_outputs
      ON derivation_inputs.derivation_output_id = derivation_outputs.id
    INNER JOIN derivations
      ON derivation_outputs.derivation_id = derivations.id
)
SELECT all_derivations.file_name, latest_build_status.status
FROM all_derivations
INNER JOIN derivations_by_output_details_set
  ON all_derivations.id = derivations_by_output_details_set.derivation_id
LEFT OUTER JOIN builds
  ON derivations_by_output_details_set.derivation_output_details_set_id =
     builds.derivation_output_details_set_id
 AND builds.build_server_id = $2
LEFT OUTER JOIN (
  SELECT DISTINCT ON (build_id) *
  FROM build_status
  ORDER BY build_id, timestamp DESC
) AS latest_build_status
  ON builds.id = latest_build_status.build_id
WHERE latest_build_status.status = 'failed'")

  (exec-query conn
              query
              (list derivation-file-name
                    (number->string build-server-id))))

(define (select-build-id-by-build-server-and-derivation-file-name
         conn build-server-id derivation-file-name)
  (define query
    "
SELECT id
FROM builds
WHERE build_server_id = $1 AND derivation_file_name = $2")

  (match (exec-query conn
                     query
                     (list (number->string build-server-id)
                           derivation-file-name))
    (((id))
     (string->number id))
    (_
     #f)))

(define (update-builds-derivation-output-details-set-id conn derivation-file-names)
  (exec-query
   conn
   (string-append
    "
UPDATE builds SET derivation_output_details_set_id = (
  SELECT derivations_by_output_details_set.derivation_output_details_set_id
  FROM derivations_by_output_details_set
  INNER JOIN derivations
    ON derivations.file_name = builds.derivation_file_name
  WHERE derivations_by_output_details_set.derivation_id = derivations.id
) WHERE builds.derivation_output_details_set_id IS NULL AND
        builds.derivation_file_name IN ("
    (string-join (map quote-string derivation-file-names)
                 ",")
    ")")))

(define (select-derivations-by-output-details-set-id-by-derivation-file-name
         conn
         derivation-file-name)
  (match (exec-query
          conn
          "
SELECT derivation_output_details_set_id
FROM derivations_by_output_details_set
INNER JOIN derivations
  ON derivations.id = derivations_by_output_details_set.derivation_id
WHERE derivations.file_name = $1"
          (list derivation-file-name))
    (((id))
     (string->number id))
    (_
     #f)))

(define (insert-builds conn build-server-id derivation-file-names)
  (let ((build-ids
         (insert-missing-data-and-return-all-ids
          conn
          "builds"
          '(build_server_id derivation_file_name)
          (map (lambda (derivation-file-name)
                 (list build-server-id
                       derivation-file-name))
               derivation-file-names)
          #:delete-duplicates? #t)))

    (exec-query
     conn
     (string-append
      "
UPDATE builds SET derivation_output_details_set_id = (
  SELECT derivations_by_output_details_set.derivation_output_details_set_id
  FROM derivations_by_output_details_set
  INNER JOIN derivations
    ON derivations.file_name = builds.derivation_file_name
  WHERE derivations_by_output_details_set.derivation_id = derivations.id
) WHERE builds.derivation_output_details_set_id IS NULL AND builds.id IN ("
      (string-join (map number->string
                        build-ids)
                   ",")
      ")"))

    build-ids))

(define (insert-build conn build-server-id derivation-file-name)
  (match (exec-query
          conn
          (string-append
           "
INSERT INTO builds
  (build_server_id, derivation_file_name, derivation_output_details_set_id)
VALUES ("
           (number->string build-server-id)
           ", "
           (quote-string derivation-file-name)
           ", "
           (or
            (and=>
             (select-derivations-by-output-details-set-id-by-derivation-file-name
              conn
              derivation-file-name)
             number->string)
            "NULL")
           ")
RETURNING (id)"))
    (((id))
     (string->number id))))

(define (ensure-build-exists conn
                             build-server-id
                             derivation-file-name)
  (let ((existing-build-id
         (select-build-id-by-build-server-and-derivation-file-name
          conn build-server-id derivation-file-name)))

    (if existing-build-id
        existing-build-id
        (insert-build conn
                      build-server-id
                      derivation-file-name))))

