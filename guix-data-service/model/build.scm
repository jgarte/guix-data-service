(define-module (guix-data-service model build)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix-data-service model utils)
  #:export (select-build-stats
            select-builds-with-context
            select-builds-with-context-by-derivation-file-name
            select-build-by-build-server-and-derivation-file-name
            update-builds-derivation-output-details-set-id
            insert-builds
            insert-build
            ensure-build-exists))

(define (select-build-stats conn build-servers)
  (define query
    (string-append
     "
SELECT latest_build_status.status AS build_status, builds.build_server_id, COUNT(*)
FROM derivation_output_details_sets
LEFT JOIN builds
   ON builds.derivation_output_details_set_id =
      derivation_output_details_sets.id
LEFT JOIN
(
  SELECT DISTINCT ON (build_id) *
  FROM build_status
  ORDER BY build_id, timestamp DESC
) AS latest_build_status
ON builds.id = latest_build_status.build_id
"
     (if build-servers
         (string-append
          "WHERE builds.build_server_id IN ("
          (string-join (map number->string build-servers)
                       ", ")
          ")")
         "")
     "
GROUP BY latest_build_status.status, builds.build_server_id
ORDER BY status"))

  (map (match-lambda
         (((build-status) . data)
          (list build-status
                (map (match-lambda
                       ((build-server-id count)
                        (cons (string->number build-server-id)
                              (string->number count))))
                     data))))
       (group-list-by-first-n-fields 1 (exec-query conn query))))

(define (select-builds-with-context conn build-statuses build-server-ids)
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
         ")")))))

  (define query
    (string-append "
SELECT builds.id, build_servers.url, derivations.file_name,
       latest_build_status.timestamp, latest_build_status.status
FROM builds
INNER JOIN build_servers ON build_servers.id = builds.build_server_id
INNER JOIN derivations ON derivations.file_name = builds.derivation_file_name
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

  (exec-query conn query))

(define (select-builds-with-context-by-derivation-file-name
         conn derivation-file-name)
  (define query
    "
SELECT build_servers.url,
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
WHERE builds.derivation_file_name = $1
ORDER BY latest_build_status.timestamp DESC")

  (exec-query conn query (list derivation-file-name)))

(define (select-build-by-build-server-and-derivation-file-name
         conn build-server-id derivation-file-name)
  (define query
    "
SELECT id, build_server_id, derivation_file_name
FROM builds
WHERE build_server_id = $1 AND derivation_file_name = $2")

  (match (exec-query conn
                     query
                     (list (number->string build-server-id)
                           derivation-file-name))
    ((id build_server_id derivation_file_name)
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
          "
INSERT INTO builds
  (build_server_id, derivation_file_name, derivation_output_details_set_id)
VALUES ($1, $2, $3)
RETURNING (id)"
          (list (number->string build-server-id)
                derivation-file-name
                (or
                 (and=> (select-derivations-by-output-details-set-id-by-derivation-file-name
                         conn
                         derivation-file-name)
                        number->string)
                 "NULL")))
    (((id))
     (string->number id))))

(define (ensure-build-exists conn
                             build-server-id
                             derivation-file-name)
  (let ((existing-build-id
         (select-build-by-build-server-and-derivation-file-name
          conn build-server-id derivation-file-name)))

    (if existing-build-id
        existing-build-id
        (insert-build conn
                      build-server-id
                      derivation-file-name))))

