(define-module (guix-data-service model build)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix-data-service model utils)
  #:export (select-build-stats
            select-builds-with-context
            select-builds-with-context-by-derivation-file-name
            select-build-by-build-server-and-derivation-file-name
            insert-builds
            insert-build
            ensure-build-exists))

(define (select-build-stats conn)
  (define query
    "
SELECT latest_build_status.status AS build_status, COUNT(*)
FROM derivations
LEFT JOIN builds ON builds.derivation_file_name = derivations.file_name
LEFT JOIN
(
  SELECT DISTINCT ON (build_id) *
  FROM build_status
  ORDER BY build_id, timestamp DESC
) AS latest_build_status
ON builds.id = latest_build_status.build_id
GROUP BY latest_build_status.status
ORDER BY status")

  (exec-query conn query))

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

(define (insert-builds conn build-server-id derivation-file-names)
  (insert-missing-data-and-return-all-ids
   conn
   "builds"
   '(build_server_id derivation_file_name)
   (map (lambda (derivation-file-name)
          (list build-server-id
                derivation-file-name))
        derivation-file-names)
   #:delete-duplicates? #t))

(define (insert-build conn build-server-id derivation-file-name)
  (match (exec-query conn
                     "
INSERT INTO builds (build_server_id, derivation_file_name)
VALUES ($1, $2)
RETURNING (id)"
                     (list (number->string build-server-id)
                           derivation-file-name))
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

