(define-module (guix-data-service model build)
  #:use-module (squee)
  #:export (select-build-stats
            select-builds-with-context
            select-builds-with-context-by-derivation-id
            select-build-by-build-server-and-id
            insert-build
            ensure-build-exists))

(define (select-build-stats conn)
  (define query
    (string-append
     "SELECT latest_build_status.status AS build_status, COUNT(*) "
     "FROM derivations "
     "FULL OUTER JOIN builds ON builds.derivation_id = derivations.id "
     "FULL OUTER JOIN "
     "(SELECT DISTINCT ON (internal_build_id) * FROM build_status "
     "ORDER BY internal_build_id, status_fetched_at DESC"
     ") AS latest_build_status "
     "ON builds.internal_id = latest_build_status.internal_build_id "
     "GROUP BY (builds.id IS NULL), latest_build_status.status "
     "ORDER BY build_status"))

  (exec-query conn query))

(define (select-builds-with-context conn)
  (define query
    (string-append
     "SELECT builds.id, build_servers.url, derivations.file_name, "
     "latest_build_status.status_fetched_at, latest_build_status.starttime, "
     "latest_build_status.stoptime, latest_build_status.status "
     "FROM builds "
     "INNER JOIN build_servers ON build_servers.id = builds.build_server_id "
     "INNER JOIN derivations ON derivations.id = builds.derivation_id "
     "INNER JOIN "
     "(SELECT DISTINCT ON (internal_build_id) * "
     "FROM build_status "
     "ORDER BY internal_build_id, status_fetched_at DESC"
     ") AS latest_build_status "
     "ON latest_build_status.internal_build_id = builds.internal_id "
     "ORDER BY latest_build_status.status_fetched_at DESC "
     "LIMIT 100"))

  (exec-query conn query))

(define (select-builds-with-context-by-derivation-id conn derivation-id)
  (define query
    (string-append
     "SELECT builds.id, build_servers.url, "
     "latest_build_status.status_fetched_at, latest_build_status.starttime, "
     "latest_build_status.stoptime, latest_build_status.status "
     "FROM builds "
     "INNER JOIN build_servers ON build_servers.id = builds.build_server_id "
     "INNER JOIN "
     "(SELECT DISTINCT ON (internal_build_id) * "
     "FROM build_status "
     "ORDER BY internal_build_id, status_fetched_at DESC"
     ") AS latest_build_status "
     "ON latest_build_status.internal_build_id = builds.internal_id "
     "WHERE builds.derivation_id = $1 "
     "ORDER BY latest_build_status.status_fetched_at DESC "))

  (exec-query conn query (list (number->string derivation-id))))

(define (select-build-by-build-server-and-id
         conn build-server-id id)
  (exec-query conn
              (string-append
               "SELECT internal_id, id, build_server_id, "
               "derivation_id, timestamp "
               "FROM builds "
               "WHERE build_server_id = $1 AND id = $2")
              (list build-server-id
                    (number->string id))))

(define (insert-build conn id build-server-id derivation-id timestamp)
  (caar
   (exec-query conn
               (string-append
                "INSERT INTO builds "
                "(id, build_server_id, derivation_id, timestamp) "
                "VALUES "
                "($1, $2, $3, to_timestamp($4))"
                "RETURNING "
                "(internal_id)")
               (list (number->string id)
                     build-server-id
                     derivation-id
                     (number->string timestamp)))))

(define (ensure-build-exists conn build-server-id id
                             derivation-id timestamp)
  (let ((existing-build
         (select-build-by-build-server-and-id
          conn build-server-id id)))

    (if (null? existing-build)
        (insert-build conn
                      id
                      build-server-id
                      derivation-id
                      timestamp)
        (caar existing-build))))

