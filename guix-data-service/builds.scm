(define-module (guix-data-service builds)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:use-module (json parser)
  #:use-module (web response)
  #:use-module (web client)
  #:use-module (squee)
  #:use-module (guix scripts substitute)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service builds)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service model build)
  #:use-module (guix-data-service model build-server)
  #:use-module (guix-data-service model build-status)
  #:use-module (guix-data-service model nar)
  #:export (query-build-servers))

(define (query-build-servers conn build-server-ids revision-commits)
  (while #t
    (let ((build-servers (select-build-servers conn)))
      (for-each
       (match-lambda
         ((id url lookup-all-derivations?)
          (when (or (not build-servers)
                    (member id build-server-ids))
            (when lookup-all-derivations?
              (simple-format #t "\nQuerying ~A\n" url)
              (query-build-server conn id url revision-commits)))))
       build-servers))))

(define (query-build-server conn id url revision-commits)
  (simple-format #t "\nFetching pending builds\n")
  (process-pending-builds conn id url)
  (simple-format #t "\nFetching unseen derivations\n")
  (process-derivations conn id url revision-commits)
  (simple-format #t "\nFetching narinfo files\n")
  (fetch-narinfo-files conn id url revision-commits))

(define (insert-build-statuses-from-data conn build-server-id build-id data)
  (define stop-statuses
    (lset-difference string=?
                     build-status-strings
                     '("scheduled" "started")))

  (let* ((status-string
          (assq-ref build-statuses
                    (assoc-ref data "buildstatus")))
         (finished?
          (member status-string stop-statuses))
         (existing-status-entries
          (map second
               (select-build-statuses-by-build-id conn
                                                  build-id
                                                  build-server-id)))
        (timestamp
         (assoc-ref data "timestamp"))
        (starttime
         (assoc-ref data "starttime"))
        (stoptime
         (assoc-ref data "stoptime")))
    (map (match-lambda
           ((timestamp status)
            (insert-build-status conn build-id timestamp status)))
         (filter
          list?
          (list
           (when (and
                  ;; Cuirass returns the stoptime as the timestamp if the
                  ;; build has finished, so only use the timestamp if the
                  ;; build hasn't finished.
                  (not finished?)
                  (not (member "scheduled" existing-status-entries)))
             (list timestamp "scheduled"))
           (when (and (< 0 starttime)
                      (not (member "started" existing-status-entries)))
             (list starttime "started"))
           (when (and (< 0 stoptime)
                      (not (member status-string existing-status-entries)))
             (list stoptime status-string)))))))

(define (process-pending-builds conn build-server-id url)
  (for-each
   (match-lambda
     ((build-id derivation-file-name)
      (match (fetch-build url derivation-file-name)
        (#f
         (display ".")
         #f)
        (()
         (display ".")
         #f)
        (data
         (insert-build-statuses-from-data
          conn
          build-server-id
          build-id
          data)
         (display "-")))
      ;; Try not to make to many requests at once
      (usleep 200)))
   (select-pending-builds conn build-server-id)))

(define (process-derivations conn build-server-id url revision-commits)
  (define derivations
    (select-derivations-with-no-known-build conn revision-commits))

  (simple-format (current-error-port) "Fetching ~A derivations\n"
                 (length derivations))
  (for-each
   (match-lambda
     ((derivation-id derivation-file-name)
      (if
       (and=> (fetch-build url derivation-file-name)
              (lambda (data)
                (let ((build-id
                       (ensure-build-exists conn
                                            build-server-id
                                            derivation-file-name)))
                  (insert-build-statuses-from-data
                   conn
                   build-server-id
                   build-id
                   data))
                #t))
       (display "-")
       (display "."))
      ;; Try not to make to many requests at once
      (usleep 200)))
   derivations))

(define (json-string->scm* string)
  (catch
    'json-invalid
    (lambda ()
      (json-string->scm string))
    (lambda args
      (display args)
      (newline)
      (simple-format #t "\nerror parsing: ~A\n" string)
      #f)))

(define (fetch-build url derivation-file-name)
  (define build-url
    (string-append url
                   "build"
                   (string-drop
                    derivation-file-name
                    (string-length "/gnu/store"))))

  (let-values
      (((response body)
        (http-request build-url)))

    (cond
     ((eq? (response-code response) 200)
      (json-string->scm
       (bytevector->string body "utf-8")))
     (else
      #f))))

(define (select-pending-builds conn build-server-id)
  (define query
    "
SELECT builds.id, derivations.file_name
FROM derivations
INNER JOIN builds
  ON derivations.file_name = builds.derivation_file_name
LEFT JOIN (
  SELECT DISTINCT ON (build_id) *
  FROM build_status
  ORDER BY build_id, timestamp DESC
) AS latest_build_status
ON builds.id = latest_build_status.build_id
WHERE builds.build_server_id = $1 AND
      latest_build_status.status IN (
        'scheduled', 'started'
      )
ORDER BY latest_build_status.status DESC -- 'started' first
LIMIT 1000")

  (map
   (match-lambda
     ((build-id derivation-file-name)
      (list (string->number build-id)
            derivation-file-name)))
   (exec-query conn query (list (number->string build-server-id)))))

(define (select-derivations-with-no-known-build conn revision-commits)
  (define query
    ;; Only select derivations that are in the package_derivations table, as
    ;; Cuirass doesn't build the intermediate derivations
    (string-append
     "
SELECT derivations.id, derivations.file_name
FROM derivations
WHERE derivations.id NOT IN (
  SELECT unnest(equivalent_derivations.derivation_ids)
  FROM equivalent_derivations
  WHERE ARRAY[(
    SELECT derivations.id WHERE derivations.file_name IN (SELECT derivation_file_name FROM builds)
  )] <@ equivalent_derivations.derivation_ids
) AND derivations.id IN (
  SELECT unnest(derivation_ids)
  FROM package_derivations"
     (if (null? revision-commits)
         "\n"
         (string-append
          "
  INNER JOIN guix_revision_package_derivations
    ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
  INNER JOIN guix_revisions
    ON guix_revisions.id = guix_revision_package_derivations.revision_id
  INNER JOIN equivalent_derivations
    ON ARRAY[derivation_id] <@ equivalent_derivations.derivation_ids
  WHERE guix_revisions.commit IN ("
          (string-join (map quote-string revision-commits) ",")
          ")"
          ))
     "
  -- TODO: Filter better on what systems and targets build servers use
  AND package_derivations.system = 'x86_64-linux'
  AND package_derivations.target = 'x86_64-linux'
)
LIMIT 15000"))

  (exec-query conn query))

(define (fetch-narinfo-files conn build-server-id build-server-url revision-commits)
  (define outputs
    (select-outputs-for-successful-builds-without-known-nar-entries
     conn
     build-server-id
     revision-commits))

  (simple-format #t "Querying ~A outputs\n"
                 (length outputs))

  (let ((narinfos
         (lookup-narinfos (string-trim-right build-server-url #\/) outputs)))

    (simple-format #t "Got ~A narinfo files\n"
                   (length narinfos))

    (unless (eq? (length narinfos) 0)
      (with-postgresql-transaction
       conn
       (lambda (conn)
         (record-narinfo-details-and-return-ids
          conn
          build-server-id
          narinfos))))))
