(define-module (guix-data-service builds)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:use-module (json parser)
  #:use-module (web response)
  #:use-module (web client)
  #:use-module (squee)
  #:use-module (guix-data-service builds)
  #:use-module (guix-data-service model build)
  #:use-module (guix-data-service model build-server)
  #:use-module (guix-data-service model build-status)
  #:export (query-build-servers))

(define (query-build-servers conn)
  (while #t
    (let ((build-servers (select-build-servers conn)))
      (for-each
       (match-lambda
         ((id url lookup-all-derivations?)
          (when lookup-all-derivations?
            (query-build-server conn id url))))
       build-servers))))

(define (query-build-server conn id url)
  (simple-format #t "\nFetching pending builds\n")
  (process-pending-builds conn id url)
  (simple-format #t "\nFetching unseen derivations\n")
  (process-derivations conn id url))

(define (insert-build-statuses-from-data conn build-server-id build-id data)
  (define stop-statuses
    (lset-difference string=?
                     build-status-strings
                     '("scheduled" "started")))

  (let ((status-string
         (assq-ref build-statuses
                   (assoc-ref data "buildstatus")))
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
           (unless (member "scheduled" existing-status-entries)
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

(define (process-derivations conn build-server-id url)
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
   (select-derivations-with-no-known-build conn)))

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
  (let-values
      (((response body)
        (http-request (string-append
                       url
                       (string-append
                        "build"
                        (string-drop
                         derivation-file-name
                         (string-length "/gnu/store")))))))

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
INNER JOIN build_status
  ON builds.id = build_status.build_id
WHERE builds.build_server_id = $1 AND
      build_status.status IN (
        'scheduled', 'started'
      )
LIMIT 1000")

  (map
   (match-lambda
     ((build-id derivation-file-name)
      (list (string->number build-id)
            derivation-file-name)))
   (exec-query conn query (list (number->string build-server-id)))))

(define (select-derivations-with-no-known-build conn)
  (define query
    ;; Only select derivations that are in the package_derivations table, as
    ;; Cuirass doesn't build the intermediate derivations
    "
SELECT derivations.id, derivations.file_name
FROM derivations
WHERE derivations.file_name NOT IN (
  SELECT derivation_file_name FROM builds
) AND derivations.id IN (
  SELECT derivation_id FROM package_derivations
)
LIMIT 15000")

  (exec-query conn query))
