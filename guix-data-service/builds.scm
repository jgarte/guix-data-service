(define-module (guix-data-service builds)
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
          (when (string=? lookup-all-derivations? "t")
            (query-build-server conn id url))))
       build-servers))))

(define (query-build-server conn id url)
  (simple-format #t "\nFetching pending builds\n")
  (process-pending-builds conn id url)
  (simple-format #t "\nFetching unseen derivations\n")
  (process-derivations conn id url))

(define (process-pending-builds conn build-server-id url)
  (for-each
   (match-lambda
     ((build-id internal-build-id derivation-id derivation-file-name)
      (match (fetch-build url build-id)
        (#f #f)
        (() #f)
        (status
         (insert-build-status conn
                              internal-build-id
                              (assoc-ref status "starttime")
                              (assoc-ref status "stoptime")
                              (assq-ref build-statuses
                                        (assoc-ref status "buildstatus")))))
      (display ".")
      ;; Try not to make to many requests at once
      (usleep 200)))
   (select-pending-builds conn build-server-id)))

(define (process-derivations conn build-server-id url)
  (for-each
   (match-lambda
     ((derivation-id derivation-file-name)
      (and=> (fetch-build-for-derivation url derivation-file-name)
             (lambda (status)
               (let ((internal-build-id
                      (ensure-build-exists conn
                                           build-server-id
                                           (assoc-ref status "id")
                                           derivation-id
                                           (assoc-ref status "timestamp"))))

                 (insert-build-status conn
                                      internal-build-id
                                      (assoc-ref status "starttime")
                                      (assoc-ref status "stoptime")
                                      (assq-ref build-statuses
                                                (assoc-ref status "buildstatus"))))))
      (display ".")
      ;; Try not to make to many requests at once
      (usleep 200)))
   (select-derivations-with-no-known-build conn)))

(define (fetch-build-for-derivation url derivation-file-name)
  (match (fetch-latest-builds-for-derivation url derivation-file-name)
    ((or #f #())
     (match (fetch-queued-builds-for-derivation url derivation-file-name)
       ((or #f #())
        (simple-format #t "\nwarning: couldn't find build for ~A on ~A\n"
                       derivation-file-name
                       url)
        #f)
       (#(status)
        status)))
    (#(status)
     status)))

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

(define (fetch-latest-builds-for-derivation base-url derivation-file-name)
  (define url
    (string-append base-url
                   "api/latestbuilds?nr=1"
                   "&derivation=" derivation-file-name))

  (let-values (((response body) (http-request url)))
    (let ((code (response-code response)))
      (cond
       ((eq? code 200)
        (json-string->scm
         (bytevector->string body "utf-8")))
       (else
        (simple-format #t "\nerror: response code ~A: ~A\n" url code)
        #f)))))

(define (fetch-queued-builds-for-derivation base-url derivation-file-name)
  (define url
    (string-append base-url
                   "api/queue?nr=1"
                   "&derivation=" derivation-file-name))

  (let-values (((response body) (http-request url)))
    (let ((code (response-code response)))
      (cond
       ((eq? code 200)
        (json-string->scm
         (bytevector->string body "utf-8")))
       (else
        (simple-format #t "\nerror: response code ~A: ~A\n" url code)
        #f)))))

(define (fetch-build url id)
  (let-values
      (((response body)
        (http-request (string-append url "build/" id))))

    (cond
     ((eq? (response-code response) 200)
      (json-string->scm
       (bytevector->string body "utf-8")))
     (else
      (simple-format #t "\nwarning: couldn't find build ~A on ~A\n"
                     id
                     url)
      #f))))

(define (select-pending-builds conn build-server-id)
  (define query
    (string-append
     "SELECT builds.id, builds.internal_id, derivations.id, derivations.file_name "
     "FROM derivations "
     "INNER JOIN builds "
     "ON derivations.id = builds.derivation_id "
     "INNER JOIN build_status "
     "ON builds.internal_id = build_status.internal_build_id "
     "WHERE builds.build_server_id = $1 AND "
     "build_status.status IN ("
     "'scheduled', 'started'"
     ") "
     "LIMIT 1000"))

  (exec-query conn query (list build-server-id)))

(define (select-derivations-with-no-known-build conn)
  (define query
    (string-append
     "SELECT derivations.id, derivations.file_name "
     "FROM derivations "
     "WHERE derivations.id NOT IN ("
     "SELECT derivation_id FROM builds"
     ") "
     "LIMIT 1000"))

  (exec-query conn query))
