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

(define-module (guix-data-service builds)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 binary-ports)
  #:use-module (json parser)
  #:use-module (web uri)
  #:use-module (web http)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web client)
  #:use-module (squee)
  #:use-module ((guix build download)
                #:select (close-connection
                          (open-connection-for-uri
                           . guix:open-connection-for-uri)))
  #:use-module ((guix build utils) #:select (dump-port))
  #:use-module (guix scripts substitute)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service builds)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service model build)
  #:use-module (guix-data-service model build-server)
  #:use-module (guix-data-service model build-status)
  #:use-module (guix-data-service model nar)
  #:export (query-build-servers))

(define (at-most max-length lst)
  "If LST is shorter than MAX-LENGTH, return it; otherwise return its
MAX-LENGTH first elements."
  (let loop ((len 0)
             (lst lst)
             (result '()))
    (match lst
      (()
       (reverse result))
      ((head . tail)
       (if (>= len max-length)
           (reverse result)
           (loop (+ 1 len) tail (cons head result)))))))

(define* (http-multiple-get base-uri proc seed requests
                            #:key port (verify-certificate? #t)
                            (batch-size 1000))
  "Send all of REQUESTS to the server at BASE-URI.  Call PROC for each
response, passing it the request object, the response, a port from which to
read the response body, and the previous result, starting with SEED, à la
'fold'.  Return the final result.  When PORT is specified, use it as the
initial connection on which HTTP requests are sent."
  (let connect ((port     port)
                (requests requests)
                (result   seed))
    (define batch
      (at-most batch-size requests))

    ;; (format (current-error-port) "connecting (~a requests left)..."
    ;;         (length requests))
    (let ((p (or port (guix:open-connection-for-uri
                       base-uri
                       #:verify-certificate?
                       verify-certificate?))))
      ;; For HTTPS, P is not a file port and does not support 'setvbuf'.
      (when (file-port? p)
        (setvbuf p 'block (expt 2 16)))

      ;; Send BATCH in a row.
      ;; XXX: Do our own caching to work around inefficiencies when
      ;; communicating over TLS: <http://bugs.gnu.org/22966>.
      (let-values (((buffer get) (open-bytevector-output-port)))
        ;; Inherit the HTTP proxying property from P.
        (set-http-proxy-port?! buffer (http-proxy-port? p))

        (for-each (cut write-request <> buffer)
                  batch)
        (put-bytevector p (get))
        (force-output p))

      ;; Now start processing responses.
      (let loop ((sent      batch)
                 (processed 0)
                 (result    result))
        (match sent
          (()
           (match (drop requests processed)
             (()
              (close-port p)
              (reverse result))
             (remainder
              (connect p remainder result))))
          ((head tail ...)
           (let* ((resp   (read-response p))
                  (body   (response-body-port resp))
                  (result (proc head resp body result)))
             ;; The server can choose to stop responding at any time, in which
             ;; case we have to try again.  Check whether that is the case.
             ;; Note that even upon "Connection: close", we can read from BODY.
             (match (assq 'connection (response-headers resp))
               (('connection 'close)
                (close-port p)
                (connect #f                       ;try again
                         (drop requests (+ 1 processed))
                         result))
               (_
                (loop tail (+ 1 processed) result)))))))))) ;keep going

(define (cleanup-bad-build-data conn)
  (exec-query
   conn
   "
DELETE FROM build_status
WHERE status IN ('started', 'scheduled')
  AND timestamp = '1970-01-01T00:00:00'
  AND EXISTS (
    SELECT 1
    FROM build_status AS other
    WHERE other.status = build_status.status
      AND build_status.build_id = other.build_id
      AND other.timestamp > '1970-01-01T00:00:01'
  )"))

(define verbose-output?
  (make-parameter #f))

(define* (query-build-servers conn build-server-ids revision-commits
                              outputs
                              #:key verbose?)
  (cleanup-bad-build-data conn)
  (parameterize
      ((verbose-output? verbose?))
    (while #t
      (let ((build-servers (select-build-servers conn)))
        (for-each
         (match-lambda
           ((id url lookup-all-derivations? lookup-builds?)
            (when (or (or (not build-servers)
                          (not build-server-ids))
                      (member id build-server-ids))
              (when lookup-builds?
                (simple-format #t "\nQuerying ~A\n" url)
                (catch #t
                  (lambda ()
                    (with-exception-handler
                        (lambda (exn)
                          (backtrace)
                          (raise-exception exn))
                      (lambda ()
                        (query-build-server conn
                                            id
                                            url
                                            revision-commits
                                            outputs))))
                  (lambda (key . args)
                    (simple-format
                     (current-error-port)
                     "exception in query-build-server: ~A ~A\n"
                     key args)))))))
         build-servers)))))

(define (query-build-server conn id url revision-commits outputs)
  (define (fetch-derivation-output-details-set-id output)
    (match (exec-query
            conn
            "
SELECT derivations_by_output_details_set.derivation_output_details_set_id
FROM derivations_by_output_details_set
INNER JOIN derivation_outputs
  ON derivation_outputs.derivation_id =
     derivations_by_output_details_set.derivation_id
INNER JOIN derivation_output_details
  ON derivation_outputs.derivation_output_details_id =
     derivation_output_details.id
WHERE derivation_output_details.path = $1"
            (list output))
      (((id))
       (string->number id))
      (() #f)))

  (simple-format #t "\nFetching pending builds\n")
  (process-pending-builds conn id url)
  (simple-format #t "\nFetching unseen derivations\n")
  (process-derivation-outputs
   conn id url
   (if outputs
       (fold (lambda (output result)
               (vhash-cons output
                           (fetch-derivation-output-details-set-id output)
                           result))
             vlist-null
             outputs)
       (select-derivation-outputs-with-no-known-build conn
                                                      id
                                                      revision-commits))))

(define (insert-build-statuses-from-data conn build-server-id build-id data)
  (define stop-statuses
    (lset-difference string=?
                     build-status-strings
                     '("scheduled" "started")))

  (let* ((status-string
          (assq-ref build-statuses
                    (or (assoc-ref data "buildstatus")
                        ;; status is for the /output/ requests
                        (assoc-ref data "status"))))
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
           (when (not (member status-string existing-status-entries))
             (list (if (< 0 stoptime)
                       timestamp
                       stoptime)
                   status-string)))))))

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
         (if (verbose-output?)
             (simple-format (current-error-port)
                            "debug: status: ~A\n"
                            (assq-ref build-statuses
                                      (or (assoc-ref data "buildstatus")
                                          ;; status is for the /output/ requests
                                          (assoc-ref data "status"))))
             (display "-"))))
      ;; Try not to make to many requests at once
      (usleep 200)))
   (select-pending-builds conn build-server-id)))

(define (process-derivation-outputs conn build-server-id url
                                    derivation-output-paths-and-details-sets-ids)
  (simple-format (current-error-port) "Fetching ~A derivation outputs\n"
                 (vlist-length derivation-output-paths-and-details-sets-ids))
  (fetch-builds-by-output
   url
   (vhash-fold (lambda (key value result)
                 (cons key result))
               '()
               derivation-output-paths-and-details-sets-ids)
   (lambda (data output)
     (if data
         (catch #t
           (lambda ()
             (when (assoc-ref data "build")
               ;; The build can be #f
               (let* ((derivation
                       (assoc-ref data "derivation"))
                      (build-id
                       (ensure-build-exists
                        conn
                        build-server-id
                        derivation
                        #f
                        #:derivation-output-details-set-id
                        (match
                            (vhash-assoc
                             output
                             derivation-output-paths-and-details-sets-ids)
                          ((key . value) value)
                          (#f #f)))))
                 (insert-build-statuses-from-data
                  conn
                  build-server-id
                  build-id
                  (assoc-ref data "build"))
                 (if (verbose-output?)
                     (simple-format #t "found build for: ~A (~A)\n"
                                    output derivation)
                     (display "-")))))
           (lambda (key . args)
             (simple-format
              (current-error-port)
              "\nerror processing data for output: ~A\n"
              output)
             (simple-format (current-error-port)
                            "~A\n"
                            data)
             (simple-format
              (current-error-port)
              "~A: ~A\n"
              key
              args)))
         (if (verbose-output?)
             (simple-format #t "no build found:  ~A\n" output)
             (display "."))))))

(define (process-derivations conn build-server-id url revision-commits)
  (define derivations
    (select-derivations-with-no-known-build conn
                                            build-server-id
                                            revision-commits))

  (simple-format (current-error-port) "Fetching ~A derivations\n"
                 (length derivations))
  (fetch-builds
   url
   (map second derivations)
   (lambda (data)
     (if data
         (let ((build-id
                (ensure-build-exists conn
                                     build-server-id
                                     (assoc-ref data "derivation")
                                     #f)))
           (insert-build-statuses-from-data
            conn
            build-server-id
            build-id
            data)
           (display "-"))
         (display ".")))))

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

  (when (verbose-output?)
    (simple-format (current-error-port)
                   "debug: fetching ~A\n"
                   build-url))
  (let-values
      (((response body)
        (http-request build-url)))

    (cond
     ((eq? (response-code response) 200)
      (json-string->scm
       (bytevector->string body "utf-8")))
     (else
      #f))))

(define (fetch-builds url derivation-file-names handler)
  (define (read-to-eof port)
    "Read from PORT until EOF is reached.  The data are discarded."
    (dump-port port (%make-void-port "w")))

  (http-multiple-get
   (string->uri url)
   (lambda (request response port result)
     (let* ((len (response-content-length response))
            (response-body
             (if len
                 (get-bytevector-n port len)
                 (read-to-eof port))))
       (handler
        (cond
         ((eq? (response-code response) 200)
          (json-string->scm
           (bytevector->string response-body
                               "utf-8")))
         (else
          #f))))
     '())
   '()
   (map (lambda (derivation-file-name)
          (build-request
           (string->uri
            (string-append url
                           "build"
                           (string-drop
                            derivation-file-name
                            (string-length "/gnu/store"))))
           #:method 'GET
           #:headers '((User-Agent . "Guix Data Service"))))
        derivation-file-names)
   #:batch-size 100))

(define (fetch-builds-by-output url derivation-outputs handler)
  (define (read-to-eof port)
    "Read from PORT until EOF is reached.  The data are discarded."
    (dump-port port (%make-void-port "w")))

  (http-multiple-get
   (string->uri url)
   (lambda (request response port result)
     (let* ((len (response-content-length response))
            (response-body
             (if len
                 (get-bytevector-n port len)
                 (read-to-eof port))))
       (handler
        (cond
         ((eq? (response-code response) 200)
          (json-string->scm
           (bytevector->string response-body
                               "utf-8")))
         (else
          #f))
        (string-append
         "/gnu/store"
         (string-drop
          (uri-path (request-uri request))
          (string-length "/output")))))
     '())
   '()
   (map (lambda (output-file-name)
          (build-request
           (string->uri
            (string-append url
                           "output"
                           (string-drop
                            output-file-name
                            (string-length "/gnu/store"))))
           #:method 'GET
           #:headers '((User-Agent . "Guix Data Service"))))
        derivation-outputs)
   #:batch-size 100))

(define (select-pending-builds conn build-server-id)
  (define query
    "
SELECT builds.id, derivations.file_name
FROM derivations
INNER JOIN builds
  ON derivations.file_name = builds.derivation_file_name
LEFT JOIN latest_build_status
  ON latest_build_status.build_id = builds.id
WHERE builds.build_server_id = $1 AND
      latest_build_status.status IN (
        'scheduled', 'started'
      )
ORDER BY latest_build_status.status DESC, -- 'started' first
         latest_build_status.timestamp ASC
LIMIT 10000")

  (map
   (match-lambda
     ((build-id derivation-file-name)
      (list (string->number build-id)
            derivation-file-name)))
   (exec-query conn query (list (number->string build-server-id)))))

(define (select-derivations-with-no-known-build conn
                                                build-server-id
                                                revision-commits)
  (define query
    ;; Only select derivations that are in the package_derivations table, as
    ;; Cuirass doesn't build the intermediate derivations
    (string-append
     "
SELECT derivations.id, derivations.file_name
FROM derivations
INNER JOIN derivations_by_output_details_set
  ON derivations.id = derivations_by_output_details_set.derivation_id
WHERE derivation_output_details_set_id NOT IN (
  SELECT derivation_output_details_set_id
  FROM builds
  WHERE build_server_id = $1
) AND derivation_output_details_set_id IN (
  SELECT derivation_output_details_set_id
  FROM package_derivations
  INNER JOIN derivations_by_output_details_set
    ON package_derivations.derivation_id =
       derivations_by_output_details_set.derivation_id
  INNER JOIN build_servers_build_config
    ON build_servers_build_config.build_server_id = $1
   AND build_servers_build_config.system = package_derivations.system
   AND build_servers_build_config.target = package_derivations.target
"
     (if (null? revision-commits)
         ""
         (string-append
          "
  INNER JOIN guix_revision_package_derivations
    ON package_derivations.id =
       guix_revision_package_derivations.package_derivation_id
  INNER JOIN guix_revisions
    ON guix_revisions.id = guix_revision_package_derivations.revision_id
  WHERE guix_revisions.commit IN ("
          (string-join (map quote-string revision-commits) ",")
          ")"))
     "
)
ORDER BY derivation_output_details_set_id, derivations.id
LIMIT 30000"))

  (exec-query conn query (list (number->string build-server-id))))


(define (select-derivation-outputs-with-no-known-build conn
                                                       build-server-id
                                                       revision-commits)
  (define query
    ;; Only select derivations that are in the package_derivations table, as
    ;; Cuirass doesn't build the intermediate derivations
    (string-append
     "
SELECT derivation_output_details.path, derivation_output_details_sets.id
FROM derivation_output_details
INNER JOIN derivation_output_details_sets
  ON derivation_output_details.id =
     derivation_output_details_sets.derivation_output_details_ids[1]
WHERE NOT EXISTS (
  SELECT 1
  FROM builds
  WHERE builds.derivation_output_details_set_id =
        derivation_output_details_sets.id
    AND build_server_id = $1
) AND derivation_output_details_sets.id IN (
  SELECT derivation_output_details_set_id
  FROM package_derivations
  INNER JOIN derivations_by_output_details_set
    ON package_derivations.derivation_id =
       derivations_by_output_details_set.derivation_id
  INNER JOIN build_servers_build_config
    ON build_servers_build_config.build_server_id = $1
   AND build_servers_build_config.system = package_derivations.system
   AND build_servers_build_config.target = package_derivations.target
"
     (if (null? revision-commits)
         ""
         (string-append
          "
  INNER JOIN guix_revision_package_derivations
    ON package_derivations.id =
       guix_revision_package_derivations.package_derivation_id
  INNER JOIN guix_revisions
    ON guix_revisions.id = guix_revision_package_derivations.revision_id
  WHERE guix_revisions.commit IN ("
          (string-join (map quote-string revision-commits) ",")
          ")"))
     "
  UNION ALL
  SELECT derivations_by_output_details_set.derivation_output_details_set_id
  FROM guix_revision_system_test_derivations
  INNER JOIN derivations_by_output_details_set
    ON guix_revision_system_test_derivations.derivation_id =
       derivations_by_output_details_set.derivation_id
  INNER JOIN guix_revisions
    ON guix_revisions.id = guix_revision_system_test_derivations.guix_revision_id
"
     (if (null? revision-commits)
         ""
         (string-append
          "
  WHERE guix_revisions.commit IN ("
         (string-join (map quote-string revision-commits) ",")
         ")"))
     "
  UNION ALL
  SELECT derivations_by_output_details_set.derivation_output_details_set_id
  FROM channel_instances
  INNER JOIN derivations_by_output_details_set
    ON channel_instances.derivation_id =
       derivations_by_output_details_set.derivation_id
  INNER JOIN guix_revisions
    ON guix_revisions.id = channel_instances.guix_revision_id
"
     (if (null? revision-commits)
         ""
         (string-append
          "
  WHERE guix_revisions.commit IN ("
         (string-join (map quote-string revision-commits) ",")
         ")"))
     "
)
ORDER BY derivation_output_details_sets.id DESC, derivation_output_details.id
LIMIT 30000"))

  (fold (lambda (row result)
          (match row
            ((path derivation-output-details-sets-id)
             (vhash-cons path
                         (string->number
                          derivation-output-details-sets-id)
                         result))))
        vlist-null
        (exec-query conn query (list (number->string build-server-id)))))
