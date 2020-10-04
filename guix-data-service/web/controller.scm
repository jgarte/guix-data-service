;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix-data-service web controller)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (system repl error-handling)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:use-module (squee)
  #:use-module (json)
  #:use-module (prometheus)
  #:use-module (guix-data-service utils)
  #:use-module (guix-data-service config)
  #:use-module (guix-data-service comparison)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service metrics)
  #:use-module (guix-data-service model git-branch)
  #:use-module (guix-data-service model git-repository)
  #:use-module (guix-data-service model guix-revision)
  #:use-module (guix-data-service model nar)
  #:use-module (guix-data-service model package)
  #:use-module (guix-data-service model package-derivation)
  #:use-module (guix-data-service model package-metadata)
  #:use-module (guix-data-service model derivation)
  #:use-module (guix-data-service model build-status)
  #:use-module (guix-data-service model build)
  #:use-module (guix-data-service model lint-checker)
  #:use-module (guix-data-service model lint-warning)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service jobs load-new-guix-revision)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web sxml)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service web build controller)
  #:use-module (guix-data-service web dumps controller)
  #:use-module (guix-data-service web revision controller)
  #:use-module (guix-data-service web nar controller)
  #:use-module (guix-data-service web jobs controller)
  #:use-module (guix-data-service web view html)
  #:use-module (guix-data-service web build-server controller)
  #:use-module (guix-data-service web compare controller)
  #:use-module (guix-data-service web revision controller)
  #:use-module (guix-data-service web repository controller)
  #:export (%show-error-details
            controller))

(define cache-control-default-max-age
  (* 60 60 24)) ; One day

(define http-headers-for-unchanging-content
  `((cache-control
     . (public
        (max-age . ,cache-control-default-max-age)))))

(define-syntax-rule (-> target functions ...)
  (fold (lambda (f val) (and=> val f))
        target
        (list functions ...)))

(define render-metrics
  (let* ((registry                  (make-metrics-registry
                                     #:namespace "guixdataservice"))

         (revisions-count-metric    (make-gauge-metric registry
                                                       "revision_count"))

         (load-new-guix-revision-job-count (make-gauge-metric
                                            registry
                                            "load_new_guix_revision_job_count"
                                            #:labels '(repository_label
                                                       completed)))

         (table-row-estimate-metric (make-gauge-metric registry
                                                       "table_row_estimate"
                                                       #:labels '(name)))
         (table-bytes-metric        (make-gauge-metric registry
                                                       "table_bytes"
                                                       #:labels '(name)))
         (table-index-bytes-metric  (make-gauge-metric registry
                                                       "table_index_bytes"
                                                       #:labels '(name)))
         (table-toast-bytes-metric  (make-gauge-metric registry
                                                       "table_toast_bytes"
                                                       #:labels '(name)))

         (pg-stat-fields '(seq-scan seq-tup-read idx-scan idx-tup-fetch
                           n-tup-ins n-tup-upd n-tup-del
                           n-tup-hot-upd n-live-tup n-dead-tup
                           n-mod-since-analyze last-vacuum
                           last-autovacuum last-analyze last-autoanalyze
                           vacuum-count autovacuum-count
                           analyze-count autoanalyze-count))

         (pg-stat-metrics (map (lambda (field)
                                 (cons
                                  field
                                  (make-gauge-metric
                                   registry
                                   (string-append "pg_stat_"
                                                  (string-replace-substring
                                                   (symbol->string field)
                                                   "-"
                                                   "_"))
                                   #:labels '(name))))
                               pg-stat-fields)))
    (lambda ()
      (letpar& ((metric-values
                 (with-thread-postgresql-connection
                  fetch-high-level-table-size-metrics))
                (guix-revisions-count
                 (with-thread-postgresql-connection
                  count-guix-revisions))
                (pg-stat-user-tables-metrics
                 (with-thread-postgresql-connection
                  fetch-pg-stat-user-tables-metrics))
                (load-new-guix-revision-job-metrics
                 (with-thread-postgresql-connection
                  select-load-new-guix-revision-job-metrics)))

        (for-each (match-lambda
                    ((name row-estimate table-bytes index-bytes toast-bytes)

                     (metric-set table-row-estimate-metric
                                 row-estimate
                                 #:label-values `((name . ,name)))
                     (metric-set table-bytes-metric
                                 table-bytes
                                 #:label-values `((name . ,name)))
                     (metric-set table-index-bytes-metric
                                 index-bytes
                                 #:label-values `((name . ,name)))
                     (metric-set table-toast-bytes-metric
                                 toast-bytes
                                 #:label-values `((name . ,name)))))
                  metric-values)

        (metric-set revisions-count-metric
                    guix-revisions-count)

        (map (lambda (field-values)
               (let ((name (assq-ref field-values 'name)))
                 (for-each
                  (match-lambda
                    (('name . _) #f)
                    ((field . value)
                     (let ((metric (or (assq-ref pg-stat-metrics field)
                                       (error field))))
                       (metric-set metric
                                   value
                                   #:label-values `((name . ,name))))))
                  field-values)))
             pg-stat-user-tables-metrics)

        (for-each (match-lambda
                    ((repository-label completed count)
                     (metric-set
                      load-new-guix-revision-job-count
                      count
                      #:label-values
                      `((repository_label . ,repository-label)
                        (completed        . ,(if completed "yes" "no"))))))
                  load-new-guix-revision-job-metrics)

        (list (build-response
               #:code 200
               #:headers '((content-type . (text/plain))))
              (lambda (port)
                (write-metrics registry port)))))))

(define (render-derivation derivation-file-name)
  (letpar& ((derivation
             (with-thread-postgresql-connection
              (lambda (conn)
                (select-derivation-by-file-name conn derivation-file-name)))))

    (if derivation
        (letpar& ((derivation-inputs
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-derivation-inputs-by-derivation-id
                       conn
                       (first derivation)))))
                  (derivation-outputs
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-derivation-outputs-by-derivation-id
                       conn
                       (first derivation)))))
                  (builds
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-builds-with-context-by-derivation-file-name
                       conn
                       (second derivation))))))
          (render-html
           #:sxml (view-derivation derivation
                                   derivation-inputs
                                   derivation-outputs
                                   builds)
           #:extra-headers http-headers-for-unchanging-content))

        (render-html
         #:sxml (general-not-found
                 "Derivation not found"
                 "No derivation found with this file name.")
         #:code 404))))

(define (render-json-derivation derivation-file-name)
  (let ((derivation
         (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (select-derivation-by-file-name conn
                                             derivation-file-name))))))
    (if derivation
        (letpar& ((derivation-inputs
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-derivation-inputs-by-derivation-id
                       conn
                       (first derivation)))))
                  (derivation-outputs
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-derivation-outputs-by-derivation-id
                       conn
                       (first derivation)))))
                  (derivation-sources
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-derivation-sources-by-derivation-id
                       conn
                       (first derivation))))))
          (render-json
           `((inputs . ,(list->vector
                                    (map
                                     (match-lambda
                                       ((filename outputs)
                                        `((filename . ,filename)
                                          (out_name
                                           . ,(list->vector
                                               (map
                                                (lambda (output)
                                                  (assoc-ref output "output_name"))
                                                (vector->list outputs)))))))
                                     derivation-inputs)))
             (outputs . ,(list->vector
                                     (map
                                      (match-lambda
                                        ((output-name path hash-algorithm hash recursive?)
                                         `((output-name . ,output-name)
                                           (path . ,path)
                                           (hash-algorithm . ,hash-algorithm)
                                           (recursive? . ,recursive?))))
                                      derivation-outputs)))
             (sources . ,(list->vector derivation-sources))
             ,@(match derivation
                 ((_ _ builder args env-var system)
                  `((system . ,system)
                    (builder . ,builder)
                    (arguments . ,(list->vector args))
                    (environment-variables
                     . ,(map (lambda (var)
                               (cons (assq-ref var 'key)
                                     (assq-ref var 'value)))
                             env-var))))))))
        (render-json '((error . "invalid path"))))))

(define (render-formatted-derivation derivation-file-name)
  (let ((derivation
         (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (select-derivation-by-file-name conn
                                             derivation-file-name))))))
    (if derivation
        (letpar& ((derivation-inputs
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-derivation-inputs-by-derivation-id
                       conn
                       (first derivation)))))
                  (derivation-outputs
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-derivation-outputs-by-derivation-id
                       conn
                       (first derivation)))))
                  (derivation-sources
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-derivation-sources-by-derivation-id
                       conn
                       (first derivation))))))
          (render-html
           #:sxml (view-formatted-derivation derivation
                                             derivation-inputs
                                             derivation-outputs
                                             derivation-sources)
           #:extra-headers http-headers-for-unchanging-content))

        (render-html
         #:sxml (general-not-found
                 "Derivation not found"
                 "No derivation found with this file name.")
         #:code 404))))

(define (render-narinfos filename)
  (let ((narinfos
         (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (select-nars-for-output
              conn
              (string-append "/gnu/store/" filename)))))))
    (if (null? narinfos)
        (render-html
         #:sxml (general-not-found
                 "No nars found"
                 "No nars found for this output name.")
         #:code 404)

        (render-html
         #:sxml (view-narinfos narinfos)))))

(define (render-store-item filename)
  (letpar& ((derivation
             (with-thread-postgresql-connection
              (lambda (conn)
                (select-derivation-by-output-filename conn filename)))))
    (match derivation
      (()
       (match (parallel-via-thread-pool-channel
               (with-thread-postgresql-connection
                (lambda (conn)
                  (select-derivation-source-file-by-store-path conn filename))))
         (()
          (render-html
           #:sxml (general-not-found
                   "Store item not found"
                   "No derivation found producing this output")
           #:code 404))
         ((id)
          (render-html
           #:sxml (view-derivation-source-file
                   filename
                   (parallel-via-thread-pool-channel
                    (with-thread-postgresql-connection
                     (lambda (conn)
                       (select-derivation-source-file-nar-details-by-file-name
                        conn
                        filename)))))
           #:extra-headers http-headers-for-unchanging-content))))
      (derivations
       (letpar& ((derivations-using-store-item-list
                  (with-thread-postgresql-connection
                   (lambda (conn)
                     (map (lambda (derivation)
                            (match derivation
                              ((file-name output-id rest ...)
                               (select-derivations-using-output
                                conn output-id))))
                          derivations))))
                 (nars
                  (with-thread-postgresql-connection
                   (lambda (conn)
                     (select-nars-for-output conn filename))))
                 (builds
                  (with-thread-postgresql-connection
                   (lambda (conn)
                     (select-builds-with-context-by-derivation-output
                      conn
                      filename)))))
         (render-html
          #:sxml (view-store-item filename
                                  derivations
                                  derivations-using-store-item-list
                                  nars
                                  builds)))))))

(define (render-json-store-item filename)
  (let ((derivation
         (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (select-derivation-by-output-filename conn filename))))))
    (match derivation
      (()
       (match (parallel-via-thread-pool-channel
               (with-thread-postgresql-connection
                (lambda (conn)
                  (select-derivation-source-file-by-store-path conn filename))))
         (()
          (render-json '((error . "store item not found"))))
         ((id)
          (render-json
           `((derivation-source-file
              . ,(list->vector
                  (map
                   (match-lambda
                     ((key . value)
                      `((,key . ,value))))
                   (parallel-via-thread-pool-channel
                    (with-thread-postgresql-connection
                     (lambda (conn)
                       (select-derivation-source-file-nar-details-by-file-name
                        conn
                        filename))))))))))))
      (derivations
       (letpar& ((nars
                  (with-thread-postgresql-connection
                   (lambda (conn)
                     (select-nars-for-output conn filename)))))
         (render-json
          `((nars . ,(list->vector
                      (map
                       (match-lambda
                         ((_ hash _ urls signatures)
                          `((hash . ,hash)
                            (urls
                             . ,(list->vector
                                 (map
                                  (lambda (url-data)
                                    `((size . ,(assoc-ref url-data "size"))
                                      (compression . ,(assoc-ref url-data "compression"))
                                      (url . ,(assoc-ref url-data "url"))))
                                  urls)))
                            (signatures
                             . ,(list->vector
                                 (map
                                  (lambda (signature)
                                    `((version . ,(assoc-ref signature "version"))
                                      (host-name . ,(assoc-ref signature "host_name"))))
                                  signatures))))))
                       nars)))
            (derivations
             . ,(list->vector
                 (map
                  (match-lambda
                    ((filename output-id)
                     `((filename . ,filename)
                       (derivations-using-store-item
                        . ,(list->vector
                            (map car
                                 (parallel-via-thread-pool-channel
                                  (with-thread-postgresql-connection
                                   (lambda (conn)
                                     (select-derivations-using-output
                                      conn output-id))))))))))
                derivations))))))))))

(define handle-static-assets
  (if assets-dir-in-store?
      (static-asset-from-store-renderer)
      render-static-asset))

(define %show-error-details
  (make-parameter #f))

(define* (controller request method-and-path-components
                     mime-types body
                     secret-key-base)
  (define (controller-thunk)
    (actual-controller request
                       method-and-path-components
                       mime-types
                       body
                       secret-key-base))

  (call-with-error-handling
   controller-thunk
   #:on-error 'backtrace
   #:post-error (lambda args
                  (render-html #:sxml (error-page
                                       (if (%show-error-details)
                                           args
                                           #f))
                               #:code 500))))

(define (actual-controller request
                           method-and-path-components
                           mime-types
                           body
                           secret-key-base)
  (define path
    (uri-path (request-uri request)))

  (define (delegate-to f)
    (or (f request
           method-and-path-components
           mime-types
           body)
        (render-html
         #:sxml (general-not-found
                 "Page not found"
                 "")
         #:code 404)))

  (define (delegate-to-with-secret-key-base f)
    (or (f request
           method-and-path-components
           mime-types
           body
           secret-key-base)
        (render-html
         #:sxml (general-not-found
                 "Page not found"
                 "")
         #:code 404)))

  (match method-and-path-components
    (('GET)
     (render-html
      #:sxml (index
              (parallel-via-thread-pool-channel
               (with-thread-postgresql-connection
                (lambda (conn)
                  (map
                   (lambda (git-repository-details)
                     (cons
                      git-repository-details
                      (all-branches-with-most-recent-commit
                       conn (first git-repository-details))))
                   (all-git-repositories conn))))))))
    (('GET "assets" rest ...)
     (or (handle-static-assets (string-join rest "/")
                               (request-headers request))
         (not-found (request-uri request))))
    (('GET "healthcheck")
     (let ((database-status
            (catch
              #t
              (lambda ()
                (with-postgresql-connection
                 "web healthcheck"
                 (lambda (conn)
                   (number? (count-guix-revisions conn)))))
              (lambda (key . args)
                #f))))
       (render-json
        `((status . ,(if database-status
                         "ok"
                         "not ok")))
        #:code (if (eq? database-status
                        #t)
                   200
                   500))))
    (('GET "README")
     (let ((filename (string-append (%config 'doc-dir) "/README.html")))
       (if (file-exists? filename)
           (render-html
            #:sxml (readme (call-with-input-file filename
                             get-string-all)))
           (render-html
            #:sxml (general-not-found
                    "README not found"
                    "The README.html file does not exist")
            #:code 404))))
    (('GET "builds")
     (delegate-to build-controller))
    (('GET "statistics")
     (letpar& ((guix-revisions-count
                (with-thread-postgresql-connection count-guix-revisions))
               (count-derivations
                (with-thread-postgresql-connection count-derivations)))

       (render-html
        #:sxml (view-statistics guix-revisions-count
                                count-derivations))))
    (('GET "metrics")
     (render-metrics))
    (('GET "revision" args ...)
     (delegate-to revision-controller))
    (('GET "repositories")
     (delegate-to repository-controller))
    (('GET "repository" _ ...)
     (delegate-to repository-controller))
    (('GET "gnu" "store" filename)
     ;; These routes are a little special, as the extensions aren't used for
     ;; content negotiation, so just use the path from the request
     (let ((path (uri-path (request-uri request))))
       (if (string-suffix? ".drv" path)
           (render-derivation path)
           (render-store-item path))))
    (('GET "gnu" "store" filename "formatted")
     (if (string-suffix? ".drv" filename)
         (render-formatted-derivation (string-append "/gnu/store/" filename))
         (render-html
          #:sxml (general-not-found
                  "Not a derivation"
                  "The formatted display is only for derivations, where the filename ends in .drv")
          #:code 404)))
    (('GET "gnu" "store" filename "plain")
     (if (string-suffix? ".drv" filename)
         (let ((raw-drv
                (parallel-via-thread-pool-channel
                 (with-thread-postgresql-connection
                  (lambda (conn)
                    (select-serialized-derivation-by-file-name
                     conn
                     (string-append "/gnu/store/" filename)))))))
           (if raw-drv
               (render-text raw-drv)
               (not-found (request-uri request))))
         (not-found (request-uri request))))
    (('GET "gnu" "store" filename "narinfos")
     (render-narinfos filename))
    (('GET "gnu" "store" filename "json")
     (if (string-suffix? ".drv" filename)
         (render-json-derivation (string-append "/gnu/store/" filename))
         (render-json-store-item (string-append "/gnu/store/" filename))))
    (('GET "build-servers")
     (delegate-to-with-secret-key-base build-server-controller))
    (('GET "dumps" _ ...)
     (delegate-to dumps-controller))
    (((or 'GET 'POST) "build-server" _ ...)
     (delegate-to-with-secret-key-base build-server-controller))
    (('GET "compare" _ ...)             (delegate-to compare-controller))
    (('GET "compare-by-datetime" _ ...) (delegate-to compare-controller))
    (('GET "jobs" _ ...)   (delegate-to jobs-controller))
    (('GET "job" job-id)   (delegate-to jobs-controller))
    (('GET _ ...) (delegate-to nar-controller))
    ((method path ...)
     (render-html
      #:sxml (general-not-found
              "Page not found"
              "")
      #:code 404))))
