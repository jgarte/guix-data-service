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
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
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
  #:use-module (guix-data-service config)
  #:use-module (guix-data-service comparison)
  #:use-module (guix-data-service database)
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

(define (render-derivation conn derivation-file-name)
  (let ((derivation (select-derivation-by-file-name conn
                                                    derivation-file-name)))
    (if derivation
        (let ((derivation-inputs (select-derivation-inputs-by-derivation-id
                                  conn
                                  (first derivation)))
              (derivation-outputs (select-derivation-outputs-by-derivation-id
                                   conn
                                   (first derivation)))
              (builds (select-builds-with-context-by-derivation-file-name
                       conn
                       (second derivation))))
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

(define (render-json-derivation conn derivation-file-name)
   (let ((derivation (select-derivation-by-file-name conn
                                                    derivation-file-name)))
     (if derivation
        (let ((derivation-inputs (select-derivation-inputs-by-derivation-id
                                  conn
                                  (first derivation)))
              (derivation-outputs (select-derivation-outputs-by-derivation-id
                                   conn
                                   (first derivation)))
              (derivation-sources (select-derivation-sources-by-derivation-id
                                   conn
                                   (first derivation))))
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

(define (render-formatted-derivation conn derivation-file-name)
  (let ((derivation (select-derivation-by-file-name conn
                                                    derivation-file-name)))
    (if derivation
        (let ((derivation-inputs (select-derivation-inputs-by-derivation-id
                                  conn
                                  (first derivation)))
              (derivation-outputs (select-derivation-outputs-by-derivation-id
                                   conn
                                   (first derivation)))
              (derivation-sources (select-derivation-sources-by-derivation-id
                                   conn
                                   (first derivation))))
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

(define (render-narinfos conn filename)
  (let ((narinfos (select-nars-for-output
                   conn
                   (string-append "/gnu/store/" filename))))
    (if (null? narinfos)
        (render-html
         #:sxml (general-not-found
                 "No nars found"
                 "No nars found for this output name.")
         #:code 404)

        (render-html
         #:sxml (view-narinfos narinfos)))))

(define (render-store-item conn filename)
  (let ((derivation (select-derivation-by-output-filename conn filename)))
    (match derivation
      (()
       (match (select-derivation-source-file-by-store-path conn filename)
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
                   (select-derivation-source-file-nar-details-by-file-name conn
                                                                           filename))
           #:extra-headers http-headers-for-unchanging-content))))
      (derivations
       (render-html
        #:sxml (view-store-item filename
                                derivations
                                (map (lambda (derivation)
                                       (match derivation
                                         ((file-name output-id rest ...)
                                          (select-derivations-using-output
                                           conn output-id))))
                                     derivations)
                                (select-nars-for-output conn
                                                        filename)
                                (select-builds-with-context-by-derivation-output
                                 conn filename)))))))

(define handle-static-assets
  (if assets-dir-in-store?
      (static-asset-from-store-renderer)
      render-static-asset))

(define %show-error-details
  (make-parameter #f))

(define* (controller request method-and-path-components
                     mime-types body
                     secret-key-base
                     #:key postgresql-statement-timeout)
  (define (controller-thunk)
    (match method-and-path-components
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
                     (number?
                      (string->number
                       (first
                        (count-guix-revisions conn)))))))
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
      (_
       (with-postgresql-connection
        "web"
        (lambda (conn)
          (controller-with-database-connection request
                                               method-and-path-components
                                               mime-types
                                               body
                                               conn
                                               secret-key-base))
        #:statement-timeout postgresql-statement-timeout))))
  (call-with-error-handling
   controller-thunk
   #:on-error 'backtrace
   #:post-error (lambda args
                  (render-html #:sxml (error-page
                                       (if (%show-error-details)
                                           args
                                           #f))
                               #:code 500))))

(define (controller-with-database-connection request
                                             method-and-path-components
                                             mime-types
                                             body
                                             conn
                                             secret-key-base)
  (define path
    (uri-path (request-uri request)))

  (define (delegate-to f)
    (or (f request
           method-and-path-components
           mime-types
           body
           conn)
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
           conn
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
              (map
               (lambda (git-repository-details)
                 (cons
                  git-repository-details
                  (all-branches-with-most-recent-commit
                   conn (first git-repository-details))))
               (all-git-repositories conn)))))
    (('GET "builds")
     (delegate-to build-controller))
    (('GET "statistics")
     (render-html
      #:sxml (view-statistics (count-guix-revisions conn)
                              (count-derivations conn))))
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
           (render-derivation conn path)
           (render-store-item conn path))))
    (('GET "gnu" "store" filename "formatted")
     (if (string-suffix? ".drv" filename)
         (render-formatted-derivation conn
                                      (string-append "/gnu/store/" filename))
         (render-html
          #:sxml (general-not-found
                  "Not a derivation"
                  "The formatted display is only for derivations, where the filename ends in .drv")
          #:code 404)))
    (('GET "gnu" "store" filename "plain")
     (if (string-suffix? ".drv" filename)
         (let ((raw-drv
                (select-serialized-derivation-by-file-name
                 conn
                 (string-append "/gnu/store/" filename))))
           (if raw-drv
               (render-text raw-drv)
               (not-found (request-uri request))))
         (not-found (request-uri request))))
    (('GET "gnu" "store" filename "narinfos")
     (render-narinfos conn filename))
    (('GET "gnu" "store" filename "json")
     (if (string-suffix? ".drv" filename)
         (render-json-derivation conn
                                 (string-append "/gnu/store/" filename))
         '()))
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
