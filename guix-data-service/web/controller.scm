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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (web request)
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
  #:use-module (guix-data-service web revision controller)
  #:use-module (guix-data-service web jobs controller)
  #:use-module (guix-data-service web view html)
  #:use-module (guix-data-service web revision controller)
  #:export (controller))

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

(define (render-with-error-handling page message)
  (apply render-html (page))
  ;; (catch #t
  ;;   (lambda ()
  ;;     (receive (sxml headers)
  ;;         (pretty-print (page))
  ;;       (render-html sxml headers)))
  ;;   (lambda (key . args)
  ;;     (format #t "ERROR: ~a ~a\n"
  ;;             key args)
  ;;     (render-html (error-page message))))
  )

(define (assoc-ref-multiple alist key)
  (filter-map
   (match-lambda
     ((k . value)
      (and (string=? k key)
           value)))
   alist))

(define (render-compare mime-types
                        conn
                        query-parameters)
  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          '((error . "invalid query"))))
        (else
         (render-html
          #:sxml (compare-invalid-parameters
                  query-parameters
                  (match (assq-ref query-parameters 'base_commit)
                    (($ <invalid-query-parameter> value)
                     (select-job-for-commit conn value))
                    (_ #f))
                  (match (assq-ref query-parameters 'target_commit)
                    (($ <invalid-query-parameter> value)
                     (select-job-for-commit conn value))
                    (_ #f))))))

      (let ((base-revision-id (commit->revision-id
                               conn
                               (assq-ref query-parameters 'base_commit)))
            (target-revision-id (commit->revision-id
                                 conn
                                 (assq-ref query-parameters 'target_commit))))
        (let-values
            (((base-packages-vhash target-packages-vhash)
              (package-data->package-data-vhashes
               (package-differences-data conn
                                         base-revision-id
                                         target-revision-id))))
          (let* ((new-packages
                  (package-data-vhashes->new-packages base-packages-vhash
                                                      target-packages-vhash))
                 (removed-packages
                  (package-data-vhashes->removed-packages base-packages-vhash
                                                          target-packages-vhash))
                 (version-changes
                  (package-data-version-changes base-packages-vhash
                                                target-packages-vhash))
                 (lint-warnings-data
                  (group-list-by-first-n-fields
                   2
                   (lint-warning-differences-data conn
                                                  base-revision-id
                                                  target-revision-id))))
            (case (most-appropriate-mime-type
                   '(application/json text/html)
                   mime-types)
              ((application/json)
               (render-json
                `((new-packages . ,(list->vector new-packages))
                  (removed-packages . ,(list->vector removed-packages))
                  (version-changes . ,(list->vector
                                       (map
                                        (match-lambda
                                          ((name data ...)
                                           `((name . ,name)
                                             ,@data)))
                                        version-changes))))
                #:extra-headers http-headers-for-unchanging-content))
              (else
               (render-html
                #:sxml (compare query-parameters
                                (guix-revisions-cgit-url-bases
                                 conn
                                 (list base-revision-id
                                       target-revision-id))
                                new-packages
                                removed-packages
                                version-changes
                                lint-warnings-data)
                #:extra-headers http-headers-for-unchanging-content))))))))

(define (render-compare-by-datetime mime-types
                                    conn
                                    query-parameters)
  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          '((error . "invalid query"))))
        (else
         (render-html
          #:sxml (compare-invalid-parameters
                  query-parameters
                  (match (assq-ref query-parameters 'base_commit)
                    (($ <invalid-query-parameter> value)
                     (select-job-for-commit conn value))
                    (_ #f))
                  (match (assq-ref query-parameters 'target_commit)
                    (($ <invalid-query-parameter> value)
                     (select-job-for-commit conn value))
                    (_ #f))))))

      (let ((base-branch     (assq-ref query-parameters 'base_branch))
            (base-datetime   (assq-ref query-parameters 'base_datetime))
            (target-branch   (assq-ref query-parameters 'target_branch))
            (target-datetime (assq-ref query-parameters 'target_datetime)))
        (let* ((base-revision-details
                (select-guix-revision-for-branch-and-datetime conn
                                                              base-branch
                                                              base-datetime))
               (base-revision-id
                (first base-revision-details))
               (target-revision-details
                (select-guix-revision-for-branch-and-datetime conn
                                                              target-branch
                                                              target-datetime))
               (target-revision-id
                (first target-revision-details)))
          (let-values
              (((base-packages-vhash target-packages-vhash)
                (package-data->package-data-vhashes
                 (package-differences-data conn
                                           base-revision-id
                                           target-revision-id))))
            (let* ((new-packages
                    (package-data-vhashes->new-packages base-packages-vhash
                                                        target-packages-vhash))
                   (removed-packages
                    (package-data-vhashes->removed-packages base-packages-vhash
                                                            target-packages-vhash))
                   (version-changes
                    (package-data-version-changes base-packages-vhash
                                                  target-packages-vhash))
                   (lint-warnings-data
                    (group-list-by-first-n-fields
                     2
                     (lint-warning-differences-data conn
                                                    base-revision-id
                                                    target-revision-id))))
              (case (most-appropriate-mime-type
                     '(application/json text/html)
                     mime-types)
                ((application/json)
                 (render-json
                  `((new-packages . ,(list->vector new-packages))
                    (removed-packages . ,(list->vector removed-packages))
                    (version-changes . ,(list->vector
                                         (map
                                          (match-lambda
                                            ((name data ...)
                                             `((name . ,name)
                                               ,@data)))
                                          version-changes))))
                  #:extra-headers http-headers-for-unchanging-content))
                (else
                 (render-html
                  #:sxml (compare `(,@query-parameters
                                    (base_commit . ,(second base-revision-details))
                                    (target_commit . ,(second target-revision-details)))
                                  (guix-revisions-cgit-url-bases
                                   conn
                                   (list base-revision-id
                                         target-revision-id))
                                  new-packages
                                  removed-packages
                                  version-changes
                                  lint-warnings-data)
                  #:extra-headers http-headers-for-unchanging-content)))))))))

(define (render-compare/derivations mime-types
                                    conn
                                    query-parameters)
  (define (derivations->alist derivations)
    (map (match-lambda
           ((file-name system target buildstatus)
            `((file_name . ,file-name)
              (system . ,system)
              (target . ,target)
              (build_status . ,(if (string=? buildstatus "")
                                   "unknown"
                                   buildstatus)))))
         derivations))

  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          '((error . "invalid query"))))
        (else
         (render-html
          #:sxml (compare/derivations
                  query-parameters
                  (valid-systems conn)
                  build-status-strings
                  '()))))

      (let ((base-commit    (assq-ref query-parameters 'base_commit))
            (target-commit  (assq-ref query-parameters 'target_commit))
            (systems        (assq-ref query-parameters 'system))
            (targets        (assq-ref query-parameters 'target))
            (build-statuses (assq-ref query-parameters 'build_status)))
        (let*
            ((data
              (package-differences-data conn
                                        (commit->revision-id conn base-commit)
                                        (commit->revision-id conn target-commit)
                                        #:systems systems
                                        #:targets targets))
             (names-and-versions
              (package-data->names-and-versions data)))
          (let-values
              (((base-packages-vhash target-packages-vhash)
                (package-data->package-data-vhashes data)))
            (let ((derivation-changes
                   (package-data-derivation-changes names-and-versions
                                                    base-packages-vhash
                                                    target-packages-vhash)))
              (case (most-appropriate-mime-type
                     '(application/json text/html)
                     mime-types)
                ((application/json)
                 (render-json
                  derivation-changes
                  #:extra-headers http-headers-for-unchanging-content))
                (else
                 (render-html
                  #:sxml (compare/derivations
                          query-parameters
                          (valid-systems conn)
                          build-status-strings
                          derivation-changes)
                  #:extra-headers http-headers-for-unchanging-content)))))))))

(define (render-compare-by-datetime/derivations mime-types
                                                conn
                                                query-parameters)
  (define (derivations->alist derivations)
    (map (match-lambda
           ((file-name system target buildstatus)
            `((file_name . ,file-name)
              (system . ,system)
              (target . ,target)
              (build_status . ,(if (string=? buildstatus "")
                                   "unknown"
                                   buildstatus)))))
         derivations))

  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          '((error . "invalid query"))))
        (else
         (render-html
          #:sxml (compare-by-datetime/derivations
                  query-parameters
                  (valid-systems conn)
                  build-status-strings
                  '()))))

      (let ((base-branch     (assq-ref query-parameters 'base_branch))
            (base-datetime   (assq-ref query-parameters 'base_datetime))
            (target-branch   (assq-ref query-parameters 'target_branch))
            (target-datetime (assq-ref query-parameters 'target_datetime))
            (systems         (assq-ref query-parameters 'system))
            (targets         (assq-ref query-parameters 'target))
            (build-statuses  (assq-ref query-parameters 'build_status)))
        (let*
            ((base-revision-details
              (select-guix-revision-for-branch-and-datetime conn
                                                            base-branch
                                                            base-datetime))
             (target-revision-details
              (select-guix-revision-for-branch-and-datetime conn
                                                            target-branch
                                                            target-datetime))
             (data
              (package-differences-data conn
                                        (first base-revision-details)
                                        (first target-revision-details)
                                        #:systems systems
                                        #:targets targets))
             (names-and-versions
              (package-data->names-and-versions data)))
          (let-values
              (((base-packages-vhash target-packages-vhash)
                (package-data->package-data-vhashes data)))
            (let ((derivation-changes
                   (package-data-derivation-changes names-and-versions
                                                    base-packages-vhash
                                                    target-packages-vhash)))
              (case (most-appropriate-mime-type
                     '(application/json text/html)
                     mime-types)
                ((application/json)
                 (render-json
                  derivation-changes
                  #:extra-headers http-headers-for-unchanging-content))
                (else
                 (render-html
                  #:sxml (compare-by-datetime/derivations
                          query-parameters
                          (valid-systems conn)
                          build-status-strings
                          base-revision-details
                          target-revision-details
                          derivation-changes)
                  #:extra-headers http-headers-for-unchanging-content)))))))))

(define (render-compare/packages mime-types
                                 conn
                                 query-parameters)
  (define (package-data-vhash->json vh)
    (delete-duplicates
     (vhash-fold (lambda (name data result)
                   (cons `((name . ,name)
                           (version . ,(car data)))
                         result))
                 '()
                 vh)))

  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          '((error . "invalid query"))))
        (else
         (render-html
          #:sxml (compare-invalid-parameters
                  query-parameters
                  (match (assq-ref query-parameters 'base_commit)
                    (($ <invalid-query-parameter> value)
                     (select-job-for-commit conn value))
                    (_ #f))
                  (match (assq-ref query-parameters 'target_commit)
                    (($ <invalid-query-parameter> value)
                     (select-job-for-commit conn value))
                    (_ #f))))))

      (let ((base-commit    (assq-ref query-parameters 'base_commit))
            (target-commit  (assq-ref query-parameters 'target_commit)))
        (let ((base-revision-id (commit->revision-id conn base-commit))
              (target-revision-id (commit->revision-id conn target-commit)))

          (let-values
              (((base-packages-vhash target-packages-vhash)
                (package-data->package-data-vhashes
                 (package-differences-data conn
                                           base-revision-id
                                           target-revision-id))))
            (case (most-appropriate-mime-type
                   '(application/json text/html)
                   mime-types)
              ((application/json)
               (render-json
                `((base
                   . ((commit . ,base-commit)
                      (packages . ,(list->vector
                                    (package-data-vhash->json base-packages-vhash)))))
                  (target
                   . ((commit . ,target-commit)
                      (packages . ,(list->vector
                                    (package-data-vhash->json target-packages-vhash))))))
                #:extra-headers http-headers-for-unchanging-content))
              (else
               (render-html
                #:sxml (compare/packages
                        query-parameters
                        base-packages-vhash
                        target-packages-vhash)
                #:extra-headers http-headers-for-unchanging-content))))))))

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
              (builds (select-builds-with-context-by-derivation-id
                       conn
                       (first derivation))))
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

(define (render-store-item conn filename)
  (let ((derivation (select-derivation-by-output-filename conn filename)))
    (match derivation
      (()
       (render-html
        #:sxml (general-not-found
                "Store item not found"
                "No derivation found producing this output")
        #:code 404))
      (derivations
       (render-html
        #:sxml (view-store-item filename
                                derivations
                                (map (lambda (derivation)
                                       (match derivation
                                         ((file-name output-id rest ...)
                                          (select-derivations-using-output
                                           conn output-id))))
                                     derivations))
        #:extra-headers http-headers-for-unchanging-content)))))

(define (parse-commit conn)
  (lambda (s)
    (if (guix-commit-exists? conn s)
        s
        (make-invalid-query-parameter
         s "unknown commit"))))

(define (parse-system s)
  s)

(define (parse-build-status s)
  s)

(define handle-static-assets
  (if assets-dir-in-store?
      (static-asset-from-store-renderer)
      render-static-asset))

(define (controller request method-and-path-components mime-types body)
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
                                             conn))))))

(define (controller-with-database-connection request
                                             method-and-path-components
                                             mime-types
                                             body
                                             conn)
  (define path
    (uri-path (request-uri request)))

  (define (delegate-to f)
    (f request
       method-and-path-components
       mime-types
       body
       conn))

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
     (render-html
      #:sxml (view-builds (select-build-stats conn)
                          (select-builds-with-context conn))))
    (('GET "statistics")
     (render-html
      #:sxml (view-statistics (count-guix-revisions conn)
                              (count-derivations conn))))
    (('GET "revision" args ...)
     (delegate-to revision-controller))
    (('GET "repository" id)
     (match (select-git-repository conn id)
       ((label url cgit-url-base)
        (render-html
         #:sxml
         (view-git-repository
          (string->number id)
          label url cgit-url-base
          (all-branches-with-most-recent-commit conn
                                                (string->number id)))))
       (#f
        (render-html
         #:sxml (general-not-found
                 "Repository not found"
                 "")
         #:code 404))))
    (('GET "repository" repository-id "branch" branch-name)
     (let ((parsed-query-parameters
            (parse-query-parameters
             request
             `((after_date     ,parse-datetime)
               (before_date    ,parse-datetime)
               (limit_results  ,parse-result-limit #:default 100)))))
       (render-html
        #:sxml (if (any-invalid-query-parameters? parsed-query-parameters)
                   (view-branch repository-id
                                branch-name parsed-query-parameters '())
                   (view-branch
                    repository-id
                    branch-name
                    parsed-query-parameters
                    (most-recent-commits-for-branch
                     conn
                     (string->number repository-id)
                     branch-name
                     #:limit (assq-ref parsed-query-parameters 'limit_results)
                     #:after-date (assq-ref parsed-query-parameters
                                            'after_date)
                     #:before-date (assq-ref parsed-query-parameters
                                             'before_date)))))))
    (('GET "repository" repository-id "branch" branch-name "package" package-name)
     (let ((package-versions
            (package-versions-for-branch conn
                                         (string->number repository-id)
                                         branch-name
                                         package-name)))
       (case (most-appropriate-mime-type
              '(application/json text/html)
              mime-types)
         ((application/json)
          (render-json
           `((versions . ,(list->vector
                           (map (match-lambda
                                  ((package-version first-guix-revision-commit
                                                    first-datetime
                                                    last-guix-revision-commit
                                                    last-datetime)
                                   `((version . ,package-version)
                                     (first_revision
                                      . ((commit . ,first-guix-revision-commit)
                                         (datetime . ,first-datetime)))
                                     (last_revision
                                      . ((commit . ,last-guix-revision-commit)
                                         (datetime . ,last-datetime))))))
                                package-versions))))))
         (else
          (render-html
           #:sxml (view-branch-package
                   repository-id
                   branch-name
                   package-name
                   package-versions))))))
    (('GET "repository" repository-id "branch" branch-name "latest-processed-revision")
     (let ((commit-hash
            (latest-processed-commit-for-branch conn repository-id branch-name)))
       (if commit-hash
           (render-view-revision mime-types
                                 conn
                                 commit-hash
                                 #:path-base path
                                 #:header-text
                                 `("Latest processed revision for branch "
                                   (samp ,branch-name)))
           (render-unknown-revision mime-types
                                    conn
                                    commit-hash))))
    (('GET "repository" repository-id "branch" branch-name "latest-processed-revision" "packages")
     (let ((commit-hash
            (latest-processed-commit-for-branch conn repository-id branch-name)))
       (if commit-hash
           (let ((parsed-query-parameters
                  (guard-against-mutually-exclusive-query-parameters
                   (parse-query-parameters
                    request
                    `((after_name     ,identity)
                      (field          ,identity #:multi-value
                                      #:default ("version" "synopsis"))
                      (search_query   ,identity)
                      (limit_results  ,parse-result-limit
                                      #:no-default-when (all_results)
                                      #:default 100)
                      (all_results    ,parse-checkbox-value)))
                   ;; You can't specify a search query, but then also limit the
                   ;; results by filtering for after a particular package name
                   '((after_name search_query)
                     (limit_results all_results)))))

             (render-revision-packages mime-types
                                       conn
                                       commit-hash
                                       parsed-query-parameters
                                       #:path-base path
                                       #:header-text
                                       `("Latest processed revision for branch "
                                         (samp ,branch-name))
                                       #:header-link
                                       (string-append
                                        "/repository/" repository-id
                                        "/branch/" branch-name
                                        "/latest-processed-revision")))
           (render-unknown-revision mime-types
                                    conn
                                    commit-hash))))
    (('GET "repository" repository-id "branch" branch-name "latest-processed-revision"
           "lint-warnings")
     (let ((commit-hash
            (latest-processed-commit-for-branch conn repository-id branch-name)))
       (if commit-hash
           (let ((parsed-query-parameters
                  (parse-query-parameters
                   request
                   `((package_query  ,identity)
                     (linter         ,identity #:multi-value)
                     (message_query  ,identity)
                     (field          ,identity #:multi-value
                                     #:default ("linter"
                                                "message"
                                                "location"))))))

             (render-revision-lint-warnings mime-types
                                            conn
                                            commit-hash
                                            parsed-query-parameters
                                            #:path-base path
                                            #:header-text
                                            `("Latest processed revision for branch "
                                              (samp ,branch-name))
                                            #:header-link
                                            (string-append
                                             "/repository/" repository-id
                                             "/branch/" branch-name
                                             "/latest-processed-revision")))
           (render-unknown-revision mime-types
                                    conn
                                    commit-hash))))
    (('GET "repository" repository-id "branch" branch-name "latest-processed-revision" "package" name version)
     (let ((commit-hash
            (latest-processed-commit-for-branch conn repository-id branch-name)))
       (if commit-hash
           (render-revision-package-version mime-types
                                            conn
                                            commit-hash
                                            name
                                            version
                                            #:header-text
                                            `("Latest processed revision for branch "
                                              (samp ,branch-name))
                                            #:header-link
                                            (string-append
                                             "/repository/" repository-id
                                             "/branch/" branch-name
                                             "/latest-processed-revision"))
           (render-unknown-revision mime-types
                                    conn
                                    commit-hash))))
    (('GET "gnu" "store" filename)
     ;; These routes are a little special, as the extensions aren't used for
     ;; content negotiation, so just use the path from the request
     (let ((path (uri-path (request-uri request))))
       (if (string-suffix? ".drv" path)
           (render-derivation conn path)
           (render-store-item conn path))))
    (('GET "compare")
     (let* ((parsed-query-parameters
             (parse-query-parameters
              request
              `((base_commit   ,(parse-commit conn) #:required)
                (target_commit ,(parse-commit conn) #:required)))))
       (render-compare mime-types
                       conn
                       parsed-query-parameters)))
    (('GET "compare-by-datetime")
     (let* ((parsed-query-parameters
             (parse-query-parameters
              request
              `((base_branch     ,identity #:required)
                (base_datetime   ,parse-datetime #:required)
                (target_branch   ,identity #:required)
                (target_datetime ,parse-datetime #:required)))))
       (render-compare-by-datetime mime-types
                                   conn
                                   parsed-query-parameters)))
    (('GET "compare" "derivations")
     (let* ((parsed-query-parameters
             (parse-query-parameters
              request
              `((base_commit   ,(parse-commit conn) #:required)
                (target_commit ,(parse-commit conn) #:required)
                (system        ,parse-system #:multi-value)
                (target        ,parse-system #:multi-value)
                (build_status  ,parse-build-status #:multi-value)))))
       (render-compare/derivations mime-types
                                   conn
                                   parsed-query-parameters)))
    (('GET "compare-by-datetime" "derivations")
     (let* ((parsed-query-parameters
             (guard-against-mutually-exclusive-query-parameters
              (parse-query-parameters
               request
               `((base_branch     ,identity #:required)
                 (base_datetime   ,parse-datetime #:required)
                 (target_branch   ,identity #:required)
                 (target_datetime ,parse-datetime #:required)
                 (system          ,parse-system #:multi-value)
                 (target          ,parse-system #:multi-value)
                 (build_status    ,parse-build-status #:multi-value)))
              '((base_commit base_datetime)
                (target_commit target_datetime)))))
       (render-compare-by-datetime/derivations mime-types
                                               conn
                                               parsed-query-parameters)))
    (('GET "compare" "packages")
     (let* ((parsed-query-parameters
             (parse-query-parameters
              request
              `((base_commit   ,(parse-commit conn) #:required)
                (target_commit ,(parse-commit conn) #:required)))))
       (render-compare/packages mime-types
                                conn
                                parsed-query-parameters)))
    (('GET "jobs")         (delegate-to jobs-controller))
    (('GET "jobs" "queue") (delegate-to jobs-controller))
    (('GET "job" job-id)   (delegate-to jobs-controller))
    (('GET path ...)
     (not-found (request-uri request)))))
