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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:use-module (texinfo plain-text)
  #:use-module (squee)
  #:use-module (json)
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
  #:use-module (guix-data-service jobs load-new-guix-revision)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web sxml)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service web view html)
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

(define (with-base-and-target-commits query-parameters conn f)
  (let* ((base-commit (assoc-ref query-parameters "base_commit"))
         (target-commit (assoc-ref query-parameters "target_commit")))

    (f base-commit
       (commit->revision-id conn base-commit)
       target-commit
       (commit->revision-id conn target-commit))))

(define* (render-view-revision mime-types
                               conn
                               commit-hash
                               #:key path-base
                               (header-text
                                `("Revision " (samp ,commit-hash))))
  (let ((packages-count
         (count-packages-in-revision conn commit-hash))
        (git-repositories-and-branches
         (git-branches-with-repository-details-for-commit conn commit-hash))
        (derivations-counts
         (count-packages-derivations-in-revision conn commit-hash))
        (jobs-and-events
         (select-jobs-and-events-for-commit conn commit-hash)))
    (case (most-appropriate-mime-type
           '(application/json text/html)
           mime-types)
      ((application/json)
       (render-json
        `((packages_count  . ,(caar packages-count))
          (derivations_counts . ,(list->vector
                                  (map (match-lambda
                                         ((system target derivation_count)
                                          `((system . ,system)
                                            (target . ,target)
                                            (derivation_count . ,derivation_count))))
                                       derivations-counts))))
        #:extra-headers http-headers-for-unchanging-content))
      (else
       (render-html
        #:sxml (view-revision
                commit-hash
                packages-count
                git-repositories-and-branches
                derivations-counts
                jobs-and-events
                #:path-base path-base
                #:header-text header-text)
        #:extra-headers http-headers-for-unchanging-content)))))

(define (texinfo->variants-alist s)
  (let ((stexi (texi-fragment->stexi s)))
    `((source . ,s)
      (html   . ,(with-output-to-string
                   (lambda ()
                     (sxml->html (stexi->shtml stexi)))))
      (plain . ,(stexi->plain-text stexi)))))

(define (render-unknown-revision mime-types conn commit-hash)
  (case (most-appropriate-mime-type
         '(application/json text/html)
         mime-types)
    ((application/json)
     (render-json
      '((unknown_commit . ,commit-hash))
      #:code 404))
    (else
     (render-html
      #:code 404
      #:sxml (unknown-revision
              commit-hash
              (select-job-for-commit
               conn commit-hash)
              (git-branches-with-repository-details-for-commit conn commit-hash)
              (select-jobs-and-events-for-commit conn commit-hash))))))


(define* (render-revision-packages mime-types
                                   conn
                                   commit-hash
                                   query-parameters
                                   #:key
                                   (path-base "/revision/")
                                   (header-text
                                    `("Revision " (samp ,commit-hash)))
                                   (header-link
                                    (string-append "/revision/" commit-hash)))
  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          `((error . "invalid query"))))
        (else
         (render-html
          #:sxml (view-revision-packages commit-hash
                                         query-parameters
                                         '()
                                         '()
                                         #f
                                         #:path-base path-base
                                         #:header-text header-text
                                         #:header-link header-link))))

      (let* ((search-query (assq-ref query-parameters 'search_query))
             (limit-results (or (assq-ref query-parameters 'limit_results)
                                99999)) ; TODO There shouldn't be a limit
             (fields (assq-ref query-parameters 'field))
             (packages
              (if search-query
                  (search-packages-in-revision
                   conn
                   commit-hash
                   search-query
                   #:limit-results limit-results)
                  (select-packages-in-revision
                   conn
                   commit-hash
                   #:limit-results limit-results
                   #:after-name (assq-ref query-parameters 'after_name))))
             (git-repositories
              (git-repositories-containing-commit conn
                                                  commit-hash))
             (show-next-page?
              (and (not search-query)
                   (>= (length packages)
                       limit-results))))
        (case (most-appropriate-mime-type
               '(application/json text/html)
               mime-types)
          ((application/json)
           (render-json
            `((revision
               . ((commit . ,commit-hash)))
              (packages
               . ,(list->vector
                   (map (match-lambda
                          ((name version synopsis description home-page
                                 location-file location-line
                                 location-column-number licenses)
                           `((name . ,name)
                             ,@(if (member "version" fields)
                                   `((version . ,version))
                                   '())
                             ,@(if (member "synopsis" fields)
                                   `((synopsis
                                      . ,(texinfo->variants-alist synopsis)))
                                   '())
                             ,@(if (member "description" fields)
                                   `((description
                                      . ,(texinfo->variants-alist description)))
                                   '())
                             ,@(if (member "home-page" fields)
                                   `((home-page . ,home-page))
                                   '())
                             ,@(if (member "location" fields)
                                   `((location
                                      . ((file   . ,location-file)
                                         (line   . ,location-line)
                                         (column . ,location-column-number))))
                                   '())
                             ,@(if (member "licenses" fields)
                                   `((licenses
                                      . ,(if (string-null? licenses)
                                             #()
                                             (json-string->scm licenses))))
                                   '()))))
                        packages))))
            #:extra-headers http-headers-for-unchanging-content))
          (else
           (render-html
            #:sxml (view-revision-packages commit-hash
                                           query-parameters
                                           packages
                                           git-repositories
                                           show-next-page?
                                           #:path-base path-base
                                           #:header-text header-text
                                           #:header-link header-link)
            #:extra-headers http-headers-for-unchanging-content))))))

(define* (render-revision-package mime-types
                                  conn
                                  commit-hash
                                  name
                                  version
                                  #:key
                                  (header-text
                                   `("Revision "
                                     (samp ,commit-hash)))
                                  (header-link
                                   (string-append
                                    "/revision/" commit-hash)))
  (let ((metadata
         (select-package-metadata-by-revision-name-and-version
          conn
          commit-hash
          name
          version))
        (derivations
         (select-derivations-by-revision-name-and-version
          conn
          commit-hash
          name
          version))
        (git-repositories
         (git-repositories-containing-commit conn
                                             commit-hash)))
    (case (most-appropriate-mime-type
           '(application/json text/html)
           mime-types)
      ((application/json)
       (render-json
        `((name . ,name)
          (version . ,version)
          ,@(match metadata
              (((synopsis description home-page))
               `((synopsis . ,synopsis)
                 (description . ,description)
                 (home-page . ,home-page))))
          (derivations . ,(list->vector
                           (map (match-lambda
                                  ((system target file-name status)
                                   `((system . ,system)
                                     (target . ,target)
                                     (derivation . ,file-name))))
                                derivations))))
        #:extra-headers http-headers-for-unchanging-content))
      (else
       (render-html
        #:sxml (view-revision-package-and-version commit-hash
                                                  name
                                                  version
                                                  metadata
                                                  derivations
                                                  git-repositories
                                                  #:header-text header-text
                                                  #:header-link header-link)
        #:extra-headers http-headers-for-unchanging-content)))))

(define (render-compare-unknown-commit mime-types
                                       conn
                                       base-commit
                                       base-revision-id
                                       target-commit
                                       target-revision-id)
  (case (most-appropriate-mime-type
         '(application/json text/html)
         mime-types)
    ((application/json)
     (render-json
      '((unknown_commit . #t))))
    (else
     (render-html
      #:sxml (compare-unknown-commit base-commit
                                     target-commit
                                     (if base-revision-id #t #f)
                                     (if target-revision-id #t #f)
                                     (select-job-for-commit conn
                                                            base-commit)
                                     (select-job-for-commit conn
                                                            target-commit))))))

(define (render-compare mime-types
                        conn
                        base-commit
                        base-revision-id
                        target-commit
                        target-revision-id)
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
                                          target-packages-vhash)))
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
          #:sxml (compare base-commit
                          target-commit
                          (guix-revisions-cgit-url-bases
                           conn
                           (list base-revision-id
                                 target-revision-id))
                          new-packages
                          removed-packages
                          version-changes)
          #:extra-headers http-headers-for-unchanging-content))))))

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
        (let-values
            (((base-packages-vhash target-packages-vhash)
              (package-data->package-data-vhashes
               (package-differences-data conn
                                         (commit->revision-id conn base-commit)
                                         (commit->revision-id conn target-commit)))))
          (let ((derivation-changes
                 (package-data-derivation-changes base-packages-vhash
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
                #:extra-headers http-headers-for-unchanging-content))))))))

(define (render-compare/packages mime-types
                                 conn
                                 base-commit
                                 base-revision-id
                                 target-commit
                                 target-revision-id)
  (define (package-data-vhash->json vh)
    (delete-duplicates
     (vhash-fold (lambda (name data result)
                   (cons `((name . ,name)
                           (version . ,(car data)))
                         result))
                 '()
                 vh)))

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
                base-commit
                target-commit
                base-packages-vhash
                target-packages-vhash)
        #:extra-headers http-headers-for-unchanging-content)))))

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

        #f ;; TODO
        )))

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

(define (render-jobs mime-types conn)
  (render-html
   #:sxml (view-jobs
           (select-jobs-and-events conn))))

(define (render-job mime-types conn job-id query-parameters)
  (render-html
   #:sxml (view-job
           job-id
           query-parameters
           (log-for-job conn job-id
                        #:character-limit
                        (assq-ref query-parameters 'characters)
                        #:start-character
                        (assq-ref query-parameters 'start_character)))))

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

(define (controller request method-and-path-components mime-types body)
  (match method-and-path-components
    ((GET "assets" rest ...)
     (or (render-static-asset (string-join rest "/")
                              (request-headers request))
         (not-found (request-uri request))))
    ((GET "healthcheck")
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
  (define query-parameters
    (-> request
        request-uri
        uri-query
        parse-query-string))

  (define path
    (uri-path (request-uri request)))

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
    (('GET "revision" commit-hash) (if (guix-commit-exists? conn commit-hash)
                                      (render-view-revision mime-types
                                                            conn
                                                            commit-hash
                                                            #:path-base path)
                                      (render-unknown-revision mime-types
                                                               conn
                                                               commit-hash)))
    (('GET "revision" commit-hash "packages")
     (if (guix-commit-exists? conn commit-hash)
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
                                     #:path-base path))
         (render-unknown-revision mime-types
                                  conn
                                  commit-hash)))
    (('GET "revision" commit-hash "package" name version)
     (if (guix-commit-exists? conn commit-hash)
         (render-revision-package mime-types
                                  conn
                                  commit-hash
                                  name
                                  version)
         (render-unknown-revision mime-types
                                  conn
                                  commit-hash)))
    (('GET "repository" id)
     (match (select-git-repository conn id)
       ((label url cgit-url-base)
        (render-html
         #:sxml
         (view-git-repository
          id
          label url cgit-url-base
          (all-branches-with-most-recent-commit conn id))))
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
                     repository-id
                     branch-name
                     #:limit (assq-ref parsed-query-parameters 'limit_results)
                     #:after-date (assq-ref parsed-query-parameters
                                            'after_date)
                     #:before-date (assq-ref parsed-query-parameters
                                             'before_date)))))))
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
    (('GET "repository" repository-id "branch" branch-name "latest-processed-revision" "package" name version)
     (let ((commit-hash
            (latest-processed-commit-for-branch conn repository-id branch-name)))
       (if commit-hash
           (render-revision-package mime-types
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
     (with-base-and-target-commits
      query-parameters conn
      (lambda (base-commit base-revision-id target-commit target-revision-id)
        (if (not (and base-revision-id target-revision-id))
            (render-compare-unknown-commit mime-types
                                           conn
                                           base-commit
                                           base-revision-id
                                           target-commit
                                           target-revision-id)
            (render-compare mime-types
                            conn
                            base-commit
                            base-revision-id
                            target-commit
                            target-revision-id)))))
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
    (('GET "compare" "packages")
     (with-base-and-target-commits
      query-parameters conn
      (lambda (base-commit base-revision-id target-commit target-revision-id)
        (if (not (and base-revision-id target-revision-id))
            (render-compare-unknown-commit mime-types
                                           conn
                                           base-commit
                                           base-revision-id
                                           target-commit
                                           target-revision-id)
            (render-compare/packages mime-types
                                     conn
                                     base-commit
                                     base-revision-id
                                     target-commit
                                     target-revision-id)))))
    (('GET "jobs")
     (render-jobs mime-types
                  conn))
    (('GET "job" job-id)
     (let ((parsed-query-parameters
            (parse-query-parameters
             request
             `((start_character ,parse-number)
               (characters ,parse-number #:default 1000000)))))
       (render-job mime-types
                   conn
                   job-id
                   parsed-query-parameters)))
    (('GET path ...)
     (not-found (request-uri request)))))
