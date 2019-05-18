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

(define (render-view-revision mime-types
                              conn
                              commit-hash)
  (let ((packages-count
         (count-packages-in-revision conn commit-hash))
        (git-repositories-and-branches
         (git-branches-with-repository-details-for-commit conn commit-hash))
        (derivations-counts
         (count-packages-derivations-in-revision conn commit-hash)))
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
                                       derivations-counts))))))
      (else
       (apply render-html
              (view-revision
               commit-hash
               packages-count
               git-repositories-and-branches
               derivations-counts))))))

(define (texinfo->variants-alist s)
  (let ((stexi (texi-fragment->stexi s)))
    `((source . ,s)
      (html   . ,(with-output-to-string
                   (lambda ()
                     (sxml->html (stexi->shtml stexi)))))
      (plain . ,(stexi->plain-text stexi)))))

(define (render-revision-packages mime-types
                                  conn
                                  commit-hash
                                  query-parameters)
  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          `((error . "invalid query"))))
        (else
         (apply render-html
                (view-revision-packages commit-hash
                                        query-parameters
                                        '()
                                        '()
                                        #f))))

      (let* ((search-query (assq-ref query-parameters 'search_query))
             (limit-results (assq-ref query-parameters 'limit_results))
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
                        packages))))))
          (else
           (apply render-html
                  (view-revision-packages commit-hash
                                          query-parameters
                                          packages
                                          git-repositories
                                          show-next-page?)))))))

(define (render-revision-package mime-types
                                 conn
                                 commit-hash
                                 name
                                 version)
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
                                derivations))))))
      (else
       (apply render-html
              (view-revision-package-and-version commit-hash
                                                 name
                                                 version
                                                 metadata
                                                 derivations
                                                 git-repositories))))))

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
     (apply render-html
            (compare-unknown-commit base-commit
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
                                          target-packages-vhash))
           (derivation-changes
            (package-data-derivation-changes base-packages-vhash
                                             target-packages-vhash)))
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          `((new-packages . ,(list->vector new-packages))
            (removed-packages . ,(list->vector removed-packages))
            (version-changes . ,version-changes)
            (derivation-changes . ,derivation-changes))))
        (else
         (apply render-html
                (compare base-commit
                         target-commit
                         new-packages
                         removed-packages
                         version-changes
                         derivation-changes)))))))

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
         (apply render-html
                (compare/derivations
                 query-parameters
                 (valid-systems conn)
                 build-status-strings
                 '()
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
          (let ((base-derivations
                 (package-data-vhash->derivations-and-build-status
                  conn
                  base-packages-vhash
                  systems
                  targets
                  build-statuses))
                (target-derivations
                 (package-data-vhash->derivations-and-build-status
                  conn
                  target-packages-vhash
                  systems
                  targets
                  build-statuses)))
            (case (most-appropriate-mime-type
                   '(application/json text/html)
                   mime-types)
              ((application/json)
               (render-json
                `((base . ((commit . ,base-commit)
                           (derivations . ,(list->vector
                                            (derivations->alist
                                             base-derivations)))))
                  (target . ((commit . ,target-commit)
                             (derivations . ,(list->vector
                                              (derivations->alist
                                               target-derivations))))))))
              (else
               (apply render-html
                      (compare/derivations
                       query-parameters
                       (valid-systems conn)
                       build-status-strings
                       base-derivations
                       target-derivations)))))))))

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
                            (package-data-vhash->json target-packages-vhash))))))))
      (else
       (apply render-html
              (compare/packages
               base-commit
               target-commit
               base-packages-vhash
               target-packages-vhash))))))

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
          (apply render-html
                 (view-derivation derivation
                                  derivation-inputs
                                  derivation-outputs
                                  builds)))
        #f ;; TODO
        )))

(define (render-store-item conn filename)
  (let ((derivation (select-derivation-by-output-filename conn filename)))
    (match derivation
      (()
       #f)
      (derivations
       (apply render-html
              (view-store-item filename
                               derivations
                               (map (lambda (derivation)
                                      (match derivation
                                        ((file-name output-id rest ...)
                                         (select-derivations-using-output
                                          conn output-id))))
                                    derivations)))))))

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

(define (controller request method-and-path-components mime-types body conn)
  (define query-parameters
    (-> request
        request-uri
        uri-query
        parse-query-string))

  (match method-and-path-components
    ((GET)
     (apply render-html
            (index
             (map
              (lambda (git-repository-details)
                (cons
                 git-repository-details
                 (map
                  (match-lambda
                    ((id job-id commit source)
                     (list id
                           job-id
                           commit
                           source
                           (git-branches-for-commit conn commit))))
                  (guix-revisions-and-jobs-for-git-repository
                   conn
                   (car git-repository-details)))))
              (all-git-repositories conn)))))
    ((GET "builds")
     (apply render-html
            (view-builds (select-build-stats conn)
                         (select-builds-with-context conn))))
    ((GET "statistics")
     (apply render-html
            (view-statistics (count-guix-revisions conn)
                             (count-derivations conn))))
    ((GET "revision" commit-hash) (render-view-revision mime-types
                                                        conn
                                                        commit-hash))
    ((GET "revision" commit-hash "packages")
     (let ((parsed-query-parameters
            (guard-against-mutually-exclusive-query-parameters
             (parse-query-parameters
              request
              `((after_name     ,identity)
                (field          ,identity #:multi-value
                                #:default ("version" "synopsis"))
                (search_query   ,identity)
                (limit_results  ,parse-result-limit #:default 100)))
             ;; You can't specify a search query, but then also limit the
             ;; results by filtering for after a particular package name
             '((after_name search_query)))))

       (render-revision-packages mime-types
                                 conn
                                 commit-hash
                                 parsed-query-parameters)))
    ((GET "revision" commit-hash "package" name version) (render-revision-package
                                                          mime-types
                                                          conn
                                                          commit-hash
                                                          name
                                                          version))
    ((GET "branches")
     (apply render-html
            (view-branches
             (all-branches-with-most-recent-commit conn))))
    ((GET "branch" branch-name)
     (let ((parsed-query-parameters
            (parse-query-parameters
             request
             `((after_date     ,parse-datetime)
               (before_date    ,parse-datetime)
               (limit_results  ,parse-result-limit #:default 100)))))
       (apply
        render-html
        (if (any-invalid-query-parameters? parsed-query-parameters)
            (view-branch branch-name parsed-query-parameters '())
            (view-branch
             branch-name
             parsed-query-parameters
             (most-recent-commits-for-branch
              conn
              branch-name
              #:limit (assq-ref parsed-query-parameters 'limit_results)
              #:after-date (assq-ref parsed-query-parameters
                                     'after_date)
              #:before-date (assq-ref parsed-query-parameters
                                      'before_date)))))))
    ((GET "gnu" "store" filename)
     ;; These routes are a little special, as the extensions aren't used for
     ;; content negotiation, so just use the path from the request
     (let ((path (uri-path (request-uri request))))
       (if (string-suffix? ".drv" path)
           (render-derivation conn path)
           (render-store-item conn path))))
    ((GET "compare")
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
    ((GET "compare" "derivations")
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
    ((GET "compare" "packages")
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
    ((GET path ...)
     (render-static-asset request))))
