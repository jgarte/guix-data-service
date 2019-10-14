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

(define-module (guix-data-service web revision controller)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:use-module (texinfo plain-text)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web sxml)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service jobs load-new-guix-revision)
  #:use-module (guix-data-service model package)
  #:use-module (guix-data-service model git-branch)
  #:use-module (guix-data-service model git-repository)
  #:use-module (guix-data-service model derivation)
  #:use-module (guix-data-service model package-derivation)
  #:use-module (guix-data-service model package-metadata)
  #:use-module (guix-data-service model lint-checker)
  #:use-module (guix-data-service model lint-warning)
  #:use-module (guix-data-service model guix-revision)
  #:use-module (guix-data-service web revision html)
  #:export (revision-controller

            render-revision-lint-warnings
            render-revision-package-version
            render-revision-packages
            render-unknown-revision
            render-view-revision))

(define cache-control-default-max-age
  (* 60 60 24)) ; One day

(define http-headers-for-unchanging-content
  `((cache-control
     . (public
        (max-age . ,cache-control-default-max-age)))))

(define (revision-controller request
                             method-and-path-components
                             mime-types
                             body
                             conn)
  (define path
    (uri-path (request-uri request)))

  (match method-and-path-components
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
    (('GET "revision" commit-hash "package" name)
     (if (guix-commit-exists? conn commit-hash)
         (render-revision-package mime-types
                                  conn
                                  commit-hash
                                  name)
         (render-unknown-revision mime-types
                                  conn
                                  commit-hash)))
    (('GET "revision" commit-hash "package" name version)
     (if (guix-commit-exists? conn commit-hash)
         (render-revision-package-version mime-types
                                          conn
                                          commit-hash
                                          name
                                          version)
         (render-unknown-revision mime-types
                                  conn
                                  commit-hash)))
    (('GET "revision" commit-hash "lint-warnings")
     (if (guix-commit-exists? conn commit-hash)
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
                                          #:path-base path))
         (render-unknown-revision mime-types
                                  conn
                                  commit-hash)))))

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
         (select-jobs-and-events-for-commit conn commit-hash))
        (lint-warning-counts
         (lint-warning-count-by-lint-checker-for-revision conn commit-hash)))
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
                                       derivations-counts)))
          (lint_warning_counts . ,(map (match-lambda
                                         ((name description network-dependent count)
                                          `(,name . ((description       . ,description)
                                                     (network_dependent . ,(string=? network-dependent "t"))
                                                     (count             . ,(string->number count))))))
                                       lint-warning-counts)))
        #:extra-headers http-headers-for-unchanging-content))
      (else
       (render-html
        #:sxml (view-revision
                commit-hash
                packages-count
                git-repositories-and-branches
                derivations-counts
                jobs-and-events
                lint-warning-counts
                #:path-base path-base
                #:header-text header-text)
        #:extra-headers http-headers-for-unchanging-content)))))

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
                                  #:key
                                  (path-base "/revision/")
                                  (header-text
                                   `("Revision "
                                     (samp ,commit-hash)))
                                  (header-link
                                   (string-append
                                    "/revision/" commit-hash)))
  (let ((package-versions
         (select-package-versions-for-revision conn
                                               commit-hash
                                               name))
        (git-repositories-and-branches
         (git-branches-with-repository-details-for-commit conn
                                                          commit-hash)))
    (case (most-appropriate-mime-type
           '(application/json text/html)
           mime-types)
      ((application/json)
       (render-json
        `((versions . ,(list->vector package-versions)))
        #:extra-headers http-headers-for-unchanging-content))
      (else
       (render-html
        #:sxml (view-revision-package commit-hash
                                      name
                                      package-versions
                                      git-repositories-and-branches
                                      #:path-base path-base
                                      #:header-text header-text
                                      #:header-link header-link)
        #:extra-headers http-headers-for-unchanging-content)))))

(define* (render-revision-package-version mime-types
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
                                             commit-hash))
        (lint-warnings
         (select-lint-warnings-by-revision-package-name-and-version
          conn
          commit-hash
          name
          version)))
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
                                                  lint-warnings
                                                  #:header-text header-text
                                                  #:header-link header-link)
        #:extra-headers http-headers-for-unchanging-content)))))

(define* (render-revision-lint-warnings mime-types
                                        conn
                                        commit-hash
                                        query-parameters
                                        #:key
                                        (path-base "/revision/")
                                        (header-text
                                         `("Revision " (samp ,commit-hash)))
                                        (header-link
                                         (string-append "/revision/" commit-hash)))
  (define lint-checker-options
    (map (match-lambda
           ((name description network-dependent)
            (cons (string-append name ": " description )
                  name)))
         (lint-checkers-for-revision conn commit-hash)))

  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          `((error . "invalid query"))))
        (else
         (render-html
          #:sxml (view-revision-lint-warnings commit-hash
                                              query-parameters
                                              '()
                                              lint-checker-options
                                              #:path-base path-base
                                              #:header-text header-text
                                              #:header-link header-link))))

      (let* ((package-query (assq-ref query-parameters 'package_query))
             (linters (assq-ref query-parameters 'linter))
             (message-query (assq-ref query-parameters 'message_query))
             (fields (assq-ref query-parameters 'field))
             (git-repositories
              (git-repositories-containing-commit conn
                                                  commit-hash))
             (lint-warnings
              (lint-warnings-for-guix-revision conn commit-hash
                                               #:package-query package-query
                                               #:linters linters
                                               #:message-query message-query)))
        (case (most-appropriate-mime-type
               '(application/json text/html)
               mime-types)
          ((application/json)
           (render-json
            `((revision
               . ((commit . ,commit-hash)))
              (lint_warnings
               . ,(list->vector
                   (map (match-lambda
                          ((id lint-checker-name lint-checker-description
                               lint-checker-network-dependent
                               package-name package-version
                               file line-number column-number
                               message)
                           `((package . ((name    . ,package-name)
                                         (version . ,package-version)))
                             ,@(if (member "message" fields)
                                   `((message . ,message))
                                   '())
                             ,@(if (member "location" fields)
                                   `((location . ((file          . ,file)
                                                  (line-number   . ,line-number)
                                                  (column-number . ,column-number))))
                                   '()))))
                        lint-warnings))))
            #:extra-headers http-headers-for-unchanging-content))
          (else
           (render-html
            #:sxml (view-revision-lint-warnings commit-hash
                                                query-parameters
                                                lint-warnings
                                                git-repositories
                                                lint-checker-options
                                                #:path-base path-base
                                                #:header-text header-text
                                                #:header-link header-link)
            #:extra-headers http-headers-for-unchanging-content))))))
