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
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:use-module (texinfo plain-text)
  #:use-module (json)
  #:use-module (guix-data-service utils)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web sxml)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service jobs load-new-guix-revision)
  #:use-module (guix-data-service model build)
  #:use-module (guix-data-service model build-server)
  #:use-module (guix-data-service model build-status)
  #:use-module (guix-data-service model channel-news)
  #:use-module (guix-data-service model channel-instance)
  #:use-module (guix-data-service model package)
  #:use-module (guix-data-service model git-branch)
  #:use-module (guix-data-service model git-repository)
  #:use-module (guix-data-service model derivation)
  #:use-module (guix-data-service model package-derivation)
  #:use-module (guix-data-service model package-metadata)
  #:use-module (guix-data-service model lint-checker)
  #:use-module (guix-data-service model lint-warning)
  #:use-module (guix-data-service model lint-warning-message)
  #:use-module (guix-data-service model guix-revision)
  #:use-module (guix-data-service model system-test)
  #:use-module (guix-data-service model nar)
  #:use-module (guix-data-service web revision html)
  #:export (revision-controller

            render-revision-lint-warnings
            render-revision-package-version
            render-revision-packages
            render-revision-package-reproduciblity
            render-revision-package-substitute-availability
            render-revision-package-derivations
            render-revision-fixed-output-package-derivations
            render-revision-package-derivation-outputs
            render-unknown-revision
            render-view-revision))

(define cache-control-default-max-age
  (* 60 60 24)) ; One day

(define http-headers-for-unchanging-content
  `((cache-control
     . (public
        (max-age . ,cache-control-default-max-age)))))

(define (parse-build-status status)
  (if (member status build-status-strings)
      status
      (make-invalid-query-parameter
       status
       (string-append "unknown build status: "
                      status))))

(define (parse-build-server v)
  (letpar& ((build-servers
             (with-thread-postgresql-connection select-build-servers)))
    (or (any (match-lambda
               ((id url lookup-all-derivations? lookup-builds?)
                (if (eq? (string->number v)
                         id)
                    id
                    #f)))
             build-servers)
        (make-invalid-query-parameter
         v
         "unknown build server"))))

(define (revision-controller request
                             method-and-path-components
                             mime-types
                             body)
  (define path
    (uri-path (request-uri request)))

  (match method-and-path-components
    (('GET "revision" commit-hash)
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (render-view-revision mime-types
                               commit-hash
                               #:path-base path)
         (render-unknown-revision mime-types
                                  commit-hash)))
    (('GET "revision" commit-hash "news")
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (let ((parsed-query-parameters
                (parse-query-parameters
                 request
                 `((lang ,identity #:multi-value)))))
           (render-revision-news mime-types
                                 commit-hash
                                 parsed-query-parameters))
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "packages")
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (let ((parsed-query-parameters
                (guard-against-mutually-exclusive-query-parameters
                 (parse-query-parameters
                  request
                  `((locale         ,identity #:default "en_US.UTF-8")
                    (after_name     ,identity)
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
                                     commit-hash
                                     parsed-query-parameters
                                     #:path-base path))
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "packages-translation-availability")
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (render-revision-packages-translation-availability mime-types
                                                            commit-hash
                                                            #:path-base path)
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "package" name)
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (render-revision-package mime-types
                                  commit-hash
                                  name)
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "package" name version)
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (let ((parsed-query-parameters
                (parse-query-parameters
                 request
                 `((locale ,identity #:default "en_US.UTF-8")))))
           (render-revision-package-version mime-types
                                               commit-hash
                                               name
                                               version
                                               parsed-query-parameters))
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "package-derivations")
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (let ((parsed-query-parameters
                (guard-against-mutually-exclusive-query-parameters
                 (parse-query-parameters
                  request
                  `((search_query   ,identity)
                    (system ,parse-system #:multi-value)
                    (target ,parse-target #:multi-value)
                    (maximum_builds ,parse-number)
                    (minimum_builds ,parse-number)
                    (build_status   ,parse-derivation-build-status)
                    (field          ,identity #:multi-value
                                    #:default ("system" "target" "builds"))
                    (after_name ,identity)
                    (limit_results  ,parse-result-limit
                                    #:no-default-when (all_results)
                                    #:default 10)
                    (all_results    ,parse-checkbox-value)))
                 '((limit_results all_results)))))

           (render-revision-package-derivations mime-types
                                                commit-hash
                                                parsed-query-parameters
                                                #:path-base path))
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "fixed-output-package-derivations")
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (let ((parsed-query-parameters
                (guard-against-mutually-exclusive-query-parameters
                 (parse-query-parameters
                  request
                  `((system ,parse-system #:default "x86_64-linux")
                    (target ,parse-target #:default "")
                    (latest_build_status ,parse-build-status)
                    (after_name          ,identity)
                    (limit_results       ,parse-result-limit
                                         #:no-default-when (all_results)
                                         #:default 50)
                    (all_results         ,parse-checkbox-value)))
                 '((limit_results all_results)))))

           (render-revision-fixed-output-package-derivations
            mime-types
            commit-hash
            parsed-query-parameters
            #:path-base path))
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "package-derivation-outputs")
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (let ((parsed-query-parameters
                (guard-against-mutually-exclusive-query-parameters
                 (parse-query-parameters
                  request
                  `((search_query ,identity)
                    (after_path ,identity)
                    (substitutes_available_from ,parse-number
                                                #:multi-value)
                    (substitutes_not_available_from ,parse-number
                                                    #:multi-value)
                    (output_consistency ,identity
                                        #:default "any")
                    (system ,parse-system #:default "x86_64-linux")
                    (target ,parse-target #:default "")
                    (limit_results  ,parse-result-limit
                                    #:no-default-when (all_results)
                                    #:default 10)
                    (all_results    ,parse-checkbox-value)))
                 '((limit_results all_results)))))

           (render-revision-package-derivation-outputs mime-types
                                                       commit-hash
                                                       parsed-query-parameters
                                                       #:path-base path))
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "system-tests")
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (let ((parsed-query-parameters
                (parse-query-parameters
                 request
                 `((system ,parse-system #:default "x86_64-linux")))))
           (render-revision-system-tests mime-types
                                         commit-hash
                                         parsed-query-parameters
                                         #:path-base path))
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "channel-instances")
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (channel-instances-exist-for-guix-revision? conn commit-hash))))
         (render-revision-channel-instances mime-types
                                            commit-hash
                                            #:path-base path)
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "package-substitute-availability")
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (render-revision-package-substitute-availability mime-types
                                                          commit-hash
                                                          #:path-base path)
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "package-reproducibility")
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (render-revision-package-reproduciblity mime-types
                                                 commit-hash
                                                 #:path-base path)
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "builds")
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (let ((parsed-query-parameters
                (guard-against-mutually-exclusive-query-parameters
                 (parse-query-parameters
                  request
                  `((build_status ,parse-build-status #:multi-value)
                    (build_server ,parse-build-server #:multi-value)
                    (system ,parse-system #:default "x86_64-linux")
                    (target ,parse-target #:default "")
                    (limit_results         ,parse-result-limit
                                           #:no-default-when (all_results)
                                           #:default 50)
                    (all_results           ,parse-checkbox-value)))
                 '((limit_results all_results)))))

           (render-revision-builds mime-types
                                   commit-hash
                                   parsed-query-parameters
                                   #:path-base path))
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (('GET "revision" commit-hash "lint-warnings")
     (if (parallel-via-thread-pool-channel
          (with-thread-postgresql-connection
           (lambda (conn)
             (guix-revision-loaded-successfully? conn commit-hash))))
         (let ((parsed-query-parameters
                (parse-query-parameters
                 request
                 `((locale         ,identity #:default "en_US.UTF-8")
                   (package_query  ,identity)
                   (linter         ,identity #:multi-value)
                   (message_query  ,identity)
                   (field          ,identity #:multi-value
                                   #:default ("linter"
                                              "message"
                                              "location"))))))

           (render-revision-lint-warnings mime-types
                                          commit-hash
                                          parsed-query-parameters
                                          #:path-base path))
         (render-unprocessed-revision mime-types
                                      commit-hash)))
    (_ #f)))

(define (texinfo->variants-alist s locale)
  (let ((stexi (texi-fragment->stexi s)))
    `((source . ,s)
      (html   . ,(with-output-to-string
                   (lambda ()
                     (sxml->html (stexi->shtml stexi)))))
      (plain . ,(stexi->plain-text stexi))
      (locale . ,locale))))

(define (render-unknown-revision mime-types commit-hash)
  (case (most-appropriate-mime-type
         '(application/json text/html)
         mime-types)
    ((application/json)
     (render-json
      '((unknown_commit . ,commit-hash))
      #:code 404))
    (else
     (letpar& ((job
                (with-thread-postgresql-connection
                 (lambda (conn)
                   (select-job-for-commit conn commit-hash))))
               (git-repositories-and-branches
                (with-thread-postgresql-connection
                 (lambda (conn)
                   (git-branches-with-repository-details-for-commit conn
                                                                    commit-hash))))
               (jobs-and-events
                (with-thread-postgresql-connection
                 (lambda (conn)
                   (select-jobs-and-events-for-commit conn commit-hash)))))

     (render-html
      #:code 404
      #:sxml (unknown-revision
              commit-hash
              job
              git-repositories-and-branches
              jobs-and-events))))))

(define (render-unprocessed-revision mime-types commit-hash)
  (case (most-appropriate-mime-type
         '(application/json text/html)
         mime-types)
    ((application/json)
     (render-json
      '((unknown_commit . ,commit-hash))
      #:code 404))
    (else
     (letpar& ((job
                (with-thread-postgresql-connection
                 (lambda (conn)
                   (select-job-for-commit conn commit-hash))))
               (git-repositories-and-branches
                (with-thread-postgresql-connection
                 (lambda (conn)
                   (git-branches-with-repository-details-for-commit conn
                                                                    commit-hash))))
               (jobs-and-events
                (with-thread-postgresql-connection
                 (lambda (conn)
                   (select-jobs-and-events-for-commit conn commit-hash)))))

     (render-html
      #:code 404
      #:sxml (unprocessed-revision
              commit-hash
              job
              git-repositories-and-branches
              jobs-and-events))))))

(define* (render-view-revision mime-types
                               commit-hash
                               #:key path-base
                               (header-text
                                `("Revision " (samp ,commit-hash))))
  (letpar& ((packages-count
             (with-thread-postgresql-connection
              (lambda (conn)
                (count-packages-in-revision conn commit-hash))))
            (git-repositories-and-branches
             (with-thread-postgresql-connection
              (lambda (conn)
                (git-branches-with-repository-details-for-commit conn
                                                                 commit-hash))))
            (derivations-counts
             (with-thread-postgresql-connection
              (lambda (conn)
                (count-packages-derivations-in-revision conn commit-hash))))
            (jobs-and-events
             (with-thread-postgresql-connection
              (lambda (conn)
                (select-jobs-and-events-for-commit conn commit-hash))))
            (lint-warning-counts
             (with-thread-postgresql-connection
              (lambda (conn)
                (lint-warning-count-by-lint-checker-for-revision conn
                                                                 commit-hash)))))
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

(define* (render-revision-system-tests mime-types
                                       commit-hash
                                       query-parameters
                                       #:key
                                       (path-base "/revision/")
                                       (header-text
                                        `("Revision " (samp ,commit-hash)))
                                       (header-link
                                        (string-append "/revision/" commit-hash)))
  (letpar& ((system-tests
             (with-thread-postgresql-connection
              (lambda (conn)
                (select-system-tests-for-guix-revision
                 conn
                 (assq-ref query-parameters 'system)
                 commit-hash)))))
    (case (most-appropriate-mime-type
           '(application/json text/html)
           mime-types)
      ((application/json)
       (render-json
        `((commit . ,commit-hash)
          (system . ,(assq-ref query-parameters 'system))
          (system_tests
           . ,(list->vector
               (map
                (match-lambda
                  ((name description file line column-number
                         derivation-file-name builds)
                   `((name . ,name)
                     (description . ,description)
                     (location . ((file . ,file)
                                  (line . ,line)
                                  (column-number . ,column-number)))
                     (derivation . ,derivation-file-name)
                     (builds . ,(list->vector builds)))))
                system-tests))))))
      (else
       (letpar& ((git-repositories
                  (with-thread-postgresql-connection
                   (lambda (conn)
                     (git-repositories-containing-commit conn
                                                         commit-hash))))
                 (systems
                  (with-thread-postgresql-connection valid-systems)))
         (render-html
          #:sxml (view-revision-system-tests
                  commit-hash
                  system-tests
                  git-repositories
                  systems
                  query-parameters
                  #:path-base path-base
                  #:header-text header-text
                  #:header-link header-link)))))))

(define* (render-revision-channel-instances mime-types
                                            commit-hash
                                            #:key
                                            (path-base "/revision/")
                                            (header-text
                                             `("Revision " (samp ,commit-hash)))
                                            (header-link
                                             (string-append "/revision/"
                                                            commit-hash)))
  (letpar& ((channel-instances
             (with-thread-postgresql-connection
              (lambda (conn)
                (select-channel-instances-for-guix-revision conn commit-hash)))))
    (case (most-appropriate-mime-type
           '(application/json text/html)
           mime-types)
      ((application/json)
       (render-json
        `((channel_instances . ,(list->vector
                                 (map
                                  (match-lambda
                                    ((system derivation-file-name builds)
                                     `((system     . ,system)
                                       (derivation . ,derivation-file-name)
                                       (builds     . ,(list->vector builds)))))
                                  channel-instances))))))
      (else
       (render-html
        #:sxml (view-revision-channel-instances
                commit-hash
                channel-instances
                #:path-base path-base
                #:header-text header-text
                #:header-link header-link))))))

(define* (render-revision-package-substitute-availability mime-types
                                                          commit-hash
                                                          #:key path-base)
  (letpar& ((substitute-availability
             (with-thread-postgresql-connection
              (lambda (conn)
                (select-package-output-availability-for-revision conn
                                                                 commit-hash))))
            (build-server-urls
             (with-thread-postgresql-connection
              select-build-server-urls-by-id)))
    (case (most-appropriate-mime-type
           '(application/json text/html)
           mime-types)
      ((application/json)
       (render-json
        '()))                           ; TODO
      (else
       (render-html
        #:sxml (view-revision-package-substitute-availability
                commit-hash
                substitute-availability
                build-server-urls))))))

(define* (render-revision-package-reproduciblity mime-types
                                                 commit-hash
                                                 #:key
                                                 (path-base "/revision/")
                                                 (header-text
                                                  `("Revision "
                                                    (samp ,commit-hash)))
                                                 (header-link
                                                  (string-append "/revision/"
                                                                 commit-hash)))
  (letpar& ((output-consistency
             (with-thread-postgresql-connection
              (lambda (conn)
                (select-output-consistency-for-revision conn commit-hash)))))
    (case (most-appropriate-mime-type
           '(application/json text/html)
           mime-types)
      ((application/json)
       (render-json
        '()))
      (else
       (render-html
        #:sxml (view-revision-package-reproducibility
                commit-hash
                output-consistency
                #:path-base path-base
                #:header-text header-text
                #:header-link header-link))))))

(define (render-revision-news mime-types
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
         (render-html
          #:sxml (view-revision-news commit-hash
                                     query-parameters
                                     '()))))
      (letpar& ((news-entries
                 (with-thread-postgresql-connection
                  (lambda (conn)
                    (select-channel-news-entries-contained-in-guix-revision
                     conn
                     commit-hash)))))
        (case (most-appropriate-mime-type
               '(application/json text/html)
               mime-types)
          ((application/json)
           (render-json
            '()))
          (else
           (render-html
            #:sxml (view-revision-news commit-hash
                                       query-parameters
                                       news-entries)
            #:extra-headers http-headers-for-unchanging-content))))))

(define* (render-revision-packages mime-types
                                   commit-hash
                                   query-parameters
                                   #:key
                                   (path-base "/revision/")
                                   (header-text
                                    `("Revision " (samp ,commit-hash)))
                                   (header-link
                                    (string-append "/revision/" commit-hash)))
  (define (description-and-synopsis-locale-options locale-data)
    (map
     (match-lambda
       ((locale)
        locale))
     locale-data))

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
                                         #f
                                         #f
                                         #:path-base path-base
                                         #:header-text header-text
                                         #:header-link header-link))))

      (let ((search-query (assq-ref query-parameters 'search_query))
            (limit-results (or (assq-ref query-parameters 'limit_results)
                               99999)) ; TODO There shouldn't be a limit
            (fields (assq-ref query-parameters 'field))
            (locale (assq-ref query-parameters 'locale)))
        (letpar&
            ((packages
              (with-thread-postgresql-connection
               (lambda (conn)
                 (if search-query
                     (search-packages-in-revision
                      conn
                      commit-hash
                      search-query
                      #:limit-results limit-results
                      #:locale locale)
                     (select-packages-in-revision
                      conn
                      commit-hash
                      #:limit-results limit-results
                      #:after-name (assq-ref query-parameters 'after_name)
                      #:locale (assq-ref query-parameters 'locale))))))
             (git-repositories
              (with-thread-postgresql-connection
               (lambda (conn)
                 (git-repositories-containing-commit conn
                                                     commit-hash)))))
          (let ((show-next-page?
                 (and (not search-query)
                      (>= (length packages)
                          limit-results)))
                (any-translations? (any-package-synopsis-or-descriptions-translations?
                                    packages locale)))
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
                              ((name version synopsis synopsis-locale description description-locale home-page
                                     location-file location-line
                                     location-column-number licenses)
                               `((name . ,name)
                                 ,@(if (member "version" fields)
                                       `((version . ,version))
                                       '())
                                 ,@(if (member "synopsis" fields)
                                       `((synopsis
                                          . ,(texinfo->variants-alist synopsis synopsis-locale)))
                                       '())
                                 ,@(if (member "description" fields)
                                       `((description
                                          . ,(texinfo->variants-alist description description-locale)))
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
               (letpar&
                   ((locale-options
                     (with-thread-postgresql-connection
                      (lambda (conn)
                        (description-and-synopsis-locale-options
                         (package-description-and-synopsis-locale-options-guix-revision
                          conn
                          (commit->revision-id conn commit-hash)))))))
                 (render-html
                  #:sxml (view-revision-packages commit-hash
                                                 query-parameters
                                                 packages
                                                 git-repositories
                                                 show-next-page?
                                                 locale-options
                                                 any-translations?
                                                 #:path-base path-base
                                                 #:header-text header-text
                                                 #:header-link header-link)
                  #:extra-headers http-headers-for-unchanging-content)))))))))

(define* (render-revision-packages-translation-availability mime-types
                                                            commit-hash
                                                            #:key
                                                            path-base
                                                            (header-link
                                                             (string-append
                                                              "/revision/" commit-hash))
                                                            (header-text
                                                             `("Revision " (samp ,commit-hash))))
  (letpar& ((package-synopsis-counts
             (with-thread-postgresql-connection
              (lambda (conn)
                (synopsis-counts-by-locale conn
                                           (commit->revision-id
                                            conn
                                            commit-hash)))))
            (package-description-counts
             (with-thread-postgresql-connection
              (lambda (conn)
                (description-counts-by-locale conn
                                              (commit->revision-id
                                               conn
                                               commit-hash))))))
    (case (most-appropriate-mime-type
           '(application/json text/html)
           mime-types)
      ((application/json)
       (render-json
        `((package-synopsis-counts . ,package-synopsis-counts)
          (package-description-counts . ,package-description-counts))))
      (else
       (render-html
        #:sxml
        (view-revision-packages-translation-availability commit-hash
                                                         package-synopsis-counts
                                                         package-description-counts
                                                         #:path-base path-base
                                                         #:header-link header-link
                                                         #:header-text header-text))))))

(define* (render-revision-package mime-types
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
  (letpar& ((package-versions
             (with-thread-postgresql-connection
              (lambda (conn)
                (select-package-versions-for-revision conn
                                                      commit-hash
                                                      name))))
            (git-repositories-and-branches
             (with-thread-postgresql-connection
              (lambda (conn)
                (git-branches-with-repository-details-for-commit conn
                                                                 commit-hash)))))
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
                                          commit-hash
                                          name
                                          version
                                          query-parameters
                                          #:key
                                          (header-text
                                           `("Revision "
                                             (samp ,commit-hash)))
                                          (header-link
                                           (string-append
                                            "/revision/" commit-hash))
                                          version-history-link)

  (define locale-options
    (map
     (match-lambda
       ((locale)
        locale))
     (parallel-via-thread-pool-channel
      (with-thread-postgresql-connection
       (lambda (conn)
         (delete-duplicates
          (append
           (package-description-and-synopsis-locale-options-guix-revision
            conn (commit->revision-id conn commit-hash))
           (lint-warning-message-locales-for-revision conn commit-hash))))))))

  (define locale (assq-ref query-parameters 'locale))

  (letpar& ((metadata
             (with-thread-postgresql-connection
              (lambda (conn)
                (select-package-metadata-by-revision-name-and-version
                 conn
                 commit-hash
                 name
                 version
                 locale))))
            (derivations
             (with-thread-postgresql-connection
              (lambda (conn)
                (select-derivations-by-revision-name-and-version
                 conn
                 commit-hash
                 name
                 version))))
            (git-repositories
             (with-thread-postgresql-connection
              (lambda (conn)
                (git-repositories-containing-commit conn
                                                    commit-hash))))
            (lint-warnings
             (with-thread-postgresql-connection
              (lambda (conn)
                (select-lint-warnings-by-revision-package-name-and-version
                 conn
                 commit-hash
                 name
                 version
                 #:locale locale)))))
    (case (most-appropriate-mime-type
           '(application/json text/html)
           mime-types)
      ((application/json)
       (render-json
        `((name . ,name)
          (version . ,version)
          ,@(match metadata
              (((synopsis synopsis-locale description description-locale home-page file line column-number
                          licenses))
               `((synopsis . ,(texinfo->variants-alist synopsis synopsis-locale))
                 (description . ,(texinfo->variants-alist description description-locale))
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
                                                  query-parameters
                                                  locale-options
                                                  #:header-text header-text
                                                  #:header-link header-link
                                                  #:version-history-link
                                                  version-history-link)
        #:extra-headers http-headers-for-unchanging-content)))))

(define* (render-revision-package-derivations mime-types
                                              commit-hash
                                              query-parameters
                                              #:key
                                              (path-base "/revision/")
                                              (header-text
                                               `("Revision " (samp ,commit-hash)))
                                              (header-link
                                               (string-append "/revision/"
                                                              commit-hash)))
  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          `((error . "invalid query"))))
        (else
         (letpar& ((systems
                    (with-thread-postgresql-connection valid-systems))
                   (targets
                    (with-thread-postgresql-connection valid-targets)))
           (render-html
            #:sxml (view-revision-package-derivations commit-hash
                                                      query-parameters
                                                      systems
                                                      (valid-targets->options
                                                       targets)
                                                      '()
                                                      '()
                                                      #f
                                                      #:path-base path-base
                                                      #:header-text header-text
                                                      #:header-link header-link)))))
      (let ((limit-results
             (assq-ref query-parameters 'limit_results))
            (all-results
             (assq-ref query-parameters 'all_results))
            (search-query
             (assq-ref query-parameters 'search_query))
            (fields
             (assq-ref query-parameters 'field)))
        (letpar&
            ((derivations
              (with-thread-postgresql-connection
               (lambda (conn)
                 (if search-query
                     (search-package-derivations-in-revision
                      conn
                      commit-hash
                      search-query
                      #:systems (assq-ref query-parameters 'system)
                      #:targets (assq-ref query-parameters 'target)
                      #:maximum-builds (assq-ref query-parameters 'maximum_builds)
                      #:minimum-builds (assq-ref query-parameters 'minimum_builds)
                      #:build-status (and=> (assq-ref query-parameters
                                                      'build_status)
                                            string->symbol)
                      #:limit-results limit-results
                      #:after-name (assq-ref query-parameters 'after_name)
                      #:include-builds? (member "builds" fields))
                     (select-package-derivations-in-revision
                      conn
                      commit-hash
                      #:systems (assq-ref query-parameters 'system)
                      #:targets (assq-ref query-parameters 'target)
                      #:maximum-builds (assq-ref query-parameters 'maximum_builds)
                      #:minimum-builds (assq-ref query-parameters 'minimum_builds)
                      #:build-status (and=> (assq-ref query-parameters
                                                      'build_status)
                                            string->symbol)
                      #:limit-results limit-results
                      #:after-name (assq-ref query-parameters 'after_name)
                      #:include-builds? (member "builds" fields))))))
             (build-server-urls
              (with-thread-postgresql-connection
               select-build-server-urls-by-id)))
          (let ((show-next-page?
                 (if all-results
                     #f
                     (and (not (null? derivations))
                          (>= (length derivations)
                              limit-results)))))
            (case (most-appropriate-mime-type
                   '(application/json text/html)
                   mime-types)
              ((application/json)
               (render-json
                `((derivations . ,(list->vector
                                   (map (match-lambda
                                          ((derivation system target)
                                           `((derivation . ,derivation)
                                             ,@(if (member "system" fields)
                                                   `((system . ,system))
                                                   '())
                                             ,@(if (member "target" fields)
                                                   `((target . ,target))
                                                   '())))
                                          ((derivation system target builds)
                                           `((derivation . ,derivation)
                                             ,@(if (member "system" fields)
                                                   `((system . ,system))
                                                   '())
                                             ,@(if (member "target" fields)
                                                   `((target . ,target))
                                                   '())
                                             (builds . ,builds))))
                                        derivations))))))
              (else
               (letpar& ((systems
                          (with-thread-postgresql-connection valid-systems))
                         (targets
                          (with-thread-postgresql-connection valid-targets)))
                 (render-html
                  #:sxml (view-revision-package-derivations
                          commit-hash
                          query-parameters
                          systems
                          (valid-targets->options targets)
                          derivations
                          build-server-urls
                          show-next-page?
                          #:path-base path-base
                          #:header-text header-text
                          #:header-link header-link))))))))))

(define* (render-revision-fixed-output-package-derivations
          mime-types
          commit-hash
          query-parameters
          #:key
          (path-base "/revision/")
          (header-text
           `("Revision " (samp ,commit-hash)))
          (header-link
           (string-append "/revision/"
                          commit-hash)))
  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          `((error . "invalid query"))))
        (else
         (letpar& ((systems
                    (with-thread-postgresql-connection valid-systems))
                   (targets
                    (with-thread-postgresql-connection valid-targets)))
           (render-html
            #:sxml (view-revision-fixed-output-package-derivations
                    commit-hash
                    query-parameters
                    systems
                    (valid-targets->options targets)
                    '()
                    '()
                    #f
                    #:path-base path-base
                    #:header-text header-text
                    #:header-link header-link)))))
      (let ((limit-results
             (assq-ref query-parameters 'limit_results))
            (all-results
             (assq-ref query-parameters 'all_results))
            (search-query
             (assq-ref query-parameters 'search_query))
            (fields
             (assq-ref query-parameters 'field)))
        (letpar&
            ((derivations
              (with-thread-postgresql-connection
               (lambda (conn)
                 (select-fixed-output-package-derivations-in-revision
                  conn
                  commit-hash
                  (assq-ref query-parameters 'system)
                  (assq-ref query-parameters 'target)
                  #:latest-build-status (assq-ref query-parameters
                                                  'latest_build_status)
                  #:limit-results limit-results
                  #:after-derivation-file-name
                  (assq-ref query-parameters 'after_name)))))
             (build-server-urls
              (with-thread-postgresql-connection
               select-build-server-urls-by-id)))
          (let ((show-next-page?
                 (if all-results
                     #f
                     (and (not (null? derivations))
                          (>= (length derivations)
                              limit-results)))))
            (case (most-appropriate-mime-type
                   '(application/json text/html)
                   mime-types)
              ((application/json)
               (render-json
                `((derivations . ,(list->vector derivations)))))
              (else
               (letpar& ((systems
                          (with-thread-postgresql-connection valid-systems))
                         (targets
                          (with-thread-postgresql-connection valid-targets)))
                 (render-html
                  #:sxml (view-revision-fixed-output-package-derivations
                          commit-hash
                          query-parameters
                          systems
                          (valid-targets->options targets)
                          derivations
                          build-server-urls
                          show-next-page?
                          #:path-base path-base
                          #:header-text header-text
                          #:header-link header-link))))))))))

(define* (render-revision-package-derivation-outputs
          mime-types
          commit-hash
          query-parameters
          #:key
          (path-base "/revision/")
          (header-text
           `("Revision " (samp ,commit-hash)))
          (header-link
           (string-append "/revision/" commit-hash)))
  (define build-server-urls
    (parallel-via-thread-pool-channel
     (with-thread-postgresql-connection select-build-server-urls-by-id)))

  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          `((error . "invalid query"))))
        (else
         (letpar& ((systems
                    (with-thread-postgresql-connection valid-systems))
                   (targets
                    (with-thread-postgresql-connection valid-targets)))
           (render-html
            #:sxml (view-revision-package-derivation-outputs
                    commit-hash
                    query-parameters
                    '()
                    build-server-urls
                    systems
                    (valid-targets->options targets)
                    #f
                    #:path-base path-base
                    #:header-text header-text
                    #:header-link header-link)))))
      (let ((limit-results
             (assq-ref query-parameters 'limit_results))
            (all-results
             (assq-ref query-parameters 'all_results)))
        (letpar&
            ((derivation-outputs
              (with-thread-postgresql-connection
               (lambda (conn)
                 (select-derivation-outputs-in-revision
                  conn
                  commit-hash
                  #:search-query (assq-ref query-parameters 'search_query)
                  #:nars-from-build-servers
                  (assq-ref query-parameters 'substitutes_available_from)
                  #:no-nars-from-build-servers
                  (assq-ref query-parameters 'substitutes_not_available_from)
                  #:output-consistency
                  (assq-ref query-parameters 'output_consistency)
                  #:system (assq-ref query-parameters 'system)
                  #:target (assq-ref query-parameters 'target)
                  #:limit-results limit-results
                  #:after-path (assq-ref query-parameters 'after_path))))))
          (let ((show-next-page?
                 (if all-results
                     #f
                     (>= (length derivation-outputs)
                         limit-results))))
            (case (most-appropriate-mime-type
                   '(application/json text/html)
                   mime-types)
              ((application/json)
               (render-json
                `((revision . ,commit-hash)
                  (store_paths
                   . ,(list->vector
                       (map (match-lambda
                              ((package-name package-version
                                             path hash-algorithm hash recursive
                                             nars)
                               `((package . ((name    . ,package-name)
                                             (version . ,package-version)))
                                 (path . ,path)
                                 (data
                                  . ,(if (null? hash-algorithm)
                                         (list->vector
                                          (map
                                           (match-lambda
                                             ((hash . nars)
                                              `((hash . ,hash)
                                                (nars . ,(list->vector nars)))))
                                           (group-to-alist
                                            (lambda (nar)
                                              (cons (assoc-ref nar "hash")
                                                    nar))
                                            (vector->list nars))))
                                         hash))
                                 (output_consistency
                                  . ,(let* ((hashes
                                             (delete-duplicates
                                              (map (lambda (nar)
                                                     (assoc-ref nar "hash"))
                                                   (vector->list nars))))
                                            (build-servers
                                             (delete-duplicates
                                              (map (lambda (nar)
                                                     (assoc-ref nar "build_server_id"))
                                                   (vector->list nars))))
                                            (hash-count
                                             (length hashes))
                                            (build-server-count
                                             (length build-servers)))
                                       (cond
                                        ((or (eq? hash-count 0)
                                             (eq? build-server-count 1))
                                         "unknown")
                                        ((eq? hash-count 1)
                                         "matching")
                                        ((> hash-count 1)
                                         "not-matching")))))))
                            derivation-outputs))))))
              (else
               (letpar& ((systems
                          (with-thread-postgresql-connection valid-systems))
                         (targets
                          (with-thread-postgresql-connection valid-targets)))
                 (render-html
                  #:sxml (view-revision-package-derivation-outputs
                          commit-hash
                          query-parameters
                          derivation-outputs
                          build-server-urls
                          systems
                          (valid-targets->options targets)
                          show-next-page?
                          #:path-base path-base
                          #:header-text header-text
                          #:header-link header-link))))))))))

(define* (render-revision-builds mime-types
                                 commit-hash
                                 query-parameters
                                 #:key
                                 (path-base "/revision/")
                                 (header-text
                                  `("Revision " (samp ,commit-hash)))
                                 (header-link
                                  (string-append "/revision/" commit-hash)))
  (if (any-invalid-query-parameters? query-parameters)
      (letpar& ((systems
                 (with-thread-postgresql-connection valid-systems))
                (targets
                 (with-thread-postgresql-connection valid-targets)))
        (render-html
         #:sxml
         (view-revision-builds query-parameters
                               commit-hash
                               build-status-strings
                               systems
                               (valid-targets->options targets)
                               '()
                               '()
                               '())))
      (let ((system (assq-ref query-parameters 'system))
            (target (assq-ref query-parameters 'target)))
        (letpar& ((systems
                   (with-thread-postgresql-connection valid-systems))
                  (targets
                   (with-thread-postgresql-connection valid-targets))
                  (build-server-options
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (map (match-lambda
                             ((id url lookup-all-derivations
                                  lookup-builds)
                              (cons url id)))
                           (select-build-servers conn)))))
                  (stats
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-build-stats
                       conn
                       (assq-ref query-parameters
                                 'build_server)
                       #:revision-commit commit-hash
                       #:system system
                       #:target target))))
                  (builds
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-builds-with-context
                       conn
                       (assq-ref query-parameters
                                 'build_status)
                       (assq-ref query-parameters
                                 'build_server)
                       #:revision-commit commit-hash
                       #:system system
                       #:target target
                       #:limit (assq-ref query-parameters
                                         'limit_results))))))
          (render-html
           #:sxml (view-revision-builds query-parameters
                                        commit-hash
                                        build-status-strings
                                        systems
                                        (valid-targets->options targets)
                                        build-server-options
                                        stats
                                        builds))))))

(define* (render-revision-lint-warnings mime-types
                                        commit-hash
                                        query-parameters
                                        #:key
                                        (path-base "/revision/")
                                        (header-text
                                         `("Revision " (samp ,commit-hash)))
                                        (header-link
                                         (string-append "/revision/" commit-hash)))
  (define lint-checker-options
    (parallel-via-thread-pool-channel
     (with-thread-postgresql-connection
      (lambda (conn)
        (map (match-lambda
               ((name description network-dependent)
                (cons (string-append name ": " description )
                      name)))
             (lint-checkers-for-revision conn commit-hash))))))

  (define lint-warnings-locale-options
    (parallel-via-thread-pool-channel
     (with-thread-postgresql-connection
      (lambda (conn)
        (map
         (match-lambda
           ((locale)
            locale))
         (lint-warning-message-locales-for-revision conn commit-hash))))))

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
                                              '()
                                              lint-checker-options
                                              lint-warnings-locale-options
                                              #t ; any-translated-lint-warnings?
                                              #:path-base path-base
                                              #:header-text header-text
                                              #:header-link header-link))))

      (let ((locale (assq-ref query-parameters 'locale))
            (package-query (assq-ref query-parameters 'package_query))
            (linters (assq-ref query-parameters 'linter))
            (message-query (assq-ref query-parameters 'message_query))
            (fields (assq-ref query-parameters 'field)))
        (letpar&
            ((git-repositories
              (with-thread-postgresql-connection
               (lambda (conn)
                 (git-repositories-containing-commit conn
                                                     commit-hash))))
             (lint-warnings
              (with-thread-postgresql-connection
               (lambda (conn)
                 (lint-warnings-for-guix-revision conn commit-hash
                                                  #:locale locale
                                                  #:package-query package-query
                                                  #:linters linters
                                                  #:message-query message-query)))))
          (let ((any-translated-lint-warnings?
                 (any-translated-lint-warnings? lint-warnings locale)))
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
                                   lint-checker-description-locale
                                   lint-checker-network-dependent
                                   package-name package-version
                                   file line-number column-number
                                   message message-locale)
                               `((package . ((name    . ,package-name)
                                             (version . ,package-version)))
                                 ,@(if (member "message" fields)
                                       `((message . ,message)
                                         (message-locale . ,message-locale))
                                       '())
                                 ,@(if (member "linter" fields)
                                       `((lint-checker-description . ,lint-checker-description)
                                         (lint-checker-description-locale . ,lint-checker-description-locale))
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
                                                    lint-warnings-locale-options
                                                    any-translated-lint-warnings?
                                                    #:path-base path-base
                                                    #:header-text header-text
                                                    #:header-link header-link)
                #:extra-headers http-headers-for-unchanging-content))))))))
