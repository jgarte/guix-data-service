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

(define-module (guix-data-service web repository controller)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service model build-server)
  #:use-module (guix-data-service model derivation)
  #:use-module (guix-data-service model package)
  #:use-module (guix-data-service model git-branch)
  #:use-module (guix-data-service model git-repository)
  #:use-module (guix-data-service web view html)
  #:use-module (guix-data-service web revision controller)
  #:use-module (guix-data-service web repository html)
  #:export (repository-controller))

(define (repository-controller request
                               method-and-path-components
                               mime-types
                               body
                               conn)
  (define path
    (uri-path (request-uri request)))

  (match method-and-path-components
    (('GET "repositories")
     (let ((git-repositories (all-git-repositories conn)))
       (render-html
        #:sxml
        (view-git-repositories git-repositories))))
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
       (let ((revisions
              (most-recent-commits-for-branch
               conn
               (string->number repository-id)
               branch-name
               #:limit (assq-ref parsed-query-parameters 'limit_results)
               #:after-date (assq-ref parsed-query-parameters
                                      'after_date)
               #:before-date (assq-ref parsed-query-parameters
                                       'before_date))))
         (case (most-appropriate-mime-type
                '(application/json text/html)
                mime-types)
           ((application/json)
            (render-json
             `((revisions
                . ,(list->vector
                    (map (match-lambda
                           ((commit-hash date _ _)
                            `((date . ,date)
                              (commit-hash . ,commit-hash))))
                         revisions))))))
           (else
            (render-html
             #:sxml (if (any-invalid-query-parameters? parsed-query-parameters)
                        (view-branch repository-id
                                     branch-name parsed-query-parameters '())
                        (view-branch
                         repository-id
                         branch-name
                         parsed-query-parameters
                         revisions))))))))
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
    (('GET "repository" repository-id "branch" branch-name "package" package-name "derivation-history")
     (render-branch-package-derivation-history request
                                               mime-types
                                               conn
                                               repository-id
                                               branch-name
                                               package-name))
    (('GET "repository" repository-id "branch" branch-name
           "package" package-name "output-history")
     (render-branch-package-output-history request
                                           mime-types
                                           conn
                                           repository-id
                                           branch-name
                                           package-name))
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
                   `((locale         ,identity #:default "en_US.utf8")
                     (package_query  ,identity)
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
                                             "/latest-processed-revision")
                                            #:version-history-link
                                            (string-append
                                             "/repository/" repository-id
                                             "/branch/" branch-name
                                             "/package/" name))
           (render-unknown-revision mime-types
                                    conn
                                    commit-hash))))
    (_ #f)))

(define (parse-build-system conn)
  (let ((systems
         (valid-systems conn)))
    (lambda (s)
      (if (member s systems)
          s
          (make-invalid-query-parameter
           s "unknown system")))))

(define (render-branch-package-derivation-history request
                                                  mime-types
                                                  conn
                                                  repository-id
                                                  branch-name
                                                  package-name)
  (let ((parsed-query-parameters
         (parse-query-parameters
          request
          `((system  ,(parse-build-system conn)
                     #:default "x86_64-linux")
            (target  ,parse-target
                     #:default "")))))
    (let* ((system
            (assq-ref parsed-query-parameters 'system))
           (target
            (assq-ref parsed-query-parameters 'target))
           (package-derivations
            (package-derivations-for-branch conn
                                            (string->number repository-id)
                                            branch-name
                                            system
                                            target
                                            package-name))
           (build-server-urls
           (group-to-alist
            (match-lambda
              ((id url lookup-all-derivations)
               (cons id url)))
            (select-build-servers conn))))
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          `((derivations . ,(list->vector
                             (map (match-lambda
                                    ((package-version derivation-file-name
                                                      first-guix-revision-commit
                                                      first-datetime
                                                      last-guix-revision-commit
                                                      last-datetime)
                                     `((version . ,package-version)
                                       (derivation . ,derivation-file-name)
                                       (first_revision
                                        . ((commit . ,first-guix-revision-commit)
                                           (datetime . ,first-datetime)))
                                       (last_revision
                                        . ((commit . ,last-guix-revision-commit)
                                           (datetime . ,last-datetime))))))
                                  package-derivations))))))
        (else
         (render-html
          #:sxml (view-branch-package-derivations
                  parsed-query-parameters
                  repository-id
                  branch-name
                  package-name
                  (valid-systems conn)
                  (valid-targets->options
                   (valid-targets conn))
                  build-server-urls
                  package-derivations)))))))

(define (render-branch-package-output-history request
                                              mime-types
                                              conn
                                              repository-id
                                              branch-name
                                              package-name)
  (let ((parsed-query-parameters
         (parse-query-parameters
          request
          `((output  ,identity
                     #:default "out")
            (system  ,(parse-build-system conn)
                     #:default "x86_64-linux")
            (target  ,parse-target
                     #:default "")))))
    (let* ((system
            (assq-ref parsed-query-parameters 'system))
           (target
            (assq-ref parsed-query-parameters 'target))
           (output-name
            (assq-ref parsed-query-parameters 'output))
           (package-outputs
            (package-outputs-for-branch conn
                                        (string->number repository-id)
                                        branch-name
                                        system
                                        target
                                        package-name
                                        output-name))
           (build-server-urls
            (group-to-alist
             (match-lambda
               ((id url lookup-all-derivations)
                (cons id url)))
             (select-build-servers conn))))
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          `((derivations . ,(list->vector
                             (map (match-lambda
                                    ((package-version derivation-file-name
                                                      first-guix-revision-commit
                                                      first-datetime
                                                      last-guix-revision-commit
                                                      last-datetime)
                                     `((version . ,package-version)
                                       (derivation . ,derivation-file-name)
                                       (first_revision
                                        . ((commit . ,first-guix-revision-commit)
                                           (datetime . ,first-datetime)))
                                       (last_revision
                                        . ((commit . ,last-guix-revision-commit)
                                           (datetime . ,last-datetime))))))
                                  package-outputs))))))
        (else
         (render-html
          #:sxml (view-branch-package-outputs
                  parsed-query-parameters
                  repository-id
                  branch-name
                  package-name
                  output-name
                  (valid-systems conn)
                  (valid-targets->options
                   (valid-targets conn))
                  build-server-urls
                  package-outputs)))))))
