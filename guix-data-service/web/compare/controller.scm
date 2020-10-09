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

(define-module (guix-data-service web compare controller)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:use-module (texinfo plain-text)
  #:use-module (guix-data-service utils)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service web sxml)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service comparison)
  #:use-module (guix-data-service jobs load-new-guix-revision)
  #:use-module (guix-data-service model guix-revision)
  #:use-module (guix-data-service model derivation)
  #:use-module (guix-data-service model build-status)
  #:use-module (guix-data-service model lint-warning-message)
  #:use-module (guix-data-service web compare html)
  #:export (compare-controller))

(define cache-control-default-max-age
  (* 60 60 24)) ; One day

(define http-headers-for-unchanging-content
  `((cache-control
     . (public
        (max-age . ,cache-control-default-max-age)))))

(define (parse-build-status s)
  s)

(define (parse-commit s)
  (if (parallel-via-thread-pool-channel
       (with-thread-postgresql-connection
        (lambda (conn)
          (guix-commit-exists? conn s))))
      s
      (make-invalid-query-parameter
       s "unknown commit")))

(define (parse-derivation file-name)
  (if (parallel-via-thread-pool-channel
       (with-thread-postgresql-connection
        (lambda (conn)
          (select-derivation-by-file-name conn file-name))))
      file-name
      (make-invalid-query-parameter
       file-name "unknown derivation")))

(define (compare-controller request
                            method-and-path-components
                            mime-types
                            body)
  (match method-and-path-components
    (('GET "compare")
     (let* ((parsed-query-parameters
             (parse-query-parameters
              request
              `((base_commit   ,parse-commit #:required)
                (target_commit ,parse-commit #:required)
                (locale        ,identity #:default "en_US.UTF-8")))))
       (render-compare mime-types
                       parsed-query-parameters)))
    (('GET "compare-by-datetime")
     (let* ((parsed-query-parameters
             (parse-query-parameters
              request
              `((base_branch     ,identity #:required)
                (base_datetime   ,parse-datetime #:required)
                (target_branch   ,identity #:required)
                (target_datetime ,parse-datetime #:required)
                (locale          ,identity #:default "en_US.UTF-8")))))
       (render-compare-by-datetime mime-types
                                   parsed-query-parameters)))
    (('GET "compare" "derivation")
     (let* ((parsed-query-parameters
             (parse-query-parameters
              request
              `((base_derivation   ,parse-derivation #:required)
                (target_derivation ,parse-derivation #:required)))))
       (render-compare/derivation mime-types
                                  parsed-query-parameters)))
    (('GET "compare" "package-derivations")
     (let* ((parsed-query-parameters
             (parse-query-parameters
              request
              `((base_commit   ,parse-commit #:required)
                (target_commit ,parse-commit #:required)
                (system        ,parse-system #:multi-value)
                (target        ,parse-target #:multi-value)
                (build_status  ,parse-build-status #:multi-value)))))
       (render-compare/package-derivations mime-types
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
                 (target          ,parse-target #:multi-value)
                 (build_status    ,parse-build-status #:multi-value)))
              '((base_commit base_datetime)
                (target_commit target_datetime)))))
       (render-compare-by-datetime/derivations mime-types
                                               parsed-query-parameters)))
    (('GET "compare" "packages")
     (let* ((parsed-query-parameters
             (parse-query-parameters
              request
              `((base_commit   ,parse-commit #:required)
                (target_commit ,parse-commit #:required)))))
       (render-compare/packages mime-types
                               parsed-query-parameters)))
    (_ #f)))

(define (texinfo->variants-alist s)
  (let ((stexi (texi-fragment->stexi s)))
    `((source . ,s)
      (html   . ,(with-output-to-string
                   (lambda ()
                     (sxml->html (stexi->shtml stexi)))))
      (plain . ,(stexi->plain-text stexi)))))

(define (render-compare mime-types
                        query-parameters)
  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          '((error . "invalid query"))))
        (else
         (letpar& ((base-job
                    (match (assq-ref query-parameters 'base_commit)
                      (($ <invalid-query-parameter> value)
                       (with-thread-postgresql-connection
                        (lambda (conn)
                          (select-job-for-commit conn value))))
                      (_ #f)))
                   (target-job
                    (match (assq-ref query-parameters 'target_commit)
                      (($ <invalid-query-parameter> value)
                       (with-thread-postgresql-connection
                        (lambda (conn)
                          (select-job-for-commit conn value))))
                      (_ #f))))
           (render-html
            #:sxml (compare-invalid-parameters
                    query-parameters
                    base-job
                    target-job)))))

      (letpar& ((base-revision-id
                 (with-thread-postgresql-connection
                  (lambda (conn)
                    (commit->revision-id
                     conn
                     (assq-ref query-parameters 'base_commit)))))
                (target-revision-id
                 (with-thread-postgresql-connection
                  (lambda (conn)
                    (commit->revision-id
                     conn
                     (assq-ref query-parameters 'target_commit)))))
                (locale
                 (assq-ref query-parameters 'locale)))
        (let-values
            (((base-packages-vhash target-packages-vhash)
              (package-data->package-data-vhashes
               (parallel-via-thread-pool-channel
                (with-thread-postgresql-connection
                 (lambda (conn)
                   (package-differences-data conn
                                             base-revision-id
                                             target-revision-id)))))))
          (let ((new-packages
                 (package-data-vhashes->new-packages base-packages-vhash
                                                     target-packages-vhash))
                (removed-packages
                 (package-data-vhashes->removed-packages base-packages-vhash
                                                         target-packages-vhash))
                (version-changes
                 (package-data-version-changes base-packages-vhash
                                               target-packages-vhash)))
            (letpar& ((lint-warnings-data
                       (with-thread-postgresql-connection
                        (lambda (conn)
                          (group-list-by-first-n-fields
                           2
                           (lint-warning-differences-data conn
                                                          base-revision-id
                                                          target-revision-id
                                                          locale)))))
                      (channel-news-data
                       (with-thread-postgresql-connection
                        (lambda (conn)
                          (channel-news-differences-data conn
                                                         base-revision-id
                                                         target-revision-id)))))
              (case (most-appropriate-mime-type
                     '(application/json text/html)
                     mime-types)
                ((application/json)
                 (render-json
                  `((base-commit    . ,(assq-ref query-parameters 'base_commit))
                    (target-commit  . ,(assq-ref query-parameters 'target_commit))
                    (channel-news . ,(list->vector
                                      (map
                                       (match-lambda
                                         ((commit tag title_text body_text change)
                                          `(,@(if (null? commit)
                                                  '()
                                                  `((commit . ,commit)))
                                            ,@(if (null? tag)
                                                  '()
                                                  `((tag . ,tag)))
                                            (title-text
                                             . ,(map
                                                 (match-lambda
                                                   ((lang . text)
                                                    (cons
                                                     lang
                                                     (texinfo->variants-alist text))))
                                                 title_text))
                                            (body-text
                                             . ,(map
                                                 (match-lambda
                                                   ((lang . text)
                                                    (cons
                                                     lang
                                                     (texinfo->variants-alist text))))
                                                 body_text))
                                            (change . ,change))))
                                       channel-news-data)))
                    (new-packages . ,(list->vector new-packages))
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
                 (letpar& ((lint-warnings-locale-options
                            (map
                             (match-lambda
                               ((locale)
                                locale))
                             (with-thread-postgresql-connection
                              (lambda (conn)
                                (lint-warning-message-locales-for-revision
                                 conn
                                 (assq-ref query-parameters 'target_commit))))))
                           (cgit-url-bases
                            (with-thread-postgresql-connection
                             (lambda (conn)
                               (guix-revisions-cgit-url-bases
                                conn
                                (list base-revision-id
                                      target-revision-id))))))
                   (render-html
                    #:sxml (compare query-parameters
                                    cgit-url-bases
                                    new-packages
                                    removed-packages
                                    version-changes
                                    lint-warnings-data
                                    lint-warnings-locale-options
                                    channel-news-data)
                    #:extra-headers http-headers-for-unchanging-content))))))))))

(define (render-compare-by-datetime mime-types
                                    query-parameters)
  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          '((error . "invalid query"))))
        (else
         (letpar& ((base-job
                    (match (assq-ref query-parameters 'base_commit)
                      (($ <invalid-query-parameter> value)
                       (with-thread-postgresql-connection
                        (lambda (conn)
                          (select-job-for-commit conn value))))
                      (_ #f)))
                   (target-job
                    (match (assq-ref query-parameters 'target_commit)
                      (($ <invalid-query-parameter> value)
                       (with-thread-postgresql-connection
                        (lambda (conn)
                          (select-job-for-commit conn value))))
                      (_ #f))))
           (render-html
            #:sxml (compare-invalid-parameters
                    query-parameters
                    base-job
                    target-job)))))

      (let ((base-branch     (assq-ref query-parameters 'base_branch))
            (base-datetime   (assq-ref query-parameters 'base_datetime))
            (target-branch   (assq-ref query-parameters 'target_branch))
            (target-datetime (assq-ref query-parameters 'target_datetime))
            (locale          (assq-ref query-parameters 'locale)))
        (letpar& ((base-revision-details
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-guix-revision-for-branch-and-datetime
                       conn
                       base-branch
                       base-datetime))))
                  (target-revision-details
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-guix-revision-for-branch-and-datetime
                       conn
                       target-branch
                       target-datetime)))))
          (letpar& ((lint-warnings-locale-options
                     (map
                      (match-lambda
                        ((locale)
                         locale))
                      (with-thread-postgresql-connection
                       (lambda (conn)
                         (lint-warning-message-locales-for-revision
                          conn
                          (second base-revision-details)))))))
            (let ((base-revision-id
                   (first base-revision-details))
                  (target-revision-id
                   (first target-revision-details)))
              (let-values
                  (((base-packages-vhash target-packages-vhash)
                    (package-data->package-data-vhashes
                     (parallel-via-thread-pool-channel
                      (with-thread-postgresql-connection
                       (lambda (conn)
                         (package-differences-data conn
                                                   base-revision-id
                                                   target-revision-id)))))))
                (let* ((new-packages
                        (package-data-vhashes->new-packages base-packages-vhash
                                                            target-packages-vhash))
                       (removed-packages
                        (package-data-vhashes->removed-packages base-packages-vhash
                                                                target-packages-vhash))
                       (version-changes
                        (package-data-version-changes base-packages-vhash
                                                      target-packages-vhash))
                       (channel-news-data
                        (parallel-via-thread-pool-channel
                         (with-thread-postgresql-connection
                          (lambda (conn)
                            (channel-news-differences-data conn
                                                           base-revision-id
                                                           target-revision-id))))))
                  (case (most-appropriate-mime-type
                         '(application/json text/html)
                         mime-types)
                    ((application/json)
                     (render-json
                      `((revisions
                         . ((base
                             . ((commit . ,(second base-revision-details))
                                (datetime . ,(fifth base-revision-details))))
                            (target
                             . ((commit . ,(second target-revision-details))
                                (datetime . ,(fifth target-revision-details))))))
                        (channel-news . ,(list->vector
                                          (map
                                           (match-lambda
                                             ((commit tag title_text body_text change)
                                              `(,@(if (null? commit)
                                                      '()
                                                      `((commit . ,commit)))
                                                ,@(if (null? tag)
                                                      '()
                                                      `((tag . ,tag)))
                                                (title-text
                                                 . ,(map
                                                     (match-lambda
                                                       ((lang . text)
                                                        (cons
                                                         lang
                                                         (texinfo->variants-alist text))))
                                                     title_text))
                                                (body-text
                                                 . ,(map
                                                     (match-lambda
                                                       ((lang . text)
                                                        (cons
                                                         lang
                                                         (texinfo->variants-alist text))))
                                                     body_text))
                                                (change . ,change))))
                                           channel-news-data)))
                        (new-packages . ,(list->vector new-packages))
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
                                      (parallel-via-thread-pool-channel
                                       (with-thread-postgresql-connection
                                        (lambda (conn)
                                          (guix-revisions-cgit-url-bases
                                           conn
                                           (list base-revision-id
                                                 target-revision-id)))))
                                      new-packages
                                      removed-packages
                                      version-changes
                                      (parallel-via-thread-pool-channel
                                       (group-list-by-first-n-fields
                                        2
                                        (with-thread-postgresql-connection
                                         (lambda (conn)
                                           (lint-warning-differences-data
                                            conn
                                            base-revision-id
                                            target-revision-id
                                            locale)))))
                                      lint-warnings-locale-options
                                      channel-news-data)
                      #:extra-headers http-headers-for-unchanging-content)))))))))))

(define (render-compare/derivation mime-types
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
          #:sxml (compare/derivation
                  query-parameters
                  '()))))

      (let ((base-derivation    (assq-ref query-parameters 'base_derivation))
            (target-derivation  (assq-ref query-parameters 'target_derivation)))
        (letpar& ((data
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (derivation-differences-data conn
                                                   base-derivation
                                                   target-derivation)))))
          (case (most-appropriate-mime-type
                 '(application/json text/html)
                 mime-types)
            ((application/json)
             (render-json
              '((error . "unimplemented")) ; TODO
              #:extra-headers http-headers-for-unchanging-content))
            (else
             (render-html
              #:sxml (compare/derivation
                      query-parameters
                      data)
              #:extra-headers http-headers-for-unchanging-content)))))))

(define (render-compare/package-derivations mime-types
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
         (letpar& ((systems
                    (with-thread-postgresql-connection
                     valid-systems))
                   (targets
                    (with-thread-postgresql-connection
                     valid-targets)))
         (render-html
          #:sxml (compare/derivations
                  query-parameters
                  systems
                  (valid-targets->options targets)
                  build-status-strings
                  '())))))

      (let ((base-commit    (assq-ref query-parameters 'base_commit))
            (target-commit  (assq-ref query-parameters 'target_commit))
            (systems        (assq-ref query-parameters 'system))
            (targets        (assq-ref query-parameters 'target))
            (build-statuses (assq-ref query-parameters 'build_status)))
        (letpar& ((data
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (package-derivation-differences-data
                       conn
                       (commit->revision-id conn base-commit)
                       (commit->revision-id conn target-commit)
                       #:systems systems
                       #:targets targets)))))
          (let ((names-and-versions
                 (package-derivation-data->names-and-versions data)))
            (let-values
                (((base-packages-vhash target-packages-vhash)
                  (package-derivation-data->package-derivation-data-vhashes data)))
              (let ((derivation-changes
                     (package-derivation-data-changes names-and-versions
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
                   (letpar& ((systems
                              (with-thread-postgresql-connection
                               valid-systems))
                             (targets
                              (with-thread-postgresql-connection
                               valid-targets)))
                     (render-html
                      #:sxml (compare/derivations
                              query-parameters
                              systems
                              (valid-targets->options targets)
                              build-status-strings
                              derivation-changes)
                      #:extra-headers http-headers-for-unchanging-content)))))))))))

(define (render-compare-by-datetime/derivations mime-types
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
                  (parallel-via-thread-pool-channel
                   (with-thread-postgresql-connection valid-systems))
                  build-status-strings
                  '()
                  '()
                  '()))))

      (let ((base-branch     (assq-ref query-parameters 'base_branch))
            (base-datetime   (assq-ref query-parameters 'base_datetime))
            (target-branch   (assq-ref query-parameters 'target_branch))
            (target-datetime (assq-ref query-parameters 'target_datetime))
            (systems         (assq-ref query-parameters 'system))
            (targets         (assq-ref query-parameters 'target))
            (build-statuses  (assq-ref query-parameters 'build_status)))
        (letpar&
            ((base-revision-details
              (with-thread-postgresql-connection
               (lambda (conn)
                 (select-guix-revision-for-branch-and-datetime conn
                                                               base-branch
                                                               base-datetime))))
             (target-revision-details
              (with-thread-postgresql-connection
               (lambda (conn)
                 (select-guix-revision-for-branch-and-datetime conn
                                                               target-branch
                                                               target-datetime)))))
          (letpar&
              ((data
                (with-thread-postgresql-connection
                 (lambda (conn)
                   (package-derivation-differences-data
                    conn
                    (first base-revision-details)
                    (first target-revision-details)
                    #:systems systems
                    #:targets targets)))))
            (let ((names-and-versions
                   (package-derivation-data->names-and-versions data)))
              (let-values
                  (((base-packages-vhash target-packages-vhash)
                    (package-derivation-data->package-derivation-data-vhashes data)))
                (let ((derivation-changes
                       (package-derivation-data-changes names-and-versions
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
                              (parallel-via-thread-pool-channel
                               (with-thread-postgresql-connection valid-systems))
                              build-status-strings
                              base-revision-details
                              target-revision-details
                              derivation-changes)
                      #:extra-headers http-headers-for-unchanging-content)))))))))))

(define (render-compare/packages mime-types
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
         (letpar& ((base-job
                    (match (assq-ref query-parameters 'base_commit)
                      (($ <invalid-query-parameter> value)
                       (with-thread-postgresql-connection
                        (lambda (conn)
                          (select-job-for-commit conn value))))
                      (_ #f)))
                   (target-job
                    (match (assq-ref query-parameters 'target_commit)
                      (($ <invalid-query-parameter> value)
                       (with-thread-postgresql-connection
                        (lambda (conn)
                          (select-job-for-commit conn value))))
                      (_ #f))))
         (render-html
          #:sxml (compare-invalid-parameters
                  query-parameters
                  base-job
                  target-job)))))

      (let ((base-commit    (assq-ref query-parameters 'base_commit))
            (target-commit  (assq-ref query-parameters 'target_commit)))
        (letpar& ((base-revision-id
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (commit->revision-id
                       conn
                       base-commit))))
                  (target-revision-id
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (commit->revision-id
                       conn
                       target-commit)))))
          (let-values
              (((base-packages-vhash target-packages-vhash)
                (package-data->package-data-vhashes
                 (parallel-via-thread-pool-channel
                  (with-thread-postgresql-connection
                   (lambda (conn)
                     (package-differences-data conn
                                               base-revision-id
                                               target-revision-id)))))))
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
