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

(define-module (guix-data-service web revision html)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service web view html)
  #:export (view-revision-package
            view-revision-package-and-version
            view-revision
            view-revision-packages
            view-revision-lint-warnings))

(define* (view-revision-package revision-commit-hash
                                name
                                versions
                                git-repositories-and-branches
                                #:key path-base
                                header-text
                                header-link)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h3 (a (@ (href ,header-link))
               ,@header-text))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        ,(append-map
          (match-lambda
            (((id label url cgit-url-base) . branches)
             (map (match-lambda
                    ((branch-name datetime)
                     `(a (@ (class "btn btn-default btn-lg pull-right")
                            (href ,(simple-format
                                    #f "/repository/~A/branch/~A/package/~A"
                                    id branch-name name)))
                         ,(simple-format #f "View ~A branch version history"
                                         branch-name))))
                  branches)))
          git-repositories-and-branches)
        (h1 "Package " ,name)))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h3 "Versions")
        (table
         (@ (class "table"))
         (thead
          (tr
           (th (@ (class "col-sm-10")) "Version")
           (th (@ (class "col-sm-2")) "")))
         (tbody
          ,@(map
             (lambda (version)
               `(tr
                 (td (samp ,version))
                 (td
                  (a (@ (href ,(string-append
                                path-base
                                revision-commit-hash
                                "/package/" name "/" version)))
                     "More information"))))
             versions)))))))))

(define* (view-revision-package-and-version revision-commit-hash name version
                                            package-metadata
                                            derivations git-repositories
                                            lint-warnings
                                            #:key header-text
                                            header-link)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h3 (a (@ (href ,header-link))
               ,@header-text))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 "Package " ,name " @ " ,version)))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        ,(match package-metadata
           (((synopsis description home-page file line column-number
                       licenses))
            `(dl
              (@ (class "dl-horizontal"))
              (dt "Synopsis")
              (dd ,(stexi->shtml (texi-fragment->stexi synopsis)))
              (dt "Description")
              (dd ,(stexi->shtml (texi-fragment->stexi description)))
              (dt "Home page")
              (dd (a (@ (href ,home-page)) ,home-page))
              ,@(if (and file (not (string-null? file))
                         (not (null? git-repositories)))
                    `((dt "Location")
                      (dd ,@(map
                             (match-lambda
                               ((id label url cgit-url-base)
                                (if
                                 (and cgit-url-base
                                      (not (string-null? cgit-url-base)))
                                 `(a (@ (href
                                         ,(string-append
                                           cgit-url-base "tree/"
                                           file "?id=" revision-commit-hash
                                           "#n" line)))
                                     ,file
                                     " (line: " ,line
                                     ", column: " ,column-number ")")
                                 '())))
                             git-repositories)))
                    '())
              ,@(if (> (vector-length licenses) 0)
                    `((dt ,(if (eq? (vector-length licenses) 1)
                               "License"
                               "Licenses"))
                      (dd (ul
                           ,@(map (lambda (license)
                                    `(li (a (@ (href ,(assoc-ref license "uri")))
                                            ,(assoc-ref license "name"))))
                                  (vector->list licenses)))))
                    '()))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h3 "Derivations")
        (table
         (@ (class "table"))
         (thead
          (tr
           (th "System")
           (th "Target")
           (th "Derivation")
           (th "Build status")))
         (tbody
          ,@(map
             (match-lambda
               ((system target file-name status)
                `(tr
                  (td (samp ,system))
                  (td (samp ,target))
                  (td (a (@ (href ,file-name))
                         ,(display-store-item-short file-name)))
                  (td ,(build-status-span status)))))
             derivations)))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h3 "Lint warnings")
        (table
         (@ (class "table"))
         (thead
          (tr
           (th "Linter")
           (th "Message")
           (th "Location")))
         (tbody
          ,@(map
             (match-lambda
               ((id lint-checker-name lint-checker-description
                    lint-checker-network-dependent
                    file line-number column-number
                    message)
                `(tr
                  (td (span (@ (style "font-family: monospace; display: block;"))
                            ,lint-checker-name)
                      (p (@ (style "font-size: small; margin: 6px 0 0px;"))
                         ,lint-checker-description))
                  (td ,message)
                  (td
                   ,@(if (and file (not (string-null? file)))
                         `((ul
                            ,@(map
                               (match-lambda
                                 ((id label url cgit-url-base)
                                  (let ((output
                                         `(,file
                                           " "
                                           (span
                                            (@ (style "white-space: nowrap"))
                                            "(line: " ,line-number
                                            ", column: " ,column-number ")"))))
                                    (if
                                     (and cgit-url-base
                                          (not (string-null? cgit-url-base)))
                                     `(li
                                       (a (@ (href
                                              ,(string-append
                                                cgit-url-base "tree/"
                                                file "?id=" revision-commit-hash
                                                "#n" line-number)))
                                          ,@output))
                                     `(li ,@output)))))
                               git-repositories)))
                         '())))))
             lint-warnings)))))))))

(define (view-revision/git-repositories git-repositories-and-branches
                                         commit-hash)
  `((h3 "Git repositories")
    ,@(map
       (match-lambda
         (((id label url cgit-url-base) . branches)
          `((a (@ (href ,(string-append
                          "/repository/" id)))
               (h4 ,url))
            ,@(map
               (match-lambda
                 ((name datetime)
                  `(div
                    (a (@ (href ,(string-append "/repository/" id
                                                "/branch/" name)))
                       ,name)
                    " at " ,datetime
                    ,@(if (string-null? cgit-url-base)
                          '()
                          `(" "
                            (a (@ (href ,(string-append
                                          cgit-url-base
                                          "commit/?id="
                                          commit-hash)))
                               "(View cgit)"))))))
               branches))))
       git-repositories-and-branches)))

(define (view-revision/jobs-and-events jobs-and-events)
  `((h3 "Jobs")
    (table
     (@ (class "table"))
     (thead
      (tr
       (th "Source")
       (th "Events")
       (th "")))
     (tbody
      ,@(map (match-lambda
               ((id commit source git-repository-id created-at succeeded-at
                    events log-exists?)
                `(tr
                  (@ (class
                       ,(let ((event-names
                               (map (lambda (event)
                                      (assoc-ref event "event"))
                                    (vector->list events))))
                          (cond
                           ((member "success" event-names)
                            "success")
                           ((member "failure" event-names)
                            "danger")
                           ((member "start" event-names)
                            "info")
                           (else
                            ""))))
                     (title ,(simple-format #f "Job id: ~A" id)))
                  (td ,source)
                  (td
                   (dl
                    ,@(map
                       (lambda (event)
                         `((dt ,(assoc-ref event "event"))
                           (dd ,(assoc-ref event "occurred_at"))))
                       (cons
                        `(("event" . "created")
                          ("occurred_at" . ,created-at))
                        (vector->list events)))))
                  (td
                   ,@(if log-exists?
                         `((a (@ (href ,(string-append "/job/" id)))
                              "View log"))
                         '())))))
             jobs-and-events)))))

(define (view-revision/lint-warning-counts path-base lint-warning-counts)
  `((h3 "Lint warnings")
    (a (@ (href ,(string-append path-base "/lint-warnings")))
       "View lint warnings")
    (table
     (@ (class "table"))
     (thead
      (tr
       (th "Linter")
       (th "Count")))
     (tbody
      ,@(map (match-lambda
               ((name description network-dependent count)
                `(tr
                  (td (span (@ (style "font-family: monospace; display: block;"))
                            ,name)
                      (p (@ (style "margin: 6px 0 0px;"))
                         ,description))
                  (td ,count))))
             lint-warning-counts)))))

(define* (view-revision commit-hash packages-count
                        git-repositories-and-branches derivations-count
                        jobs-and-events
                        lint-warning-counts
                        #:key (path-base "/revision/")
                        header-text)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (h1 (@ (style "white-space: nowrap;"))
            ,@header-text)))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-6"))
        (h2 "Packages")
        (strong (@ (class "text-center")
                   (style "font-size: 2em; display: block;"))
                ,packages-count)
        (a (@ (href ,(string-append path-base "/packages")))
           "View packages")

        ,@(if (null? git-repositories-and-branches)
              '()
              (view-revision/git-repositories git-repositories-and-branches
                                              commit-hash))
        ,@(view-revision/jobs-and-events jobs-and-events)
        ,@(view-revision/lint-warning-counts path-base
                                             lint-warning-counts))
       (div
        (@ (class "col-md-6"))
        (h3 "Derivations")
        (table
         (@ (class "table")
            (style "white-space: nowrap;"))
         (thead
          (tr
           (th "System")
           (th "Target")
           (th "Derivations")))
         (tbody
          ,@(map (match-lambda
                   ((system target count)
                    (if (string=? system target)
                        `(tr
                          (td (@ (class "text-center")
                                 (colspan 2))
                              (samp ,system))
                          (td (samp ,count)))
                        `(tr
                          (td (samp ,system))
                          (td (samp ,target))
                          (td (samp ,count))))))
                 derivations-count)))))))))

(define* (view-revision-packages revision-commit-hash
                                 query-parameters
                                 packages
                                 git-repositories
                                 show-next-page?
                                 #:key path-base
                                 header-text header-link)
  (define field-options
    (map
     (lambda (field)
       (cons field
             (hyphenate-words
              (string-downcase field))))
     '("Version" "Synopsis" "Description"
       "Home page" "Location" "Licenses")))

  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h3 (a (@ (style "white-space: nowrap;")
                  (href ,header-link))
               ,@header-text))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (div
         (@ (class "well"))
         (form
          (@ (method "get")
             (action "")
             (style "padding-bottom: 0")
             (class "form-horizontal"))
          ,(form-horizontal-control
            "Search query" query-parameters
            #:help-text
            "List packages where the name or synopsis match the query.")
          ,(form-horizontal-control
            "Fields" query-parameters
            #:name "field"
            #:options field-options
            #:help-text "Fields to return in the response.")
          ,(form-horizontal-control
            "After name" query-parameters
            #:help-text
            "List packages that are alphabetically after the given name.")
          ,(form-horizontal-control
            "Limit results" query-parameters
            #:help-text "The maximum number of packages by name to return.")
          ,(form-horizontal-control
            "All results" query-parameters
            #:type "checkbox"
            #:help-text "Return all results.")
          (div (@ (class "form-group form-group-lg"))
               (div (@ (class "col-sm-offset-2 col-sm-10"))
                    (button (@ (type "submit")
                               (class "btn btn-lg btn-primary"))
                            "Update results")))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (a (@ (class "btn btn-default btn-lg pull-right")
              (href ,(let ((query-parameter-string
                            (query-parameters->string query-parameters)))
                       (string-append
                        path-base ".json"
                        (if (string-null? query-parameter-string)
                            ""
                            (string-append "?" query-parameter-string))))))
           "View JSON")))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 "Packages")
        (table
         (@ (class "table table-responsive"))
         (thead
          (tr
           (th (@ (class "col-md-3")) "Name")
           ,@(filter-map
              (match-lambda
                ((label . value)
                 (if (member value (assq-ref query-parameters 'field))
                     `(th (@ (class "col-md-3")) ,label)
                     #f)))
              field-options)
           (th (@ (class "col-md-3")) "")))
         (tbody
          ,@(let ((fields (assq-ref query-parameters 'field)))
              (map
               (match-lambda
                 ((name version synopsis description home-page
                        location-file location-line
                        location-column-number licenses)
                  `(tr
                    (td ,name)
                    ,@(if (member "version" fields)
                          `((td ,version))
                          '())
                    ,(if (member "synopsis" fields)
                         `((td ,(stexi->shtml (texi-fragment->stexi synopsis))))
                         '())
                    ,(if (member "description" fields)
                         `((td ,(stexi->shtml (texi-fragment->stexi description))))
                         '())
                    ,(if (member "home-page" fields)
                         `((td ,home-page))
                         '())
                    ,(if (member "location" fields)
                         `((td
                            ,@(if (and location-file
                                       (not (string-null? location-file)))
                                  `((ul
                                     ,@(map
                                        (match-lambda
                                          ((id label url cgit-url-base)
                                           (if
                                            (and cgit-url-base
                                                 (not (string-null? cgit-url-base)))
                                            `(li
                                              (a (@ (href
                                                     ,(string-append
                                                       cgit-url-base "tree/"
                                                       location-file "?id=" revision-commit-hash
                                                       "#n" location-line)))
                                                 ,location-file
                                                 " (line: " ,location-line
                                                 ", column: " ,location-column-number ")"))
                                            `(li ,location-file
                                                 " (line: " ,location-line
                                                 ", column: " ,location-column-number ")"))))
                                        git-repositories)))
                                  '())))
                         '())
                    ,(if (member "licenses" fields)
                         `((td
                            (ul
                             (@ (class "list-inline"))
                             ,@(map (lambda (license)
                                      `(li (a (@ (href ,(assoc-ref license "uri")))
                                              ,(assoc-ref license "name"))))
                                    (vector->list
                                     (json-string->scm licenses))))))
                         '())
                    (td (@ (class "text-right"))
                        (a (@ (href ,(string-append
                                      (string-drop-right path-base 1)
                                      "/" name "/" version)))
                           "More information")))))
               packages))))))
      ,@(if show-next-page?
            `((div
               (@ (class "row"))
               (a (@ (href ,(string-append path-base
                                           "?after_name="
                                           (car (last packages)))))
                  "Next page")))
            '())))))

(define* (view-revision-lint-warnings revision-commit-hash
                                      query-parameters
                                      lint-warnings
                                      git-repositories
                                      lint-checker-options
                                      #:key path-base
                                      header-text header-link)
  (define field-options
    (map
     (lambda (field)
       (cons field
             (hyphenate-words
              (string-downcase field))))
     '("Linter" "Message" "Location")))

  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h3 (a (@ (style "white-space: nowrap;")
                  (href ,header-link))
               ,@header-text))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (div
         (@ (class "well"))
         (form
          (@ (method "get")
             (action "")
             (style "padding-bottom: 0")
             (class "form-horizontal"))
          ,(form-horizontal-control
            "Package query" query-parameters
            #:help-text
            "Lint warnings where the package name matches the query.")
          ,(form-horizontal-control
            "Linter" query-parameters
            #:options lint-checker-options
            #:help-text
            "Lint warnings for specific lint checkers.")
          ,(form-horizontal-control
            "Message query" query-parameters
            #:help-text
            "Lint warnings where the message matches the query.")
          ,(form-horizontal-control
            "Fields" query-parameters
            #:name "field"
            #:options field-options
            #:help-text "Fields to return in the response.")
          (div (@ (class "form-group form-group-lg"))
               (div (@ (class "col-sm-offset-2 col-sm-10"))
                    (button (@ (type "submit")
                               (class "btn btn-lg btn-primary"))
                            "Update results")))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (a (@ (class "btn btn-default btn-lg pull-right")
              (href ,(let ((query-parameter-string
                            (query-parameters->string query-parameters)))
                       (string-append
                        path-base ".json"
                        (if (string-null? query-parameter-string)
                            ""
                            (string-append "?" query-parameter-string))))))
           "View JSON")))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 "Lint warnings")
        (table
         (@ (class "table table-responsive"))
         (thead
          (tr
           (th (@ (class "col-md-3")) "Package")
           ,@(filter-map
              (match-lambda
                ((label . value)
                 (if (member value (assq-ref query-parameters 'field))
                     `(th (@ (class "col-md-3")) ,label)
                     #f)))
              field-options)
           (th (@ (class "col-md-3")) "")))
         (tbody
          ,@(let ((fields (assq-ref query-parameters 'field)))
              (map
               (match-lambda
                 ((id lint-checker-name lint-checker-description
                      lint-checker-network-dependent
                      package-name package-version file line-number column-number
                      message)
                  `(tr
                    (td (a (@ (href ,(string-append
                                      (string-join
                                       (drop-right (string-split path-base #\/) 1)
                                       "/")
                                      "/package/" package-name "/" package-version)))
                           ,package-name " @ " ,package-version))
                    ,@(if (member "linter" fields)
                          `((td (span (@ (style "font-family: monospace; display: block;"))
                                      ,lint-checker-name)
                                (p (@ (style "font-size: small; margin: 6px 0 0px;"))
                                   ,lint-checker-description)))
                          '())
                    ,@(if (member "message" fields)
                          `((td ,message))
                          '())
                    ,@(if (member "location" fields)
                          `((td
                             ,@(if (and file (not (string-null? file)))
                                   `((ul
                                      ,@(map
                                         (match-lambda
                                           ((id label url cgit-url-base)
                                            (let ((output
                                                   `(,file
                                                     " "
                                                     (span
                                                      (@ (style "white-space: nowrap"))
                                                      "(line: " ,line-number
                                                      ", column: " ,column-number ")"))))
                                              (if
                                               (and cgit-url-base
                                                    (not (string-null? cgit-url-base)))
                                               `(li
                                                 (a (@ (href
                                                        ,(string-append
                                                          cgit-url-base "tree/"
                                                          file "?id=" revision-commit-hash
                                                          "#n" line-number)))
                                                    ,@output))
                                               `(li ,@output)))))
                                         git-repositories)))
                                   '())))
                          '()))))
               lint-warnings))))))))))
