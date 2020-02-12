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
  #:use-module (ice-9 format)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:use-module (json)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service web html-utils)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service web view html)
  #:export (view-revision-news
            view-revision-package
            view-revision-package-reproducibility
            view-revision-package-and-version
            view-revision
            view-revision-packages
            view-revision-derivations
            view-revision-derivation-outputs
            view-revision-system-tests
            view-revision-channel-instances
            view-revision-builds
            view-revision-lint-warnings
            unknown-revision))

(define* (view-revision-news commit-hash
                             query-parameters
                             news-entries)
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
                  (href ,(string-append "/revision/" commit-hash)))
               "Revision " (samp ,commit-hash)))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 "Channel News Entries")
        ,@(map
           (match-lambda
             ((commit tag title-text body-text)
              `(div
                (h4 ,@(if (null? commit)
                          '()
                          `(("Commit: " (samp ,commit))))
                    ,@(if (null? tag)
                          '()
                          `(("Tag: " ,tag))))
                (table
                 (@ (class "table"))
                 (thead
                  (tr
                   (th (@ (class "col-sm-1")) "Language")
                   (th (@ (class "col-sm-3")) "Title")
                   (th (@ (class "col-sm-8")) "Body"))
                  (tbody
                   ,@(map (lambda (lang)
                            `(tr
                              (td ,lang)
                              (td ,(stexi->shtml
                                    (texi-fragment->stexi
                                     (assoc-ref title-text lang))))
                              (td ,
                               (stexi->shtml
                                (texi-fragment->stexi
                                 (assoc-ref body-text lang))))))
                          (sort
                           (delete-duplicates
                            (append (map car title-text)
                                    (map car body-text)))
                           string<?))))))))
           news-entries)))))))

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
                                            header-link
                                            version-history-link)
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
        ,@(if version-history-link
              `((a (@ (class "btn btn-lg btn-default pull-right")
                      (href ,version-history-link)
                      (role "button"))
                   "Version history"))
              '())
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
                           (@ (class "list-inline"))
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
               ((system target file-name builds)
                `(tr
                  (td (samp ,system))
                  (td (samp ,target))
                  (td (a (@ (href ,file-name))
                         ,(display-store-item-short file-name)))
                  (td
                   (ul
                    (@ (class "list-inline"))
                    ,@(map (lambda (build)
                             `(li
                               (a (@ (href
                                      ,(simple-format
                                        #f "/build-server/~A/build?derivation_file_name=~A"
                                        (assoc-ref build "build_server_id")
                                        file-name)))
                                  ,(build-status-span
                                    (assoc-ref build "status")))))
                           builds))))))
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
                            (@ (class "list-unstyled"))
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
        (a (@ (class "pull-right")
              (href ,(string-append path-base "/derivations")))
           "View derivations")
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
               (a (@ (href
                      ,(next-page-link path-base
                                       query-parameters
                                       'after_name
                                       (car (last packages)))))
                  "Next page")))
            '())))))

(define* (view-revision-system-tests commit-hash
                                     system-tests
                                     git-repositories
                                     #:key (path-base "/revision/")
                                     header-text header-link)
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
        (h1 "System tests")
        (table
         (@ (class "table"))
         (thead
          (tr
           (th "Name")
           (th "Description")
           (th "Location")
           (th "Derivation")
           (th "Build status")))
         (tbody
          ,@(map
             (match-lambda
               ((name description
                      file line column-number
                      derivation-file-name
                      builds)
                `(tr
                  (td ,name)
                  (td
                   ,(stexi->shtml
                     (texi-fragment->stexi description)))
                  (td ,@(map
                         (match-lambda
                           ((id label url cgit-url-base)
                            (if
                             (and cgit-url-base
                                  (not (string-null? cgit-url-base)))
                             `(a (@ (href
                                     ,(string-append
                                       cgit-url-base "tree/"
                                       file "?id=" commit-hash
                                       "#n" (number->string line))))
                                 ,file
                                 " (line: " ,line
                                 ", column: " ,column-number ")")
                             '())))
                         git-repositories))
                  (td (a (@ (href ,derivation-file-name))
                         ,(display-store-item-short derivation-file-name)))
                  (td ,@(map
                         (lambda (build)
                           (let ((build-server-id
                                  (assoc-ref build "build_server_id")))
                             `(a (@ (href
                                     ,(simple-format
                                       #f "/build-server/~A/build?derivation_file_name=~A"
                                       build-server-id
                                       derivation-file-name)))
                                 ,(build-status-alist->build-icon build))))
                         builds)))))
             system-tests)))))))))

(define* (view-revision-channel-instances commit-hash
                                          channel-instances
                                          #:key (path-base "/revision/")
                                          header-text header-link)
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
        (h1 "Channel instances")
        (table
         (@ (class "table"))
         (thead
          (tr
           (th "System")
           (th "Derivation")
           (th "Build status")))
         (tbody
          ,@(map
             (match-lambda
               ((system derivation-file-name builds)
                `(tr
                  (td (@ (style "font-family: monospace;"))
                      ,system)
                  (td (a (@ (href ,derivation-file-name))
                         ,(display-store-item-short derivation-file-name)))
                  (td ,@(map
                         (lambda (build)
                           (let ((build-server-id
                                  (assoc-ref build "build_server_id")))
                             `(a (@ (href
                                     ,(simple-format
                                       #f "/build-server/~A/build?derivation_file_name=~A"
                                       build-server-id
                                       derivation-file-name)))
                                 ,(build-status-alist->build-icon build))))
                         builds)))))
             channel-instances)))))))))

(define* (view-revision-package-reproducibility revision-commit-hash
                                                output-consistency)
  (layout
   #:body
   `(,(header)
     (style "
.chart-text {
  fill: #000;
  transform: translateY(0.25em);
}
.chart-number {
  font-size: 0.6em;
  line-height: 1;
  text-anchor: middle;
  transform: translateY(-0.25em);
}
.chart-label {
  font-size: 0.2em;
  text-anchor: middle;
  transform: translateY(0.7em);
}
figure {
  display: flex;
  justify-content: space-around;
  flex-direction: column;
  margin-left: -15px;
  margin-right: -15px;
}
@media (min-width: 768px) {
  figure {
    flex-direction: row;
  }
}
.figure-content,
.figure-key {
  flex: 1;
  padding-left: 15px;
  padding-right: 15px;
  align-self: center;
}
.figure-content svg {
  height: auto;
}
.figure-key {
  min-width: calc(8 / 12);
}
.figure-key [class*=\"shape-\"] {
  margin-right: 6px;
}
.figure-key-list {
  margin: 0;
  padding: 0;
  list-style: none;
}
.figure-key-list li {
  margin: 0 0 8px;
  padding: 0;
}
.shape-circle {
  display: inline-block;
  vertical-align: middle;
  margin-right: 0.8em;
  width: 32px;
  height: 32px;
  border-radius: 50%;
}")
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h3 (a (@ (style "white-space: nowrap;")
                  (href ,(string-append "/revision/" revision-commit-hash)))
               "Revision " (samp ,revision-commit-hash)))
        (h1 "Package reproducibility")))
      (div
       (@ (class "row"))
       ;; Inspired by
       ;; https://medium.com/@heyoka/scratch-made-svg-donut-pie-charts-in-html5-2c587e935d72
       ,@(map
           (match-lambda
             ((system . output-consistency)

              (define total
                (apply + (map cdr output-consistency)))

              (define keys
                '(matching not-matching unknown))

              (define output-consistency-percentages
                (map (lambda (key)
                       (exact->inexact
                        (* 100 (/ (or (assq-ref output-consistency key)
                                      0)
                                  total))))
                     keys))


              `(div
                (@ (class "col-sm-6"))
                (h3 (@ (style "font-family: monospace;"))
                    ,system)
                (figure
                 (div
                  (@ (class "figure-content"))
                  (svg
                   (@ (width "100%")
                      (height "100%")
                      (viewBox "0 0 42 42")
                      (class "donut")
                      (aria-labelledby "beers-title beers-desc") (role "img"))
                   (title
                    (@ (id ,(string-append system "-chart-title")))
                    ,(string-append "Package reproducibility for " system))
                   (desc
                    (@ (id ,(string-append system "-chart-desc")))
                    ,(string-append
                      "Donut chart breaking down Guix package reproducibility for "
                      system
                      "."))              ; TODO Describe the data on the chart
                   (circle
                    (@ (class "donut-hole")
                       (cx "21")
                       (cy "21")
                       (r "15.91549430918954")
                       (fill "#fff")
                       (role "presentation")))

                   ,@(map
                      (lambda (key label colour percentage offset)
                        `(circle
                          (@ (class "donut-segment")
                             (cx "21")
                             (cy "21")
                             (r "15.91549430918954")
                             (fill "transparent")
                             (stroke ,colour)
                             (stroke-width "4")
                             (stroke-dasharray ,(simple-format #f "~A ~A"
                                                               percentage
                                                               (- 100 percentage)))
                             (stroke-dashoffset ,offset)
                             (aria-labelledby
                              ,(simple-format #f "donut-segment-~A-title donut-segment-~A-desc"
                                              key key)))
                          (title
                           (@ (id ,(simple-format #f "donut-segment-~A-title"
                                                  key)))
                           ,label)
                          (desc
                           (@ (id ,(simple-format #f "donut-segment-~A-desc"
                                                  key)))
                           ;; TODO Improve this description by stating the
                           ;; colour and count
                           ,(format #f "~2,2f%"
                                    (or percentage 0)))))
                      '(matching not-matching unknown)
                      '("Matching" "Not matching" "Unknown")
                      '("green" "red" "#d2d3d4")
                      output-consistency-percentages
                      (cons 25
                            (map (lambda (cumalative-percentage)
                                   (+ (- 100
                                         cumalative-percentage)
                                      ;; Start at 25, as this will position
                                      ;; the segment at the top of the chart
                                      25))
                                 (reverse
                                  (fold
                                   (lambda (val result)
                                     (cons (+ val (first result))
                                           result))
                                   (list
                                    (first output-consistency-percentages))
                                   (cdr output-consistency-percentages))))))
                   (g
                    (@ (class "chart-text"))
                    ,@(if (and (eq? (or (assq-ref output-consistency
                                                  'matching)
                                        0)
                                    0)
                               (eq? (or (assq-ref output-consistency
                                                  'not-matching)
                                        0)
                                    0))
                          `((text
                             (@ (x "50%")
                                (y "50%")
                                (class "chart-label"))
                             "No data"))
                          `((text
                             (@ (x "50%")
                                (y "50%")
                                (class "chart-number"))
                             ,(simple-format
                               #f "~~~A%"
                               (inexact->exact
                                (round (car output-consistency-percentages)))))
                            (text
                             (@ (x "50%")
                                (y "50%")
                                (class "chart-label"))
                             "Matching"))))))
                 (figcaption
                  (@ (class "figure-key"))
                  (p (@ (class "sr-only"))
                     ,(string-append
                       "Donut chart breaking down Guix package reproducibility for "
                       system
                       "."))            ; TODO Describe the data on the chart
                  (ul
                   (@ (class "figure-key-list")
                      (aria-hidden "true")
                      (role "presentation"))
                   ,@(map (lambda (key label count percentage colour)
                            `(li
                              (span (@ (class "shape-circle")
                                       (style
                                           ,(string-append "background-color: "
                                                           colour ";"))))
                              (a (@ (href
                                     ,(string-append
                                       "/revision/" revision-commit-hash
                                       "/derivation-outputs?"
                                       "output_consistency=" key
                                       "&system=" system)))
                                 ,(format #f "~a (~d, ~2,2f%)"
                                          label
                                          (or count 0)
                                          (or percentage 0)))))
                          '("matching" "not-matching" "unknown")
                          '("Matching" "Not matching" "Unknown")
                          (map (lambda (key)
                                 (assq-ref output-consistency key))
                               keys)
                          output-consistency-percentages
                          '("green" "red" "#d2d3d4"))))))))
           output-consistency))))))

(define* (view-revision-derivations commit-hash
                                    query-parameters
                                    valid-systems
                                    valid-targets
                                    derivations
                                    build-server-urls
                                    show-next-page?
                                    #:key (path-base "/revision/")
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
            "List derivations where part of the file name matches the query.")
          ,(form-horizontal-control
            "System" query-parameters
            #:options valid-systems
            #:help-text "Only include derivations for this system."
            #:font-family "monospace")
          ,(form-horizontal-control
            "Target" query-parameters
            #:options valid-targets
            #:help-text "Only include derivations that are build for this system."
            #:font-family "monospace")
          ,(form-horizontal-control
            "Minimum builds" query-parameters
            #:help-text "Only show derivations with a minimum number of known builds.")
          ,(form-horizontal-control
            "Maximum builds" query-parameters
            #:help-text "Only show derivations with a maximum number of known builds.")
          ,(form-horizontal-control
            "After name" query-parameters
            #:help-text
            "List derivations that are alphabetically after the given name.")
          ,(form-horizontal-control
            "Limit results" query-parameters
            #:help-text "The maximum number of derivations to return.")
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
        (@ (class "col-md-12"))
        (h1 "Package derivations")
        (p "Showing " ,(length derivations) " results")
        (table
         (@ (class "table"))
         (thead
          (tr
           (th "File name")
           (th "System")
           (th "Target")
           (th "Builds")))
         (tbody
          ,@(map
             (match-lambda
               ((file-name system target builds)
                `(tr
                  (td (a (@ (href ,file-name))
                         ,(display-store-item-short file-name)))
                  (td (@ (style "font-family: monospace;"))
                      ,system)
                  (td (@ (style "font-family: monospace;"))
                      ,target)
                  (td
                   (dl
                    (@ (style "margin-bottom: 0;"))
                    ,@(append-map
                       (lambda (build)
                         (let ((build-server-id
                                (assoc-ref build "build_server_id")))
                           `((dt
                              (@ (style "font-weight: unset;"))
                              (a (@ (href
                                     ,(assq-ref build-server-urls
                                                build-server-id)))
                                 ,(assq-ref build-server-urls
                                            build-server-id)))
                             (dd
                              (a (@ (href
                                     ,(simple-format
                                       #f "/build-server/~A/build?derivation_file_name=~A"
                                       build-server-id
                                       file-name)))
                                 ,(build-status-alist->build-icon build))))))
                       (vector->list builds)))))))
             derivations)))
        ,@(if show-next-page?
              `((div
                 (@ (class "row"))
                 (a (@ (href
                        ,(next-page-link path-base
                                         query-parameters
                                         'after_name
                                         (car (last derivations)))))
                    "Next page")))
              '())))))))

(define* (view-revision-derivation-outputs commit-hash
                                           query-parameters
                                           derivation-outputs
                                           build-server-urls
                                           valid-systems
                                           valid-targets
                                           show-next-page?
                                           #:key (path-base "/revision/")
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
            "List outputs where the file name matches this query.")
          ,(form-horizontal-control
            "Output consistency" query-parameters
            #:allow-selecting-multiple-options #f
            #:options '(("Any" . "any")
                        ("Fixed output" . "fixed-output")
                        ("Unknown" . "unknown")
                        ("Matching" . "matching")
                        ("Not-matching" . "not-matching"))
            #:help-text "Do the known hashes for this output suggest it's reproducible, or not reproducible.")
          ,(form-horizontal-control
            "System" query-parameters
            #:options valid-systems
            #:allow-selecting-multiple-options #f
            #:help-text "Only include outputs from derivations for this system."
            #:font-family "monospace")
          ,(form-horizontal-control
            "Target" query-parameters
            #:options valid-targets
            #:allow-selecting-multiple-options #f
            #:help-text "Only include outputs from derivations that are build for this system."
            #:font-family "monospace")
          ,(form-horizontal-control
            "After path" query-parameters
            #:help-text
            "List outputs that are alphabetically after the given name.")
          ,(form-horizontal-control
            "Limit results" query-parameters
            #:help-text "The maximum number of outputs to return.")
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
        (@ (class "col-md-12"))
        (h1 "Package derivation outputs")
        (p "Showing " ,(length derivation-outputs) " results")
        (table
         (@ (class "table"))
         (thead
          (tr
           (th (@ (class "col-sm-5")) "Path")
           (th (@ (class "col-sm-5")) "Data")
           (th (@ (class "col-sm-2")) "Output consistency")))
         (tbody
          ,@(map
             (match-lambda
               ((path hash-algorithm hash recursive nars)
                `(tr
                  (td (a (@ (href ,path))
                         ,(display-store-item-short path)))
                  (td
                   (dl
                    ,@(if
                       (null? hash-algorithm)
                       (append-map
                        (match-lambda
                          ((hash . nars)
                           `((dt
                              (a (@ (style "font-family: monospace;")
                                    (href ,(string-append
                                            path "/narinfos")))
                                 ,hash))
                             (dd
                              (ul
                               (@ (class "list-inline"))
                               ,@(map (lambda (nar)
                                        `(li
                                          ,(assq-ref build-server-urls
                                                     (assoc-ref nar "build_server_id"))))
                                      nars))))))
                        (group-to-alist
                         (lambda (nar)
                           (cons (assoc-ref nar "hash")
                                 nar))
                         (vector->list nars)))
                       `(,hash))))
                  (td
                   ,(let* ((hashes
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
                        "Unknown")
                       ((eq? hash-count 1)
                        '(span (@ (class "text-success"))
                               "Matching"))
                       ((> hash-count 1)
                        '(span (@ (class "text-danger"))
                               "Not matching"))))))))
             derivation-outputs)))
        ,@(if show-next-page?
              `((div
                 (@ (class "row"))
                 (a (@ (href
                        ,(next-page-link path-base
                                         query-parameters
                                         'after_path
                                         (car (last derivation-outputs)))))
                    "Next page")))
              '())))))))

(define (view-revision-builds query-parameters
                              commit-hash
                              build-status-strings
                              valid-systems
                              valid-targets
                              build-server-options
                              stats
                              builds)
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
                  (href ,(string-append "/revision/" commit-hash)))
               "Revision " (samp ,commit-hash)))
        (h1 "Builds")
        (table
         (@ (class "table"))
         (thead
          (tr
           (th (@ (class "col-md-2")) "Status")
           ,@(map (match-lambda
                    ((url . id)
                     `(th (@ (class "col-md-2"))
                          ,url)))
                  build-server-options)))
         (tbody
          ,@(map
             (match-lambda
               ((status counts-by-build-server-id)
                `(tr
                  (td ,(build-status-span status))
                  ,@(map (lambda (id)
                           `(td
                             ,(or (assq-ref counts-by-build-server-id
                                            id)
                                  0)))
                         (map cdr build-server-options)))))
             stats)))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (div
         (@ (class "well"))
         (form
          (@ (method "get")
             (action "")
             (class "form-horizontal"))
          ,(form-horizontal-control
            "Build status" query-parameters
            #:options
            (map (lambda (build-status)
                   (cons (build-status-value->display-string build-status)
                         build-status))
                 build-status-strings)
            #:help-text "Return builds with these statuses.")
          ,(form-horizontal-control
            "Build server"
            query-parameters
            #:options build-server-options
            #:help-text "Return builds from these build servers.")
          ,(form-horizontal-control
            "System" query-parameters
            #:options valid-systems
            #:allow-selecting-multiple-options #f
            #:help-text "Only include derivations for this system."
            #:font-family "monospace")
          ,(form-horizontal-control
            "Target" query-parameters
            #:options valid-targets
            #:allow-selecting-multiple-options #f
            #:help-text "Only include derivations that are build for this system."
            #:font-family "monospace")
          (div (@ (class "form-group form-group-lg"))
               (div (@ (class "col-sm-offset-2 col-sm-10"))
                    (button (@ (type "submit")
                               (class "btn btn-lg btn-primary"))
                            "Update results")))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (table
         (@ (class "table"))
         (thead
          (tr
           (th (@ (class "col-xs-2")) "Status")
           (th (@ (class "col-xs-9")) "Derivation")
           (th (@ (class "col-xs-1")) "Started at")
           (th (@ (class "col-xs-1")) "Finished at")
           (th (@ (class "col-xs-1")) "")))
         (tbody
          ,@(map
             (match-lambda
               ((build-id build-server-url derivation-file-name
                          timestamp status)
                `(tr
                  (td (@ (class "text-center"))
                      (a (@ (href
                             ,(simple-format
                               #f "/build-server/~A/build?derivation_file_name=~A"
                               (assoc-ref build-server-options build-server-url)
                               derivation-file-name)))
                         ,(build-status-span status)))
                  (td (a (@ (href ,derivation-file-name))
                         ,(display-store-item-short derivation-file-name)))
                  (td ,timestamp)
                  (td (a (@ (href ,(simple-format
                                    #f "~Abuild/~A"
                                    build-server-url
                                    (string-drop
                                     derivation-file-name
                                     (string-length "/gnu/store/")))))
                         "View build on " ,build-server-url)))))
             builds)))))))))

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
                                     (@ (class "list-unstyled"))
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

(define (unknown-revision commit-hash job git-repositories-and-branches
                          jobs-and-events)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      ,@(match job
          (()
           `((h1 "Unknown revision")
             (p "No known revision with commit "
                (strong (samp ,commit-hash)))))
          ((jobs ...)
           `((div
              (@ (class "row"))
              (div
               (@ (class "col-md-12"))
               (h1 (@ (style "white-space: nowrap;"))
                   "Revision " (samp ,commit-hash))))
             (div
              (@ (class "row"))
              (div
               (@ (class "col-md-6"))
               (h2 "Packages")
               (strong (@ (class "text-center")
                          (style "font-size: 2em; display: block;"))
                       "Unknown")

               ,@(if (null? git-repositories-and-branches)
                     '()
                     (view-revision/git-repositories
                      git-repositories-and-branches
                      commit-hash))
               ,@(view-revision/jobs-and-events jobs-and-events))
              (div
               (@ (class "col-md-6"))
               (h3 "Derivations")
               (strong (@ (class "text-center")
                          (style "font-size: 2em; display: block;"))
                       "Unknown"))))))))))
