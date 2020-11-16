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

(define-module (guix-data-service web repository html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service web html-utils)
  #:use-module (guix-data-service web view html)
  #:export (view-git-repositories
            view-git-repository
            view-branches
            view-branch
            view-branch-package
            view-branch-package-derivations
            view-branch-package-outputs
            view-no-latest-revision))

(define* (view-git-repositories git-repositories)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (h1 "Git repositories")))
      ,@(map
         (match-lambda
           ((id label url cgit-base-url)
            `(div
              (@ (class "row"))
              (div
               (@ (class "col-md-12"))
               (h3 ,url)
               (a (@ (href ,(string-append "/repository/" (number->string id))))
                  "View repository")
               (dl
                (@ (class "dl-horizontal"))
                (dt "Label")
                (dd ,label)
                (dt "URL")
                (dd ,url)
                (dt "cgit base URL")
                (dd ,cgit-base-url))))))
         git-repositories)))))

(define* (view-git-repository git-repository-id
                              label url cgit-url-base
                              branches-with-most-recent-commits)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (h1 ,url)))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (h3 "Branches")
        ,(table/branches-with-most-recent-commits
          git-repository-id
          branches-with-most-recent-commits)))))))

(define (view-branch git-repository-id
                     branch-name query-parameters branch-commits)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (a (@ (href ,(string-append "/repository/" git-repository-id)))
           (h3 "Repository"))
        (h1 (@ (style "white-space: nowrap;"))
            (samp ,branch-name) " branch")))
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
            "After date" query-parameters
            #:help-text "Only show the branch history after this date.")
          ,(form-horizontal-control
            "Before date" query-parameters
            #:help-text "Only show the branch history before this date.")
          ,(form-horizontal-control
            "Limit results" query-parameters
            #:help-text "The maximum number of results to return.")
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
              (href ,(string-append
                      "/repository/" git-repository-id
                      "/branch/" branch-name "/latest-processed-revision")))
           "Latest processed revision")))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (table
         (@ (class "table")
            (style "table-layout: fixed;"))
         (thead
          (tr
           (th (@ (class "col-sm-3")) "Date")
           (th (@ (class "col-sm-7")) "Commit")
           (th (@ (class "col-sm-1")))))
         (tbody
          ,@(map
             (match-lambda*
               (((commit date revision-exists? job-events)
                 (previous-commit previous-revision-exists?))
                `(tr
                  (td ,date)
                  (td ,@(if (string=? commit "")
                            '((samp "branch deleted"))
                            `((a (@ (href ,(string-append
                                            "/revision/" commit)))
                                 (samp ,commit))
                              " "
                              ,(cond
                                (revision-exists?
                                 '(span
                                   (@ (class "label label-success"))
                                   "✓"))
                                ((member "failure" job-events)
                                 '(span (@ (class "label label-danger"))
                                        "Failed to import data"))
                                (else
                                 '(span (@ (class "label label-default"))
                                        "No information yet"))))))
                  ,@(if (and previous-commit
                             revision-exists?
                             previous-revision-exists?)
                        `((td
                           (@ (style "vertical-align: middle;")
                              (rowspan "2"))
                           (div
                            (@ (class "btn-group")
                               (role "group"))
                            (a (@ (class "btn btn-sm btn-default")
                                  (title "Compare")
                                  (href ,(string-append
                                          "/compare"
                                          "?base_commit=" previous-commit
                                          "&target_commit=" commit)))
                               "⇕ Compare"))))
                          '()))))
             branch-commits
             (append (map (match-lambda
                            ((commit date revision-exists? job-events)
                             (list commit
                                   revision-exists?)))
                          (cdr branch-commits))
                     '((#f #f))))))))))))

(define (view-branch-package git-repository-id
                             branch-name
                             package-name
                             versions-by-revision-range)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container-fluid"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (a (@ (href ,(string-append "/repository/" git-repository-id)))
           (h3 "Repository"))
        (a (@ (href ,(string-append "/repository/" git-repository-id
                                    "/branch/" branch-name)))
           (h3 ,(string-append branch-name " branch")))
        (a (@ (class "btn btn-default btn-lg pull-right")
              (style "margin-left: 0.5em;")
              (href ,(string-append
                      "/repository/" git-repository-id
                      "/branch/" branch-name
                      "/package/" package-name
                      ".json")))
           "View JSON")
        (div
         (@ (class "btn-group pull-right")
            (role "group"))
         (a (@ (class "btn btn-default btn-lg disabled")
               (href ,(string-append
                       "/repository/" git-repository-id
                       "/branch/" branch-name
                       "/package/" package-name)))
            "Versions only")
         (a (@ (class "btn btn-default btn-lg")
               (href ,(string-append
                       "/repository/" git-repository-id
                       "/branch/" branch-name
                       "/package/" package-name
                       "/derivation-history")))
            "Include derivations")
         (a (@ (class "btn btn-default btn-lg")
               (href ,(string-append
                       "/repository/" git-repository-id
                       "/branch/" branch-name
                       "/package/" package-name
                       "/output-history")))
            "Include outputs"))
        (h1 (@ (style "white-space: nowrap;"))
            (samp ,package-name))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (table
         (@ (class "table")
            (style "table-layout: fixed;"))
         (thead
          (tr
           (th (@ (class "col-sm-4")) "Version")
           (th (@ (class "col-sm-4")) "From")
           (th (@ (class "col-sm-4")) "To")))
         (tbody
          ,@(let* ((times-in-seconds
                    (map (lambda (d)
                           (time-second
                            (date->time-monotonic
                             (string->date d "~Y-~m-~d ~H:~M:~S"))))
                         (append (map third versions-by-revision-range)
                                 (map fifth versions-by-revision-range))))
                   (earliest-date-seconds
                    (apply min
                           times-in-seconds))
                   (latest-date-seconds
                    (apply max
                           times-in-seconds))
                   (min-to-max-seconds
                    (- latest-date-seconds
                       earliest-date-seconds)))
              (map
               (match-lambda
                 ((package-version first-guix-revision-commit
                                   first-datetime
                                   last-guix-revision-commit
                                   last-datetime)
                  `((tr
                     (@ (style "border-bottom: 0;"))
                     (td ,package-version)
                     (td (a (@ (href ,(string-append
                                       "/revision/" first-guix-revision-commit)))
                            ,first-datetime)
                         (br)
                         (a (@ (href ,(string-append
                                       "/revision/"
                                       first-guix-revision-commit
                                       "/package/"
                                       package-name "/" package-version)))
                            "(More information)"))
                     (td (a (@ (href ,(string-append
                                       "/revision/" last-guix-revision-commit)))
                            ,last-datetime)
                         (br)
                         (a (@ (href ,(string-append
                                       "/revision/"
                                       last-guix-revision-commit
                                       "/package/"
                                       package-name "/" package-version)))
                            "(More information)")))
                    (tr
                     (td
                      (@ (colspan 3)
                         (style "border-top: 0; padding-top: 0;"))
                      (div
                       (@
                        (style
                            ,(let* ((start-seconds
                                     (time-second
                                      (date->time-monotonic
                                       (string->date first-datetime
                                                     "~Y-~m-~d ~H:~M:~S"))))
                                    (end-seconds
                                     (time-second
                                      (date->time-monotonic
                                       (string->date last-datetime
                                                     "~Y-~m-~d ~H:~M:~S"))))
                                    (margin-left
                                     (min
                                      (* (/ (- start-seconds earliest-date-seconds)
                                            min-to-max-seconds)
                                         100)
                                      98))
                                    (width
                                     (max
                                      (- (* (/ (- end-seconds earliest-date-seconds)
                                               min-to-max-seconds)
                                            100)
                                         margin-left)
                                      2)))
                               (simple-format
                                #f
                                "margin-left: ~A%; width: ~A%; height: 10px; background: #BEBEBE;"
                                (rationalize margin-left 1)
                                (rationalize width 1)))))))))))
               versions-by-revision-range))))))))))

(define (view-branch-package-derivations query-parameters
                                         git-repository-id
                                         branch-name
                                         package-name
                                         valid-systems
                                         valid-targets
                                         build-server-urls
                                         derivations-by-revision-range)
  (define versions-list
    (pair-fold (match-lambda*
                 (((last) (count result ...))
                  (cons (cons last count)
                        result))
                 (((a b rst ...) (count result ...))
                  (if (string=? a b)
                      (cons (+ 1 count)
                            (cons #f result))
                      (cons 1
                            (cons (cons a count)
                                  result)))))
               '(1)
               (reverse
                (map first derivations-by-revision-range))))

  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container-fluid"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (a (@ (href ,(string-append "/repository/" git-repository-id)))
           (h3 "Repository"))
        (a (@ (href ,(string-append "/repository/" git-repository-id
                                    "/branch/" branch-name)))
           (h3 ,(string-append branch-name " branch")))
        (a (@ (class "btn btn-default btn-lg pull-right")
              (style "margin-left: 0.5em;")
              (href ,(string-append
                      "/repository/" git-repository-id
                      "/branch/" branch-name
                      "/package/" package-name
                      "/derivation-history.json")))
           "View JSON")
        (div
         (@ (class "btn-group pull-right")
            (role "group"))
         (a (@ (class "btn btn-default btn-lg")
               (href ,(string-append
                       "/repository/" git-repository-id
                       "/branch/" branch-name
                       "/package/" package-name)))
            "Versions only")
         (a (@ (class "btn btn-default btn-lg disabled")
               (href ,(string-append
                       "/repository/" git-repository-id
                       "/branch/" branch-name
                       "/package/" package-name
                       "/derivation-history")))
            "Include derivations")
         (a (@ (class "btn btn-default btn-lg")
               (href ,(string-append
                       "/repository/" git-repository-id
                       "/branch/" branch-name
                       "/package/" package-name
                       "/output-history")))
            "Include outputs"))
        (h1 (@ (style "white-space: nowrap;"))
            (samp ,package-name))))
      (div
       (@ (class "col-md-12"))
       (div
        (@ (class "well"))
        (form
         (@ (method "get")
            (action "")
            (class "form-horizontal"))
         ,(form-horizontal-control
           "System" query-parameters
           #:options valid-systems
           #:allow-selecting-multiple-options #f
           #:help-text "Show derivations with this system.")
         ,(form-horizontal-control
           "Target" query-parameters
           #:options valid-targets
           #:allow-selecting-multiple-options #f
           #:help-text "Show derivations with this target.")
         (div (@ (class "form-group form-group-lg"))
              (div (@ (class "col-sm-offset-2 col-sm-10"))
                   (button (@ (type "submit")
                              (class "btn btn-lg btn-primary"))
                           "Update results"))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (table
         (@ (class "table")
            (style "table-layout: fixed;"))
         (thead
          (tr
           (th (@ (class "col-sm-1")) "Version")
           (th (@ (class "col-sm-4")) "Derivation")
           (th (@ (class "col-sm-2")) "Builds")
           (th (@ (class "col-sm-2")) "From")
           (th (@ (class "col-sm-2")) "To")
           (th (@ (class "col-sm-1")) "")
           (th (@ (class "col-sm-1")) "")))
         (tbody
          ,@(let* ((times-in-seconds
                    (map (lambda (d)
                           (time-second
                            (date->time-monotonic
                             (string->date d "~Y-~m-~d ~H:~M:~S"))))
                         (append (map fourth derivations-by-revision-range)
                                 (map sixth derivations-by-revision-range))))
                   (earliest-date-seconds
                    (apply min
                           times-in-seconds))
                   (latest-date-seconds
                    (apply max
                           times-in-seconds))
                   (min-to-max-seconds
                    (- latest-date-seconds
                       earliest-date-seconds)))
              (map
               (match-lambda*
                 ((version-column-entry
                   (package-version derivation-file-name
                                    first-guix-revision-commit
                                    first-datetime
                                    last-guix-revision-commit
                                    last-datetime
                                    builds)
                   next-derivation-file-name)
                  `((tr
                     (@ (style "border-bottom: 0;"))
                     ,@(match version-column-entry
                         (#f '())
                         ((package-version . rowspan)
                          `((td (@ (rowspan ,(* 2 ; To account for the extra rows
                                                rowspan)))
                                ,package-version))))
                     (td
                      (a (@ (href ,derivation-file-name))
                         ,(display-store-item derivation-file-name)))
                     (td
                      (dl
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
                                          derivation-file-name)))
                                    ,(build-status-alist->build-icon build))))))
                          builds)))
                     (td (a (@ (href ,(string-append
                                       "/revision/" first-guix-revision-commit)))
                            ,first-datetime))
                     (td (a (@ (href ,(string-append
                                       "/revision/" last-guix-revision-commit)))
                            ,last-datetime))
                     (td
                      (@ (rowspan 4)
                         (style "vertical-align: middle;"))
                      ,@(if next-derivation-file-name
                            `((a
                               (@ (class "btn btn-sm btn-default")
                                  (title "Compare")
                                  (href
                                   ,(string-append
                                     "/compare/derivation"
                                     "?base_derivation=" next-derivation-file-name
                                     "&target_derivation=" derivation-file-name)))
                               "⇕ Compare"))
                            '())))
                    (tr
                     (td
                      (@ (colspan 4)
                         (style "border-top: 0; padding-top: 0;"))
                      (div
                       (@
                        (style
                            ,(let* ((start-seconds
                                     (time-second
                                      (date->time-monotonic
                                       (string->date first-datetime
                                                     "~Y-~m-~d ~H:~M:~S"))))
                                    (end-seconds
                                     (time-second
                                      (date->time-monotonic
                                       (string->date last-datetime
                                                     "~Y-~m-~d ~H:~M:~S"))))
                                    (margin-left
                                     (min
                                      (* (/ (- start-seconds earliest-date-seconds)
                                            min-to-max-seconds)
                                         100)
                                      98))
                                    (width
                                     (max
                                      (- (* (/ (- end-seconds earliest-date-seconds)
                                               min-to-max-seconds)
                                            100)
                                         margin-left)
                                      2)))
                               (simple-format
                                #f
                                "margin-left: ~A%; width: ~A%; height: 10px; background: #BEBEBE;"
                                (rationalize margin-left 1)
                                (rationalize width 1)))))))))))
               versions-list
               derivations-by-revision-range
               (append
                (map second
                     (cdr derivations-by-revision-range))
                '(#f))))))))))))

(define (view-branch-package-outputs query-parameters
                                     git-repository-id
                                     branch-name
                                     package-name
                                     output-name
                                     valid-systems
                                     valid-targets
                                     build-server-urls
                                     outputs-by-revision-range)
  (define versions-list
    (pair-fold (match-lambda*
                 (((last) (count result ...))
                  (cons (cons last count)
                        result))
                 (((a b rst ...) (count result ...))
                  (if (string=? a b)
                      (cons (+ 1 count)
                            (cons #f result))
                      (cons 1
                            (cons (cons a count)
                                  result)))))
               '(1)
               (reverse
                (map first outputs-by-revision-range))))

  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container-fluid"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (a (@ (href ,(string-append "/repository/" git-repository-id)))
           (h3 "Repository"))
        (a (@ (href ,(string-append "/repository/" git-repository-id
                                    "/branch/" branch-name)))
           (h3 ,(string-append branch-name " branch")))
        (a (@ (class "btn btn-default btn-lg pull-right")
              (style "margin-left: 0.5em;")
              (href ,(string-append
                      "/repository/" git-repository-id
                      "/branch/" branch-name
                      "/package/" package-name
                      "/output-history.json")))
           "View JSON")
        (div
         (@ (class "btn-group pull-right")
            (role "group"))
         (a (@ (class "btn btn-default btn-lg")
               (href ,(string-append
                       "/repository/" git-repository-id
                       "/branch/" branch-name
                       "/package/" package-name)))
            "Versions only")
         (a (@ (class "btn btn-default btn-lg")
               (href ,(string-append
                       "/repository/" git-repository-id
                       "/branch/" branch-name
                       "/package/" package-name
                       "/derivation-history")))
            "Include derivations")
         (a (@ (class "btn btn-default btn-lg disabled")
               (href ,(string-append
                       "/repository/" git-repository-id
                       "/branch/" branch-name
                       "/package/" package-name
                       "/output-history")))
            "Include outputs"))
        (h1 (@ (style "white-space: nowrap;"))
            (samp ,package-name))))
      (div
       (@ (class "col-md-12"))
       (div
        (@ (class "well"))
        (form
         (@ (method "get")
            (action "")
            (class "form-horizontal"))
         ,(form-horizontal-control
           "Output" query-parameters
           #:help-text "Show this output for the package.")
         ,(form-horizontal-control
           "System" query-parameters
           #:options valid-systems
           #:allow-selecting-multiple-options #f
           #:help-text "Show derivations with this system.")
         ,(form-horizontal-control
           "Target" query-parameters
           #:options valid-targets
           #:allow-selecting-multiple-options #f
           #:help-text "Show derivations with this target.")
         (div (@ (class "form-group form-group-lg"))
              (div (@ (class "col-sm-offset-2 col-sm-10"))
                   (button (@ (type "submit")
                              (class "btn btn-lg btn-primary"))
                           "Update results"))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (table
         (@ (class "table")
            (style "table-layout: fixed;"))
         (thead
          (tr
           (th (@ (class "col-sm-1")) "Version")
           (th (@ (class "col-sm-6")) "Output")
           (th (@ (class "col-sm-2")) "Builds")
           (th (@ (class "col-sm-2")) "From")
           (th (@ (class "col-sm-2")) "To")))
         (tbody
          ,@(let* ((times-in-seconds
                    (map (lambda (d)
                           (time-second
                            (date->time-monotonic
                             (string->date d "~Y-~m-~d ~H:~M:~S"))))
                         (append (map fourth outputs-by-revision-range)
                                 (map sixth outputs-by-revision-range))))
                   (earliest-date-seconds
                    (apply min
                           times-in-seconds))
                   (latest-date-seconds
                    (apply max
                           times-in-seconds))
                   (min-to-max-seconds
                    (- latest-date-seconds
                       earliest-date-seconds)))
              (map
               (match-lambda*
                 ((version-column-entry
                   (package-version output-path
                                    first-guix-revision-commit
                                    first-datetime
                                    last-guix-revision-commit
                                    last-datetime
                                    builds))
                  `((tr
                     (@ (style "border-bottom: 0;"))
                     ,@(match version-column-entry
                         (#f '())
                         ((package-version . rowspan)
                          `((td (@ (rowspan ,(* 2 ; To account for the extra rows
                                                rowspan)))
                                ,package-version))))
                     (td
                      (a (@ (href ,output-path))
                         ,(display-store-item output-path)))
                     (td
                      (dl
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
                                          (assoc-ref build "derivation_file_name"))))
                                    ,(build-status-alist->build-icon build))))))
                          builds)))
                     (td (a (@ (href ,(string-append
                                       "/revision/" first-guix-revision-commit)))
                            ,first-datetime))
                     (td (a (@ (href ,(string-append
                                       "/revision/" last-guix-revision-commit)))
                            ,last-datetime)))
                    (tr
                     (td
                      (@ (colspan 4)
                         (style "border-top: 0; padding-top: 0;"))
                      (div
                       (@
                        (style
                            ,(let* ((start-seconds
                                     (time-second
                                      (date->time-monotonic
                                       (string->date first-datetime
                                                     "~Y-~m-~d ~H:~M:~S"))))
                                    (end-seconds
                                     (time-second
                                      (date->time-monotonic
                                       (string->date last-datetime
                                                     "~Y-~m-~d ~H:~M:~S"))))
                                    (margin-left
                                     (min
                                      (* (/ (- start-seconds earliest-date-seconds)
                                            min-to-max-seconds)
                                         100)
                                      98))
                                    (width
                                     (max
                                      (- (* (/ (- end-seconds earliest-date-seconds)
                                               min-to-max-seconds)
                                            100)
                                         margin-left)
                                      2)))
                               (simple-format
                                #f
                                "margin-left: ~A%; width: ~A%; height: 10px; background: #BEBEBE;"
                                (rationalize margin-left 1)
                                (rationalize width 1)))))))))))
               versions-list
               outputs-by-revision-range))))))))))

(define (view-no-latest-revision branch-name)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (h1 "No latest revision")
      (p "No latest revision for "
         (strong (samp ,branch-name))
         " branch")))))
