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
  #:use-module (guix-data-service web view html)
  #:export (view-git-repository
            view-branches
            view-branch
            view-branch-package
            view-branch-package-derivations))

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
      (@ (class "container"))
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
              (href ,(string-append
                      "/repository/" git-repository-id
                      "/branch/" branch-name
                      "/package/" package-name
                      ".json")))
           "View JSON")
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
                                "margin-left: ~A%; width: ~A%; height: 10px; background: #DCDCDC;"
                                (rationalize margin-left 1)
                                (rationalize width 1)))))))))))
               versions-by-revision-range))))))))))

(define (view-branch-package-derivations git-repository-id
                                         branch-name
                                         package-name
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
              (href ,(string-append
                      "/repository/" git-repository-id
                      "/branch/" branch-name
                      "/package/" package-name
                      "/derivation-history.json")))
           "View JSON")
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
           (th (@ (class "col-sm-1")) "Version")
           (th (@ (class "col-sm-5")) "Derivation")
           (th (@ (class "col-sm-1")) "From")
           (th (@ (class "col-sm-1")) "To")
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
                                    last-datetime)
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
                                "margin-left: ~A%; width: ~A%; height: 10px; background: #DCDCDC;"
                                (rationalize margin-left 1)
                                (rationalize width 1)))))))))))
               versions-list
               derivations-by-revision-range
               (append
                (map second
                     (cdr derivations-by-revision-range))
                '(#f))))))))))))
