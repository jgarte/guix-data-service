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

(define-module (guix-data-service web compare html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service web html-utils)
  #:use-module (guix-data-service web view html)
  #:export (compare
            compare/derivation
            compare/package-derivations
            compare-by-datetime/package-derivations
            compare/packages
            compare/system-test-derivations
            compare-invalid-parameters))

(define (compare-form-controls-for-mode mode query-parameters)
  (cond
   ((eq? mode 'revision)
    (list
     (form-horizontal-control
      "Base commit" query-parameters
      #:required? #t
      #:help-text "The commit to use as the basis for the comparison."
      #:font-family "monospace")
     (form-horizontal-control
      "Target commit" query-parameters
      #:required? #t
      #:help-text "The commit to compare against the base commit."
      #:font-family "monospace")))
   ((eq? mode 'datetime)
    (list
     (form-horizontal-control
      "Base branch" query-parameters
      #:required? #t
      #:help-text "The branch to compare from."
      #:font-family "monospace")
     (form-horizontal-control
      "Base datetime" query-parameters
      #:help-text "The date and time to compare from. The required format is YYYY-MM-DD HH:MM:SS"
      #:font-family "monospace")
     (form-horizontal-control
      "Target branch" query-parameters
      #:required? #t
      #:help-text "The branch to compare to."
      #:font-family "monospace")
     (form-horizontal-control
      "Target datetime" query-parameters
      #:help-text "The date and time to compare to. The required format is YYYY-MM-DD HH:MM:SS"
      #:font-family "monospace")))
   (else
    '())))

(define (compare query-parameters
                 mode
                 cgit-url-bases
                 new-packages
                 removed-packages
                 version-changes
                 lint-warnings-data
                 lint-warnings-locale-options
                 channel-news-data)
  (define invalid-query?
    (any-invalid-query-parameters? query-parameters))

  (define base-commit
    (assq-ref query-parameters 'base_commit))

  (define target-commit
    (assq-ref query-parameters 'target_commit))

  (define locale
    (assq-ref query-parameters 'locale))

  (define query-params
    (unless invalid-query?
      (query-parameters->string query-parameters)))

  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-7"))
        ,@(if invalid-query?
              `((h1 "Compare"))
              `((h1 "Comparing "
                    (a (@ (href ,(string-append "/revision/" base-commit)))
                       (samp ,(string-take base-commit 8) "…"))
                    " and "
                    (a (@ (href ,(string-append "/revision/" target-commit)))
                       (samp ,(string-take target-commit 8) "…")))
                ,@(if (apply string=? cgit-url-bases)
                      `((a (@ (href ,(string-append
                                      (first cgit-url-bases)
                                      "log/?qt=range&q="
                                      base-commit ".." target-commit)))
                           "(View cgit)"))
                      '()))))
       (div
        (@ (class "col-sm-5"))
        (div
         (@ (class "btn-group btn-group-lg")
            (style "margin-top: 1.3rem; margin-bottom: 0.5rem;")
            (role "group"))
         (a (@ (class ,(string-append
                        "btn btn-default btn-lg"
                        (if (eq? mode 'revision)
                            " disabled"
                            "")))
               (href "/compare"))
            "Compare revisions")
         (a (@ (class ,(string-append
                        "btn btn-default btn-lg"
                        (if (eq? mode 'datetime)
                            " disabled"
                            "")))
               (href "/compare-by-datetime"))
            "Compare by datetime"))))

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
            ,@(compare-form-controls-for-mode mode query-parameters)
            ,(form-horizontal-control
              "Locale" query-parameters
              #:name "locale"
              #:allow-selecting-multiple-options #f
              #:options lint-warnings-locale-options
              #:help-text "Language")
            (div (@ (class "form-group form-group-lg"))
                 (div (@ (class "col-sm-offset-2 col-sm-10"))
                      (button (@ (type "submit")
                                 (class "btn btn-lg btn-primary"))
                              "Update results")))))))

      ,@(if
         invalid-query?
         '()
         `((div
            (@ (class "row") (style "clear: left;"))
            (div
             (@ (class "col-sm-10"))
             (div
              (@ (class "btn-group btn-group-lg")
                 (role "group"))
              (a (@ (class "btn btn-default")
                    (href ,(string-append
                            "/"
                            (cond
                             ((eq? mode 'revision) "compare")
                             ((eq? mode 'datetime) "compare-by-datetime"))
                            "/packages?"
                            query-params)))
                 "Compare packages")
              (a (@ (class "btn btn-default")
                    (href ,(string-append
                            "/"
                            (cond
                             ((eq? mode 'revision) "compare")
                             ((eq? mode 'datetime) "compare-by-datetime"))
                            "/package-derivations?"
                            query-params)))
                 "Compare package derivations")
             (a (@ (class "btn btn-default")
                   (href ,(string-append
                           "/"
                           (cond
                            ((eq? mode 'revision) "compare")
                            ((eq? mode 'datetime) "compare-by-datetime"))
                           "/system-test-derivations?"
                           query-params)))
                "Compare system test derivations")))
            (div
             (@ (class "col-sm-2"))
             (a (@ (class "btn btn-default btn-lg pull-right")
                   (href ,(string-append
                           "/compare.json?" query-params)))
                "View JSON")))
           (div
            (@ (class "row"))
            (div
             (@ (class "col-sm-12"))
             (h3 (@ (style "clear: both;"))
                 "News entries")
             ,(if (null? channel-news-data)
                  "No news entry changes"
                  (map
                   (match-lambda
                     ((commit tag title-text body-text change)
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
                           (th (@ (class "col-sm-1")) "")
                           (th (@ (class "col-sm-1")) "Language")
                           (th (@ (class "col-sm-3")) "Title")
                           (th (@ (class "col-sm-7")) "Body"))
                          (tbody
                           ,@(let ((languages
                                    (sort
                                     (delete-duplicates
                                      (append (map car title-text)
                                              (map car body-text)))
                                     string<?)))
                               (map (lambda (lang index)
                                      `(tr
                                        ,@(if (eq? index 0)
                                              `((td (@ (rowspan ,(length languages)))
                                                    ,(case change
                                                       ((new) "New")
                                                       ((removed) "Removed")
                                                       ((changed) "Changed"))))
                                              '())
                                        (td ,lang)
                                        (td ,(stexi->shtml
                                              (texi-fragment->stexi
                                               (assoc-ref title-text lang))))
                                        (td ,
                                         (stexi->shtml
                                          (texi-fragment->stexi
                                           (assoc-ref body-text lang))))))
                                    languages
                                    (iota (length languages))))))))))
                   channel-news-data))))
           (div
            (@ (class "row"))
            (div
             (@ (class "col-sm-12"))
             (h3 "New packages")
             ,(if (null? new-packages)
                  '(p "No new packages")
                  `(table
                    (@ (class "table"))
                    (thead
                     (tr
                      (th (@ (class "col-md-4")) "Name")
                      (th (@ (class "col-md-4")) "Version")
                      (th (@ (class "col-md-4")) "")))
                    (tbody
                     ,@(map
                        (match-lambda
                          ((('name . name)
                            ('version . version))
                           `(tr
                             (td ,name)
                             (td ,version)
                             (td (@ (class "text-right"))
                                 (a (@ (href ,(string-append
                                               "/revision/" target-commit
                                               "/package/" name "/" version)))
                                    "More information")))))
                        new-packages))))))
           (div
            (@ (class "row"))
            (div
             (@ (class "col-sm-12"))
             (h3 "Removed packages")
             ,(if (null? removed-packages)
                  '(p "No removed packages")
                  `(table
                    (@ (class "table"))
                    (thead
                     (tr
                      (th (@ (class "col-md-4")) "Name")
                      (th (@ (class "col-md-4")) "Version")
                      (th (@ (class "col-md-4")) "")))
                    (tbody
                     ,@(map
                        (match-lambda
                          ((('name . name)
                            ('version . version))
                           `(tr
                             (td ,name)
                             (td ,version)
                             (td (@ (class "text-right"))
                                 (a (@ (href ,(string-append
                                               "/revision/" base-commit
                                               "/package/" name "/" version)))
                                    "More information")))))
                        removed-packages))))))
           (div
            (@ (class "row"))
            (div
             (@ (class "col-sm-12"))
             (h3 "Version changes")
             ,(if
               (null? version-changes)
               '(p "No version changes")
               `(table
                 (@ (class "table"))
                 (thead
                  (tr
                   (th (@ (class "col-md-3")) "Name")
                   (th (@ (class "col-md-9")) "Versions")))
                 (tbody
                  ,@(map
                     (match-lambda
                       ((name . versions)
                        `(tr
                          (td ,name)
                          (td
                           (ul
                            (@ (class "list-unstyled"))
                            ,@(map
                               (match-lambda
                                 ((type . versions)
                                  `(li (@ (class ,(if (eq? type 'base)
                                                      "text-danger"
                                                      "text-success")))
                                       (ul
                                        (@ (class "list-inline")
                                           (style "display: inline-block;"))
                                        ,@(map
                                           (lambda (version)
                                             `(li (a (@ (href
                                                         ,(string-append
                                                           "/revision/"
                                                           (if (eq? type 'base)
                                                               base-commit
                                                               target-commit)
                                                           "/package/"
                                                           name "/" version)))
                                                     ,version)))
                                           (vector->list versions)))
                                       ,(if (eq? type 'base)
                                            " (old)"
                                            " (new)"))))
                               versions))))))
                     version-changes))))))
           (div
            (@ (class "row"))
            (div
             (@ (class "col-sm-12"))
             (h2 "Lint warnings")
             ,@(if
                (null? lint-warnings-data)
                '((p "No lint warning changes"))
                (map
                 (match-lambda
                   (((package-name package-version) . warnings)
                    `((h4 ,package-name " (version: " ,package-version ")")
                      (table
                       (@ (class "table"))
                       (thead
                        (tr
                         (th "")
                         (th "Linter")
                         (th "Message")))
                       (tbody
                        ,@(map (match-lambda
                                 ((lint-checker-name
                                   message
                                   lint-checker-description
                                   lint-checker-network-dependent
                                   file line column-number ;; TODO Maybe use the location?
                                   change)

                                  `(tr
                                    (td (@ (class ,(if (string=? change "new")
                                                       "text-danger"
                                                       "text-success"))
                                           (style "font-weight: bold"))
                                        ,(if (string=? change "new")
                                             "New warning"
                                             "Resolved warning"))
                                    (td (span (@ (style "font-family: monospace; display: block;"))
                                              ,lint-checker-name)
                                        (p (@ (style "font-size: small; margin: 6px 0 0px;"))
                                           ,lint-checker-description))
                                    (td ,message))))
                               warnings))))))
                 lint-warnings-data))))))))))

(define (compare/derivation query-parameters data)
  (define base
    '(span (@ (class "text-danger glyphicon glyphicon-minus pull-left")
              (style "font-size: 1.5em; padding-right: 0.4em;"))))

  (define target
    '(span (@ (class "text-success glyphicon glyphicon-plus pull-left")
              (style "font-size: 1.5em; padding-right: 0.4em;"))))

  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (h1 ,@(let ((base-commit (assq-ref query-parameters 'base_commit))
                   (target-commit (assq-ref query-parameters 'target_commit)))
               (if (every string? (list base-commit target-commit))
                   `("Comparing "
                     (a (@ (href ,(string-append "/revision/" base-commit)))
                        (samp ,(string-take base-commit 8) "…"))
                     " and "
                     (a (@ (href ,(string-append "/revision/" target-commit)))
                        (samp ,(string-take target-commit 8) "…")))
                   '("Comparing derivations")))))
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
            "Base derivation" query-parameters
            #:required? #t
            #:help-text "The derivation to use as the basis for the comparison."
            #:font-family "monospace")
          ,(form-horizontal-control
            "Target derivation" query-parameters
            #:required? #t
            #:help-text "The derivation to compare against the base commit."
            #:font-family "monospace")
          (div (@ (class "form-group form-group-lg"))
               (div (@ (class "col-sm-offset-2 col-sm-10"))
                    (button (@ (type "submit")
                               (class "btn btn-lg btn-primary"))
                            "Update results")))
          (a (@ (class "btn btn-default btn-lg pull-right")
                (href ,(let ((query-parameter-string
                              (query-parameters->string query-parameters)))
                         (string-append
                          "/compare/derivation.json"
                          (if (string-null? query-parameter-string)
                              ""
                              (string-append "?" query-parameter-string))))))
             "View JSON")))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h2 "Outputs")
        ,@(let ((outputs (assq-ref data 'outputs)))
            `((table
               (@ (class "table"))
               (thead
                (tr
                 (th "")
                 (th "Name")
                 (th "Path")
                 (th "Hash algorithm")
                 (th "Hash")
                 (th "Recursive")))
               (tbody
                ,@(let ((base-outputs (assq-ref outputs 'base))
                        (target-outputs (assq-ref outputs 'target))
                        (common-outputs (assq-ref outputs 'common)))
                    (append-map
                     (lambda (label items)
                       (map
                        (match-lambda
                          ((name path hash-algorithm hash recursive)
                           `(tr
                             (td ,label)
                             (td ,name)
                             (td (a (@ (href ,path))
                                    ,(display-store-item path)))
                             (td ,hash-algorithm)
                             (td ,hash)
                             (td ,recursive))))
                        (or items '())))
                     (list base target "Common")
                     (list (assq-ref outputs 'base)
                           (assq-ref outputs 'target)
                           (assq-ref outputs 'common))))))))
        (h2 "Inputs")
        ,@(let ((inputs (assq-ref data 'inputs)))
            `((table
               (@ (class "table"))
               (thead
                (tr
                 (th "")
                 (th "Derivation")
                 (th "Outputs")))
               (tbody
                ,@(append-map
                   (lambda (label items)
                     (map
                      (match-lambda
                        ((derivation outputs)
                         `(tr
                           (td ,label)
                           (td (a (@ (href ,derivation))
                                  ,(display-store-item derivation)))
                           (td ,outputs))))
                      (or items '())))
                   (list base target)
                   (list (assq-ref inputs 'base)
                         (assq-ref inputs 'target)))))))
        (p "Common inputs are omitted.")
        (h2 "Sources")
        ,@(let ((sources (assq-ref data 'sources)))
            `((table
               (@ (class "table"))
               (thead
                (tr
                 (th "")
                 (th "Derivation")))
               (tbody
                ,@(append-map
                   (lambda (label items)
                     (map
                      (match-lambda
                        ((file)
                         `(tr
                           (td ,label)
                           (td (a (@ (href ,file))
                                  ,(display-store-item file))))))
                      (or items '())))
                   (list base target "Common")
                   (list (assq-ref sources 'base)
                         (assq-ref sources 'target)
                         (assq-ref sources 'common)))))))
        (h2 "System")
        ,@(let ((system (assq-ref data 'system)))
            (let ((common-system (assq-ref system 'common)))
              (if common-system
                  (list common-system)
                  `(table
                    (@ (class "table"))
                    (thead
                     (tr
                      (th "")
                      (th "System")))
                    (tbody
                     ,@(let ((base-system (assq-ref system 'base))
                             (target-system (assq-ref system 'target)))
                         `((tr
                            (td ,base)
                            (td ,base-system))
                           (tr
                            (td ,target)
                            (td ,target-system)))))))))
        (h2 "Builder and arguments")
        ,(let ((builder (assq-ref data 'builder))
               (arguments (assq-ref data 'arguments)))
           (let ((common-builder (assq-ref builder 'common))
                 (common-args (assq-ref arguments 'common)))
             (if (and common-builder
                      common-args)
                 `(table
                   (@ (class "table"))
                   (thead
                    (th "Builder")
                    (th "Arguments"))
                   (tbody
                    (tr
                     (td ,(display-possible-store-item common-builder))
                     (td (ol
                          ,@(map (lambda (arg)
                                   `(li ,(display-possible-store-item arg)))
                                 common-args))))))
                 `(table
                   (@ (class "table"))
                   (thead
                    (tr
                     (th "")
                     (th "Builder")
                     (th "Arguments")))
                   (tbody
                    ,@(let ((base-builder (assq-ref builder 'base))
                            (target-builder (assq-ref builder 'target))
                            (base-args (assq-ref arguments 'base))
                            (target-args (assq-ref arguments 'target)))
                        `((tr
                           (td ,base)
                           (td ,(display-possible-store-item
                                 (or base-builder
                                     common-builder)))
                           (td (ol
                                ,@(map (lambda (arg)
                                         `(li ,(display-possible-store-item arg)))
                                       (or common-args
                                           base-args)))))
                          (tr
                           (td ,target)
                           (td ,(display-possible-store-item
                                 (or target-builder
                                     common-builder)))
                           (td (ol
                                ,@(map (lambda (arg)
                                         `(li ,(display-possible-store-item arg)))
                                       (or common-args
                                           target-args))))))))))))
        (h2 "Environment variables")
        ,(let ((environment-variables (assq-ref data 'environment-variables)))
           `(table
             (@ (class "table"))
             (thead
              (th "Name"))
             (tbody
              ,@(append-map
                 (match-lambda
                   ((name . values)
                    (let ((common-value (assq-ref values 'common)))
                      (if common-value
                          `((tr
                             (td ,name)
                             (td ,(display-possible-store-item common-value))))
                          (let ((base-value (assq-ref values 'base))
                                (target-value (assq-ref values 'target)))
                            (if (and base-value target-value)
                                `((tr
                                   (td (@ (rowspan 2))
                                       ,name)
                                   (td ,base ,(display-possible-store-item
                                               base-value)))
                                  (tr
                                   (td ,target ,(display-possible-store-item
                                                 target-value))))
                                `((tr
                                   (td ,name)
                                   (td ,@(if base-value
                                             (list base
                                                   (display-possible-store-item
                                                    base-value))
                                             (list target
                                                   (display-possible-store-item
                                                    target-value))))))))))))
                 environment-variables))))))))))

(define* (compare/package-derivations query-parameters
                                      mode
                                      valid-systems
                                      valid-targets
                                      valid-build-statuses
                                      build-server-urls
                                      derivation-changes
                                      #:optional
                                      base-revision-details
                                      target-revision-details)
  (define field-options
    (map
     (lambda (field)
       (cons field
             (hyphenate-words
              (string-downcase field))))
     '("(no additional fields)" "Builds")))

  (define fields
    (assq-ref query-parameters 'field))

  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       ,@(cond
          ((any-invalid-query-parameters? query-parameters)
           '((h3 "Comparing package derivations")))
          ((eq? mode 'revision)
           (let ((base-commit (assq-ref query-parameters 'base_commit))
                 (target-commit (assq-ref query-parameters 'target_commit)))
             `((h3
                (a (@ (href ,(string-append
                              "/compare?base_commit="
                              base-commit
                              "&target_commit="
                              target-commit)))
                   "Comparing "
                   (samp ,(string-take base-commit 8) "…")
                   " and "
                   (samp ,(string-take target-commit 8) "…"))))))
          ((eq? mode 'datetime)
           (let ((base-branch (assq-ref query-parameters 'base_branch))
                 (base-datetime (assq-ref query-parameters 'base_datetime))
                 (target-branch (assq-ref query-parameters 'target_branch))
                 (target-datetime (assq-ref query-parameters 'target_datetime)))
             `((h3
                (a (@ (href ,(string-append
                              "/compare-by-datetime?"
                              (query-parameters->string
                               (filter (match-lambda
                                         ((key . _)
                                          (member key '(base_branch
                                                        base_datetime
                                                        target_branch
                                                        target_datetime))))
                                       query-parameters)))))
                   "Comparing "
                   (br)
                   (samp (*ENTITY* nbsp) (*ENTITY* nbsp)
                         ,base-branch
                         ,@(map (lambda _ '(*ENTITY* nbsp))
                                (iota (max
                                       0
                                       (- (string-length target-branch)
                                          (string-length base-branch))))))
                   " at " ,(date->string base-datetime "~1 ~3")
                   " to "
                   (br)
                   (samp (*ENTITY* nbsp) (*ENTITY* nbsp)
                         ,target-branch
                         ,@(map (lambda _ '(*ENTITY* nbsp))
                                (iota (max 0
                                           (- (string-length base-branch)
                                              (string-length target-branch))))))
                   " at " ,(date->string target-datetime "~1 ~3"))))))))
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
          ,@(compare-form-controls-for-mode mode query-parameters)
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
            "Build change" query-parameters
            #:options '(("(none specified)" . "")
                        ("Broken"           . "broken")
                        ("Fixed"            . "fixed")
                        ("Still working"    . "still-working")
                        ("Still failing"    . "still-failing")
                        ("Unknown"          . "unknown"))
            #:help-text '("Filter by the changes to the builds:"
                          (dl
                           (@ (class "dl-horizontal"))
                           (dt "Broken")
                           (dd
                            "There was a successful build against the base
derivation, but no successful build for the target derivation, and there's at
least one failed build.")
                           (dt "Fixed")
                           (dd
                            "No successful build for the base derivation and
at least one failed build, plus at least one successful build for the target
derivation")
                           (dt "Still working")
                           (dd
                            "At least one successful build for both the base
and target derivations")
                           (dt "Still broken")
                           (dd
                            "No successful builds and at least one failed builds for both the base and target derivations")
                           (dt "Unknown")
                           (dd
                            "No base and target derivation to compare, or not
enough builds to determine a change")))
            #:allow-selecting-multiple-options #f)
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
            #:help-text "The maximum number of results to return.")
          ,(form-horizontal-control
            "All results" query-parameters
            #:type "checkbox"
            #:help-text "Return all results.")
          (div (@ (class "form-group form-group-lg"))
               (div (@ (class "col-sm-offset-2 col-sm-10"))
                    (button (@ (type "submit")
                               (class "btn btn-lg btn-primary"))
                            "Update results")))
          (a (@ (class "btn btn-default btn-lg pull-right")
                (href ,(let ((query-parameter-string
                              (query-parameters->string query-parameters)))
                         (string-append
                          "/"
                          (cond
                           ((eq? mode 'revision) "compare")
                           ((eq? mode 'datetime) "compare-by-datetime"))
                          "/package-derivations.json"
                          (if (string-null? query-parameter-string)
                              ""
                              (string-append "?" query-parameter-string))))))
             "View JSON")))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 "Package derivation changes")
        ,(if
          (null? derivation-changes)
          '(p "No derivation changes")
          `(table
            (@ (class "table")
               (style "table-layout: fixed;"))
            (thead
             (tr
              (th "Name")
              (th "Version")
              (th "System")
              (th "Target")
              (th (@ (class "col-xs-5")) "Derivations (with build statuses)")
              (th "")))
            (tbody
             ,@(append-map
                (match-lambda
                  ((('name . name)
                    ('version . version)
                    ('base . base-derivations)
                    ('target . target-derivations))
                   (let* ((system-and-versions
                           (delete-duplicates
                            (append (map (lambda (details)
                                           (cons (assq-ref details 'system)
                                                 (assq-ref details 'target)))
                                         (vector->list base-derivations))
                                    (map (lambda (details)
                                           (cons (assq-ref details 'system)
                                                 (assq-ref details 'target)))
                                         (vector->list target-derivations)))))
                          (data-columns
                           (map
                            (match-lambda
                              ((system . target)
                               (let* ((base-entry
                                       (find (lambda (details)
                                               (and (string=? (assq-ref details 'system) system)
                                                    (string=? (assq-ref details 'target) target)))
                                             (vector->list base-derivations)))
                                      (base-derivation-file-name
                                       (assq-ref base-entry 'derivation-file-name))
                                      (base-builds
                                       (assq-ref base-entry 'builds))
                                      (target-entry
                                       (find (lambda (details)
                                               (and (string=? (assq-ref details 'system) system)
                                                    (string=? (assq-ref details 'target) target)))
                                             (vector->list target-derivations)))
                                      (target-derivation-file-name
                                       (assq-ref target-entry 'derivation-file-name))
                                      (target-builds
                                       (assq-ref target-entry 'builds)))
                                 `((td (samp (@ (style "white-space: nowrap;"))
                                             ,system))
                                   (td (samp (@ (style "white-space: nowrap;"))
                                             ,target))
                                   (td ,@(if base-derivation-file-name
                                             `((a (@ (style "display: block;")
                                                     (href ,base-derivation-file-name))
                                                  (span (@ (class "text-danger glyphicon glyphicon-minus pull-left")
                                                           (style "font-size: 1.5em; padding-right: 0.4em;")))
                                                  ,@(build-statuses->build-status-labels
                                                     (vector->list base-builds))
                                                  ,(display-store-item-short base-derivation-file-name)))
                                             '())
                                       ,@(if target-derivation-file-name
                                             `((a (@ (style "display: block; clear: left;")
                                                     (href ,target-derivation-file-name))
                                                  (span (@ (class "text-success glyphicon glyphicon-plus pull-left")
                                                           (style "font-size: 1.5em; padding-right: 0.4em;")))
                                                  ,@(build-statuses->build-status-labels
                                                     (vector->list target-builds))
                                                  ,(display-store-item-short target-derivation-file-name)))
                                             '()))
                                   (td (@ (style "vertical-align: middle;"))
                                       ,@(if (and base-derivation-file-name
                                                  target-derivation-file-name)
                                             `((a (@ (class "btn btn-sm btn-default")
                                                     (title "Compare")
                                                     (href
                                                      ,(string-append
                                                        "/compare/derivation?"
                                                        "base_derivation="
                                                        base-derivation-file-name
                                                        "&target_derivation="
                                                        target-derivation-file-name)))
                                                  "⇕ Compare"))
                                             '()))))))
                            system-and-versions)))

                     `((tr (td (@ (rowspan , (length system-and-versions)))
                               ,name)
                           (td (@ (rowspan , (length system-and-versions)))
                               ,version)
                           ,@(car data-columns))
                       ,@(map (lambda (data-row)
                                `(tr ,data-row))
                              (cdr data-columns))))))
                (vector->list derivation-changes)))))))))))

(define (compare/packages query-parameters
                          base-packages-vhash
                          target-packages-vhash)
  (define base-commit
    (assq-ref query-parameters 'base_commit))

  (define target-commit
    (assq-ref query-parameters 'target_commit))

  (define query-params
    (string-append "?base_commit=" base-commit
                   "&target_commit=" target-commit))

  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 "Comparing "
            (a (@ (href ,(string-append "/revision/" base-commit)))
               (samp ,(string-take base-commit 8) "…"))
            " and "
            (a (@ (href ,(string-append "/revision/" target-commit)))
               (samp ,(string-take target-commit 8) "…")))
        (a (@ (class "btn btn-default btn-lg")
              (href ,(string-append
                      "/compare/packages.json" query-params)))
           "View JSON")))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h3 "Base ("
            (samp ,base-commit)
            ")")
        (p "Packages found in the base revision.")
        (table
         (@ (class "table"))
         (thead
          (tr
           (th (@ (class "col-md-4")) "Name")
           (th (@ (class "col-md-4")) "Version")
           (th (@ (class "col-md-4")) "")))
         (tbody
          ,@(map
             (match-lambda
               ((name version)
                `(tr
                  (td ,name)
                  (td ,version)
                  (td (@ (class "text-right"))
                      (a (@ (href ,(string-append
                                    "/revision/" base-commit
                                    "/package/" name "/" version)))
                         "More information")))))
             (delete-duplicates
              (map (lambda (data)
                     (take data 2))
                   (vlist->list base-packages-vhash))))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h3 "Target ("
            (samp ,target-commit)
            ")")
        (p "Packages found in the target revision.")
        (table
         (@ (class "table"))
         (thead
          (tr
           (th (@ (class "col-md-4")) "Name")
           (th (@ (class "col-md-4")) "Version")
           (th (@ (class "col-md-4")) "")))
         (tbody
          ,@(map
             (match-lambda
               ((name version)
                `(tr
                  (td ,name)
                  (td ,version)
                  (td (@ (class "text-right"))
                      (a (@ (href ,(string-append
                                    "/revision/" target-commit
                                    "/package/" name "/" version)))
                         "More information")))))
             (delete-duplicates
              (map (lambda (data)
                     (take data 2))
                   (vlist->list target-packages-vhash))))))))))))

(define* (compare/system-test-derivations query-parameters
                                          mode
                                          valid-systems
                                          build-server-urls
                                          base-git-repositories
                                          target-git-repositories
                                          changes
                                          #:optional
                                          base-revision-details
                                          target-revision-details)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container-fluid"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        ,@(cond
           ((any-invalid-query-parameters? query-parameters)
            '((h3 "Comparing system test derivations")))
           ((eq? mode 'revision)
            (let ((base-commit (assq-ref query-parameters 'base_commit))
                  (target-commit (assq-ref query-parameters 'target_commit)))
              `((h3
                 (a (@ (href ,(string-append
                               "/compare?base_commit="
                               base-commit
                               "&target_commit="
                               target-commit)))
                    "Comparing "
                    (samp ,(string-take base-commit 8) "…")
                    " and "
                    (samp ,(string-take target-commit 8) "…"))))))
           ((eq? mode 'datetime)
            (let ((base-branch (assq-ref query-parameters 'base_branch))
                  (base-datetime (assq-ref query-parameters 'base_datetime))
                  (target-branch (assq-ref query-parameters 'target_branch))
                  (target-datetime (assq-ref query-parameters 'target_datetime)))
              `((h3
                 (a (@ (href ,(string-append
                               "/compare-by-datetime?"
                               (query-parameters->string
                                (filter (match-lambda
                                          ((key . _)
                                           (member key '(base_branch
                                                         base_datetime
                                                         target_branch
                                                         target_datetime))))
                                        query-parameters)))))
                    "Comparing "
                    (br)
                    (samp (*ENTITY* nbsp) (*ENTITY* nbsp)
                          ,base-branch
                          ,@(map (lambda _ '(*ENTITY* nbsp))
                                 (iota (max
                                        0
                                        (- (string-length target-branch)
                                           (string-length base-branch))))))
                    " at " ,(date->string base-datetime "~1 ~3")
                    " to "
                    (br)
                    (samp (*ENTITY* nbsp) (*ENTITY* nbsp)
                          ,target-branch
                          ,@(map (lambda _ '(*ENTITY* nbsp))
                                 (iota (max 0
                                            (- (string-length base-branch)
                                               (string-length target-branch))))))
                    " at " ,(date->string target-datetime "~1 ~3")))))))))
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
          ,@(compare-form-controls-for-mode mode query-parameters)
          ,(form-horizontal-control
            "System" query-parameters
            #:options valid-systems
            #:allow-selecting-multiple-options #f
            #:help-text "Only include derivations for this system."
            #:font-family "monospace")
          (div (@ (class "form-group form-group-lg"))
               (div (@ (class "col-sm-offset-2 col-sm-10"))
                    (button (@ (type "submit")
                               (class "btn btn-lg btn-primary"))
                            "Update results")))
          (a (@ (class "btn btn-default btn-lg pull-right")
                (href ,(let ((query-parameter-string
                              (query-parameters->string query-parameters)))
                         (string-append
                          "/"
                          (cond
                           ((eq? mode 'revision) "compare")
                           ((eq? mode 'datetime) "compare-by-datetime"))
                          "/system-test-derivations.json"
                          (if (string-null? query-parameter-string)
                              ""
                              (string-append "?" query-parameter-string))))))
             "View JSON")))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 "System test derivation changes")
        ,(if
          (null? changes)
          '(p "No system test derivation changes")
          `(table
            (@ (class "table")
               (style "table-layout: fixed;"))
            (thead
             (tr
              (th (@ (class "col-sm-2"))
                  "Name")
              (th (@ (class "col-sm-2"))
                  "Description")
              (th (@ (class "col-sm-2"))
                  "Location")
              (th "Derivation")
              (th (@ (class "col-sm-1"))
                  "")))
            (tbody
             ,@(append-map
                (match-lambda
                  ((('name        . name)
                    ('description . description-data)
                    ('derivation  . derivation-data)
                    ('location    . location-data)
                    ('builds      . builds-data))

                   (define (render-location git-repositories commit-hash
                                            data)
                     (map
                      (match-lambda
                        ((id label url cgit-url-base)
                         (if
                          (and cgit-url-base
                               (not (string-null? cgit-url-base)))
                          (match data
                            ((('file          . file)
                              ('line          . line)
                              ('column_number . column-number))
                             `(a (@ (href
                                     ,(string-append
                                       cgit-url-base "tree/"
                                       file "?id=" commit-hash
                                       "#n" (number->string line))))
                                 ,file
                                 " (line: " ,line
                                 ", column: " ,column-number ")")))
                          '())))
                      git-repositories))

                   (define cells
                     (list
                      (if (list? description-data)
                          (cons
                           `(td ,(assq-ref description-data 'base))
                           `(td ,(assq-ref description-data 'target)))
                          (cons
                           `(td (@ (rowspan 2))
                                ,description-data)
                           ""))
                      (if (assq-ref location-data 'base)
                          (cons
                           `(td ,(render-location
                                  base-git-repositories
                                  (assq-ref query-parameters 'base_commit)
                                  (assq-ref location-data 'base)))
                           `(td ,(render-location
                                  target-git-repositories
                                  (assq-ref query-parameters 'target_commit)
                                  (assq-ref location-data 'target))))
                          (cons
                           `(td (@ (rowspan 2))
                                ,(render-location
                                  target-git-repositories
                                  (assq-ref query-parameters 'target_commit)
                                  location-data))
                           ""))
                      (cons
                       (let ((base-derivation (assq-ref derivation-data 'base)))
                         `(td
                           (a (@ (style "display: block;")
                                 (href ,base-derivation))
                              (span (@ (class "text-danger glyphicon glyphicon-minus pull-left")
                                       (style "font-size: 1.5em; padding-right: 0.4em;")))
                              ,@(build-statuses->build-status-labels
                                 (vector->list (assq-ref builds-data 'base)))
                              ,(display-store-item-short base-derivation))))
                       (let ((target-derivation (assq-ref derivation-data 'target)))
                         `(td
                           (a (@ (style "display: block;")
                                 (href ,target-derivation))
                              (span (@ (class "text-success glyphicon glyphicon-plus pull-left")
                                       (style "font-size: 1.5em; padding-right: 0.4em;")))
                              ,@(build-statuses->build-status-labels
                                 (vector->list (assq-ref builds-data 'target)))
                              ,(display-store-item-short target-derivation)))))
                      (cons
                       `(td (@ (style "vertical-align: middle;")
                               (rowspan 2))
                            (a (@ (class "btn btn-sm btn-default")
                                  (title "Compare")
                                  (href
                                   ,(string-append
                                     "/compare/derivation?"
                                     "base_derivation="
                                     (assq-ref derivation-data 'base)
                                     "&target_derivation="
                                     (assq-ref derivation-data 'target))))
                               "⇕ Compare"))
                       "")))

                   `((tr
                      (td (@ (rowspan 2))
                          ,name)
                      ,@(map car cells))
                     (tr
                      ,@(map cdr cells)))))
                changes))))))))))
