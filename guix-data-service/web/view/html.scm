;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (guix-data-service web view html)
  #:use-module (guix-data-service config)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service web html-utils)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:use-module (json)
  #:export (layout
            header
            form-horizontal-control

            display-possible-store-item
            display-store-item
            display-store-item-short

            table/branches-with-most-recent-commits

            index
            readme
            general-not-found
            view-statistics
            view-builds
            view-derivation
            view-formatted-derivation
            view-narinfos
            view-store-item
            view-derivation-source-file
            error-page))

(define* (header)
  `(nav
    (@ (id "header") (class "navbar navbar-default"))
    (div
     (@ (class "container-fluid"))
     (div
      (@ (class "navbar-header"))
      (div (@ (class "navbar-brand"))
           (a (@ (href "/") (class "logo"))))))))

(define* (layout #:key
                 (head '())
                 (body '())
                 (title "Guix Data Service")
                 description)
  `((doctype "html")
    (html
     (@ (lang "en"))
     (head
      (title ,title)
      (meta (@ (http-equiv "Content-Type")
               (content "text/html; charset=UTF-8")))
      (meta (@ (name "viewport")
               (content "width=device-width, initial-scale=1")))
      ,@(if description
            `((meta
               (@ (name "description")
                  (content ,description))))
            '())
      (link
       (@ (rel "stylesheet")
          (media "screen")
          (type "text/css")
          (href "/assets/css/bootstrap.css")))
      ,@head
      (link
       (@ (rel "stylesheet")
          (media "screen")
          (type "text/css")
          (href "/assets/css/screen.css"))))
     (body (a (@ (id "top")))
           ,@body
           (footer
            (p "Copyright © 2016—2020 by the GNU Guix community."
               (br)
               "Now with even more " (span (@ (class "lambda")) "λ") "! ")
            (p "This is free software.  Download the "
               (a (@ (href "https://git.savannah.gnu.org/cgit/guix/data-service.git/"))
                  "source code here") "."))))))

(define* (form-horizontal-control label query-parameters
                                  #:key
                                  name
                                  help-text
                                  required?
                                  options
                                  (allow-selecting-multiple-options #t)
                                  font-family
                                  (type "text")
                                  (null-string-value "none"))
  (define (value->text value)
    (match value
      (#f "")
      ((? date? date)
       (date->string date "~1 ~3"))
      (other other)))

  (let* ((input-id    (hyphenate-words
                       (string-downcase label)))
         (help-span-id (string-append
                        input-id "-help-text"))
         (input-name (or name
                         (underscore-join-words
                          (string-downcase label))))
         (has-error? (let ((val
                            (assq-ref query-parameters
                                      (string->symbol input-name))))
                       (if (list? val)
                           (any invalid-query-parameter? val)
                           (invalid-query-parameter? val))))
         (show-help-span?
          (or help-text has-error? required?)))
    (if (string=? type "hidden")
        `(input (@ (class "form-control")
                   (id ,input-id)
                   (type ,type)
                   (name ,input-name)
                   ,@(match (assq (string->symbol input-name)
                                  query-parameters)
                       (#f '())
                       ((_key . value)
                        `((value ,(value->text value)))))))
        `(div
          (@ (class ,(string-append
                      "form-group form-group-lg"
                      (if has-error? " has-error" ""))))
          (label (@ (for ,input-id)
                    (class "col-sm-2 control-label"))
                 ,label)
          (div
           (@ (class "col-sm-9"))
           ,(if options
                `(select (@ (class "form-control")
                            (style ,(if font-family
                                        (string-append
                                         "font-family: " font-family ";")
                                        ""))
                            ,@(if allow-selecting-multiple-options
                                  '((multiple #t))
                                  '())
                            (id ,input-id)
                            ,@(if show-help-span?
                                  `((aria-describedby ,help-span-id))
                                  '())

                            (name ,input-name))
                   ,@(let ((selected-options
                            (match (assq (string->symbol input-name)
                                         query-parameters)
                              ((_key . value)
                               (if (not allow-selecting-multiple-options)
                                   (list value)
                                   value))
                              (_ '()))))

                       (map (match-lambda
                              ((option-label . option-value)
                               `(option
                                 (@ ,@(if (member (if (and
                                                       (string? option-value)
                                                       (string=? option-value
                                                                 null-string-value))
                                                      ""
                                                      option-value)
                                                  selected-options)
                                          '((selected ""))
                                          '())
                                    (value ,option-value))
                                 ,(value->text option-label)))
                              (option-value
                               `(option
                                 (@ ,@(if (member (if (and
                                                       (string? option-value)
                                                       (string=? option-value
                                                                 null-string-value))
                                                      ""
                                                      option-value)
                                                  selected-options)
                                          '((selected ""))
                                          '()))
                                 ,(value->text option-value))))
                            options)))
                `(input (@ (class "form-control")
                           (style ,(if font-family
                                       (string-append
                                        "font-family: " font-family ";")
                                       ""))
                           (id ,input-id)
                           (type ,type)
                           ,@(if required?
                                 '((required #t))
                                 '())
                           ,@(if show-help-span?
                                 `((aria-describedby ,help-span-id))
                                 '())
                           (name ,input-name)
                           ,@(match (assq (string->symbol input-name)
                                          query-parameters)
                               (#f '())
                               ((_key . ($ <invalid-query-parameter> value))
                                (if (string=? type "checkbox")
                                    (if value
                                        '((checked #t))
                                        '())
                                    `((value ,(value->text value)))))
                               ((_key . value)
                                (if (string=? type "checkbox")
                                    (if value
                                        '((checked #t))
                                        '())
                                    `((value ,(value->text value)))))))))
           ,@(if show-help-span?
                 `((span (@ (id ,help-span-id)
                            (class "help-block"))
                         ,@(if has-error?
                               (let* ((val
                                       (assq-ref query-parameters
                                                 (string->symbol input-name)))
                                      (messages
                                       (map invalid-query-parameter-message
                                            (if (list? val)
                                                val
                                                (list val)))))
                                 `((p
                                    ,@(if (null? messages)
                                          '(string "Error: invalid value")
                                          (map
                                           (lambda (message)
                                             `(strong
                                               (@ (style "display: block;"))
                                               "Error: "
                                               ,@(if (list? message)
                                                     message
                                                     (list message))))
                                           (remove (lambda (v)
                                                     (eq? #f v))
                                                   messages))))))
                               '())
                         ,@(if required? '((strong "Required. ")) '())
                         ,@(if help-text
                               (list help-text)
                               '())))
                 '()))))))

(define (readme contents)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 "The README document")))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (raw ,contents)))))))

(define (index git-repositories-and-revisions)
  (layout
   #:description
   "The Guix Data Service processes, stores and provides data about Guix over
time."
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 (@ (style "display: inline-block;"))
            "Guix Data Service")
        (div
         (@ (class "btn-group pull-right")
            (style "margin-top: 1.3rem;")
            (role "group"))
         (a (@ (class "btn btn-lg btn-default")
               (href "/statistics")
               (role "button"))
            "Statistics")
         (a (@ (class "btn btn-lg btn-default")
               (href "/jobs")
               (role "button"))
            "Jobs"))))
      ,@(map
         (match-lambda
           (((repository-id label url cgit-url-base) . branches-with-most-recent-commits)
            `(div
              (@ (class "row"))
              (div
               (@ (class "col-sm-12"))
               (h3 (@ (style "display: inline-block;"))
                   ,url)
               ,@(if (string-null? cgit-url-base)
                     '()
                     `((a (@ (style "padding-left: 0.8em;")
                             (href ,cgit-url-base))
                          "(View cgit)")))
               ,(if (null? branches-with-most-recent-commits)
                    '(p "No branches")
                    (table/branches-with-most-recent-commits
                     repository-id
                     (filter (lambda (data)
                               (not (string-null? (second data))))
                             branches-with-most-recent-commits)))))))
         git-repositories-and-revisions)))))

(define (view-statistics guix-revisions-count derivations-count)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-6"))
        (h3 "Guix revisions")
        (strong (@ (class "text-center")
                   (style "font-size: 2em; display: block;"))
                ,guix-revisions-count))
       (div
        (@ (class "col-md-6"))
        (h3 "Derivations")
        (strong (@ (class "text-center")
                   (style "font-size: 2em; display: block;"))
                ,derivations-count)))))))

(define (table/branches-with-most-recent-commits
         git-repository-id branches-with-most-recent-commits)
  `(table
    (@ (class "table table-responsive"))
    (thead
     (tr
      (th (@ (class "col-md-3")) "Name")
      (th (@ (class "col-md-2")) "Date")
      (th (@ (class "col-md-7")) "Commit")))
    (tbody
     ,@(map
        (match-lambda
          ((name commit date revision-exists? job-events)
           `(tr
             (td
              (a (@ (href ,(string-append
                            "/repository/" (number->string git-repository-id)
                            "/branch/" name)))
                 ,name))
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
                                   "No information yet")))))))))
        branches-with-most-recent-commits))))

(define (display-possible-store-item value)
  (match (string-split value #\/)
    (("" "gnu" "store" item)
     `(a (@ (href ,value))
         ,(display-store-item value)))
    (("" "gnu" "store" item fileparts ...)
     `(a (@ (href ,value))
         ,(display-file-in-store-item-oneline value)))
    (_
     `(span (@ (style "font-family: monospace;"))
            ,value))))

(define (display-store-item-short item)
  `((span (@ (style "font-size: small; font-family: monospace; display: block;"))
          ,(string-take item 44))
    (span (@ (style "font-size: x-large; font-family: monospace; display: block;"))
          ,(string-drop item 44))))

(define (display-store-item item)
  `((span (@ (style "font-size: small; font-family: monospace; white-space: nowrap;"))
          ,(string-take item 44))
    (span (@ (style "font-size: x-large; font-family: monospace; white-space: nowrap;"))
          ,(string-drop item 44))))

(define (display-store-item-title item)
  `(h1 (span (@ (style "font-size: 1em; font-family: monospace; display: block;"))
             ,(string-take item 44))
       (span (@ (style "line-height: 1.7em; font-size: 1.5em; font-family: monospace;"))
             ,(string-drop item 44))))

(define (display-file-in-store-item filename)
  (match (string-split filename #\/)
    (("" "gnu" "store" item fileparts ...)
     `(,(let ((full-item (string-append "/gnu/store/" item)))
          `(a (@ (href ,full-item))
              ,(display-store-item-short full-item)))
       ,(string-append
         "/" (string-join fileparts "/"))))))

(define (display-file-in-store-item-oneline filename)
  (match (string-split filename #\/)
    (("" "gnu" "store" item fileparts ...)
     `(,(let ((full-item (string-append "/gnu/store/" item)))
          `(a (@ (href ,full-item))
              ,(display-store-item full-item)))
       ,(string-append
         "/" (string-join fileparts "/"))))))

(define (view-store-item filename
                         derivations
                         derivations-using-store-item-list
                         nars
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
        ,(display-store-item-title filename)))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h2 "Nars")
        (div
         (@ (class "btn-group pull-right")
            (role group))
         (a (@ (class "btn btn-default btn-lg")
               (href ,(string-append filename "/narinfos")))
            "View narinfo details")
         (a (@ (class "btn btn-lg btn-default")
               (href ,(string-append filename "/json"))
               (role "button"))
            "View JSON"))
        ,@(map
           (match-lambda
             ((hash-algorithm hash size urls signatures)
              `(div
                (h4 (@ (style "font-family: monospace;"))
                    ,hash)
                (table
                 (@ (class "table")
                    (style "table-layout: fixed;"))
                 (thead
                  (tr
                   (th (@ (class "col-sm-1")) "Size")
                   (th (@ (class "col-sm-4")) "Urls")))
                 (tbody
                  (td ,size)
                  (td
                   (ul
                    ,@(map
                       (lambda (url-details)
                         `(li
                           "Size: " ,(assoc-ref url-details "size")
                           " Compression: " ,(assoc-ref url-details "compression")
                           " "
                           (a (@ (href ,(assoc-ref url-details "url")))
                              ,(assoc-ref url-details "url"))))
                       urls)))
                  (td
                   ,@(map
                      (lambda (signature)
                        `(dl
                          (@ (class "dl-horizontal"))
                          (dt "Version")
                          (dd ,(assoc-ref signature "version"))
                          (dt "Host name")
                          (dd ,(assoc-ref signature "host_name"))))
                      signatures)))))))
           nars)))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h2 "Builds")
        ,@(map
           (match-lambda
             ((build-server-id build-server-url build-server-build-id
                               derivation-file-name
                               timestamp status)
              `(a (@ (href ,(build-url build-server-id
                                       build-server-build-id
                                       derivation-file-name)))
                  ,(build-status-span status))))
           builds)))
      ,@(map (lambda (derivation derivations-using-store-item)
               `((div
                  (@ (class "row"))
                  (div
                   (@ (class "col-sm-12"))
                   (h4 "Derivation: ")
                   ,(match derivation
                      ((file-name output-id)
                       `(a (@ (href ,file-name))
                           ,(display-store-item file-name))))))
                 (div
                  (@ (class "row"))
                  (div
                   (@ (class "col-sm-12"))
                   (h2 "Derivations using this store item "
                       ,(let ((count (length derivations-using-store-item)))
                          (if (eq? count 100)
                              "(> 100)"
                              (simple-format #f "(~A)" count))))
                   (ul
                    (@ (class "list-unstyled"))
                    ,(map
                      (match-lambda
                        ((file-name)
                         `(li (a (@ (href ,file-name))
                                 ,(display-store-item file-name)))))
                      derivations-using-store-item))))))
             derivations
             derivations-using-store-item-list)))))

(define (view-derivation-source-file filename details)
  (define labels
    '((compression       . "Compression")
      (hash_algorithm    . "Hash algorithm")
      (hash              . "Hash")
      (uncompressed_size . "Uncompressed size")
      (compressed_size   . "Compressed size")))

  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        ,(display-store-item-title filename)
        (h4 "Derivation source file")
        ,@(if details
              `((dl
                 (@ (class "dl-horizontal"))
                 ,@(append-map
                    (match-lambda
                      ((key . value)
                       `((dt ,(assq-ref labels key))
                         (dd ,(if (eq? key 'hash)
                                  `(span (@ (style "font-family: monospace"))
                                         ,value)
                                  value)))))
                    details)))
              '())))))))

(define (view-derivation derivation derivation-inputs derivation-outputs
                         builds)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      ,(match derivation
         ((id file-name builder args env-vars system)
          `(div
            (@ (class "row"))
            (div
             (@ (class "col-sm-12"))
             ,(display-store-item-title file-name)
             (div
              (@ (class "btn-group pull-right")
                 (role "group"))
              (a (@ (class "btn btn-lg btn-default disabled")
                    (href ,file-name)
                    (role "button"))
                 "Detail view")
              (a (@ (class "btn btn-lg btn-default")
                    (href ,(string-append file-name "/formatted"))
                    (role "button"))
                 "Formatted view")
              (a (@ (class "btn btn-lg btn-default")
                    (href ,(string-append file-name "/plain"))
                    (role "button"))
                 "Plain view")
              (a (@ (class "btn btn-lg btn-default")
                    (href ,(string-append file-name "/json"))
                    (role "button"))
                 "View JSON"))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-4"))
        (h3 "Inputs")
        ,(if (null? derivation-inputs)
             "No inputs"
             `(table
               (@ (class "table"))
               (thead
                (tr
                 (th "File name")))
               (tdata
                ,@(map (match-lambda
                         ((derivation-file-name outputs)
                          `(tr
                            (td (a (@ (href ,derivation-file-name))
                                   ,(display-store-item-short derivation-file-name))))))
                       derivation-inputs)))))
       (div
        (@ (class "col-md-4"))
        (h3 "Derivation details")
        ,(match derivation
           ((id file-name builder args env-vars system)
            `(table
              (@ (class "table"))
              (tbody
               (tr
                (td "Builder")
                (td ,(if (string=? "builtin:download"
                                   builder)
                         "builtin:download"
                         `(a (@ (href ,builder))
                             ,(display-file-in-store-item builder)))))
               (tr
                (td "System")
                (td (samp ,system)))))))
        (h3 "Build status")
        ,@(if (null? builds)
              `((div
                 (@ (class "text-center"))
                 ,(build-status-span "")))
              (map
               (match-lambda
                 ((build-id build-server-id
                            build-server-url
                            build-server-build-id
                            derivation-file-name
                            timestamp
                            status)
                  `(div
                    (@ (class "text-center"))
                    (div
                     (a (@ (href ,(build-url build-server-id
                                             build-server-build-id
                                             derivation-file-name)))
                        ,(build-status-span status)))
                    (a (@ (style "display: inline-block; margin-top: 0.4em;")
                          (href ,(build-server-link-url
                                  build-server-url
                                  build-server-build-id
                                  derivation-file-name)))
                       "View build on " ,build-server-url))))
               builds)))
       (div
        (@ (class "col-md-4"))
        (h3 "Outputs")
        (table
         (@ (class "table"))
         (thead
          (tr
           (th "File name")))
         (tdata
          ,@(map (match-lambda
                   ((output-name path hash-algorithm hash recursive?)
                    `(tr
                      (td (a (@ (href ,path))
                             ,(display-store-item-short path))))))
                 derivation-outputs)))))))))

(define (view-formatted-derivation derivation derivation-inputs derivation-outputs
                                   derivation-sources)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      ,(match derivation
         ((id file-name builder args env-vars system)
          `(div
            (@ (class "row"))
            (div
             (@ (class "col-sm-12"))
             ,(display-store-item-title file-name)
             (div
              (@ (class "btn-group pull-right")
                 (role "group"))
              (a (@ (class "btn btn-lg btn-default")
                    (href ,file-name)
                    (role "button"))
                 "Detail view")
              (a (@ (class "btn btn-lg btn-default disabled")
                    (href ,(string-append file-name "/formatted"))
                    (role "button"))
                 "Formatted view")
              (a (@ (class "btn btn-lg btn-default")
                    (href ,(string-append file-name "/plain"))
                    (role "button"))
                 "Plain view"))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-offset-2 col-md-10")
           (style "font-family: monospace; font-size: 1.5em;"))
        "Derive("))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-offset-2 col-md-10")
           (style "font-family: monospace;"))
        (span (@ (style "margin-left: 1.5em;"))
              "[")))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-2"))
        "Outputs")
       (div
        (@ (class "col-md-10")
           (style "font-family: monospace;"))
        ,@(map (match-lambda*
                 (((output-name path hash-algorithm hash recursive?) count-down)
                  `(div
                    (@ (style "margin-left: 3em;"))
                    ,(simple-format #f "(\"~A\",\"" output-name)
                    (a (@ (href ,path))
                       ,(display-store-item path))
                    "\","
                    ,(string-append (if recursive? "\"r:" "\"")
                                    hash-algorithm)
                    "\""
                    ","
                    "\"" ,hash "\""
                    ")"
                    ,@(if (eq? count-down 0)
                          '()
                          '(",")))))
               derivation-outputs
               (reverse (iota (length derivation-outputs))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-offset-2 col-md-10")
           (style "font-family: monospace;"))
        (span (@ (style "margin-left: 1.5em;"))
              "],[")))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-2"))
        "Inputs")
       (div
        (@ (class "col-md-10")
           (style "font-family: monospace;"))
        ,@(map (match-lambda*
                 (((file-name outputs) count-down)
                  `(div
                    (@ (style "margin-left: 3em;"))
                    "(\""
                    (a (@ (href ,file-name))
                       ,(display-store-item file-name))
                    "\",\""
                    "[" ,(string-join
                          (map (lambda (output)
                                 (string-append
                                  "\""
                                  (assoc-ref output "output_name")
                                  "\""))
                               (vector->list outputs))
                          ",")
                    "]"
                    ")"
                    ,@(if (eq? count-down 0)
                          '()
                          '(",")))))
               derivation-inputs
               (reverse (iota (length derivation-inputs))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-offset-2 col-md-10")
           (style "font-family: monospace;"))
        (span (@ (style "margin-left: 1.5em;"))
              "],[")))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-2"))
        "Sources")
       (div
        (@ (class "col-md-10")
           (style "font-family: monospace;"))
        ,@(map (lambda (source count-down)
                 `(div (@ (style "margin-left: 3em;"))
                       "\""
                       (a (@ (href ,source))
                          ,(display-store-item source))
                       "\""
                       ,@(if (eq? count-down 0)
                             '()
                             '(","))))
               derivation-sources
               (reverse (iota (length derivation-sources))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-offset-2 col-md-10")
           (style "font-family: monospace;"))
        (span (@ (style "margin-left: 1.5em;"))
              "],")))
      ,@(match derivation
          ((id file-name builder args env-vars system)
           `((div
              (@ (class "row"))
              (div
               (@ (class "col-md-2"))
               "System")
              (div
               (@ (class "col-md-10")
                  (style "font-family: monospace;"))
               (span (@ (style "margin-left: 1.5em;"))
                     "\"" ,system "\",")))
             (div
              (@ (class "row"))
              (div
               (@ (class "col-md-2"))
               "Builder")
              (div
               (@ (class "col-md-10")
                  (style "font-family: monospace;"))
               (span (@ (style "margin-left: 1.5em;"))
                     ,@(if (string=? "builtin:download"
                                     builder)
                           '("builtin:download")
                           `("\""
                             (a (@ (href ,builder))
                                ,(display-file-in-store-item-oneline builder))
                             "\""))
                     ",")))
             (div
              (@ (class "row"))
              (div
               (@ (class "col-md-offset-2 col-md-10")
                  (style "font-family: monospace;"))
               (span (@ (style "margin-left: 1.5em;"))
                     "[")))
             (div
              (@ (class "row"))
              (div
               (@ (class "col-md-2"))
               "Arguments")
              (div
               (@ (class "col-md-10")
                  (style "font-family: monospace;"))
               (div
                (@ (style "margin-left: 3em;"))
                ,@(map (lambda (arg count-down)
                         `(div "\""
                               ,(display-possible-store-item arg)
                               "\""
                               ,@(if (eq? count-down 0)
                                     '()
                                     '(","))))
                       args
                       (reverse (iota (length args)))))))
             (div
              (@ (class "row"))
              (div
               (@ (class "col-md-offset-2 col-md-10")
                  (style "font-family: monospace;"))
               (span (@ (style "margin-left: 1.5em;"))
                     "],[")))
             (div
              (@ (class "row"))
              (div
               (@ (class "col-md-2"))
               "Environment variables")
              (div
               (@ (class "col-md-10")
                  (style "font-family: monospace;"))
               ,@(map (lambda (env-var count-down)
                        `(div (@ (style "margin-left: 3em;"))
                              "("
                              "\"" ,(assq-ref env-var 'key) "\""
                              ","
                              "\"" ,(display-possible-store-item
                                     (assq-ref env-var 'value))
                              "\""
                              ")"))
                      env-vars
                      (reverse (iota (length env-vars))))
               (span (@ (style "margin-left: 1.5em;"))
                     "]")))
             (div
              (@ (class "row"))
              (div
               (@ (class "col-md-offset-2 col-md-10")
                  (style "font-family: monospace; font-size: 1.5em;"))
               ")")))))))))

(define (view-narinfos narinfos)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      ,@(map
         (match-lambda
           ((hash-algorithm hash size urls signatures)
            `((div
               (@ (class "row"))
               (div
                (@ (class "col-sm-6"))
                (h4 (@ (style "font-family: monospace;"))
                    ,hash)
                (table
                 (@ (class "table")
                    (style "table-layout: fixed;"))
                 (thead
                  (tr
                   (th (@ (class "col-sm-1")) "Size")
                   (th (@ (class "col-sm-4")) "Urls")))
                 (tbody
                  (td ,size)
                  (td
                   (ul
                    ,@(map
                       (lambda (url-details)
                         `(li
                           "Size: " ,(assoc-ref url-details "size")
                           " Compression: " ,(assoc-ref url-details "compression")
                           " "
                           (a (@ (href ,(assoc-ref url-details "url")))
                              ,(assoc-ref url-details "url"))))
                       urls)))))))
              ,@(map
                 (lambda (signature)
                   `(div
                     (@ (class "row"))
                     (div
                      (@ (class "col-sm-6"))
                      (dl
                       (@ (class "dl-horizontal"))
                       (dt "Version")
                       (dd ,(assoc-ref signature "version"))
                       (dt "Host name")
                       (dd ,(assoc-ref signature "host_name")))

                      "data"
                      ,(sexp-div (assoc-ref signature "data"))
                      "sig_val"
                      ,(sexp-div (assoc-ref signature "sig_val"))
                      "public_key"
                      ,(sexp-div
                        (assoc-ref signature "narinfo_signature_public_key")))
                     (div
                      (@ (class "col-sm-6"))
                      (pre ,(assoc-ref signature "body"))
                      (pre ,(assoc-ref signature "signature_line")))))
                 signatures))))
         narinfos)))))

(define (general-not-found header-text body)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (h1 ,header-text)
      (p ,body)))))

(define* (error-page #:optional error)
  (layout
   #:body
   `(,(header)
     (div (@ (class "container"))
          (h1 "An error occurred")
          (p "Sorry about that!")
          ,@(if error
                (match error
                  ((key . args)
                   `((b ,key)
                     (pre ,args))))
                '())))))
