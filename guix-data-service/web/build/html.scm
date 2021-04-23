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

(define-module (guix-data-service web build html)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service web html-utils)
  #:use-module (guix-data-service web view html)
  #:export (view-builds))

(define (view-builds query-parameters
                     build-status-strings
                     build-server-options
                     valid-systems
                     valid-targets
                     stats
                     builds)
  (define page-header "Builds")

  (layout
   #:title
   page-header
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 ,page-header)
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
                           `(td ,(or (assq-ref counts-by-build-server-id
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
          ,(form-horizontal-control
            "Limit results" query-parameters
            #:help-text "The maximum number of results to return.")
          ,(form-horizontal-control
            "All results" query-parameters
            #:type "checkbox"
            #:help-text "Return all results")
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
           (th (@ (class "col-xs-10")) "Derivation")
           (th (@ (class "col-xs-1")) "Timestamp")
           (th (@ (class "col-xs-1")) "")))
         (tbody
          ,@(map
             (match-lambda
               ((build-id build-server-url build-server-build-id
                          derivation-file-name
                          timestamp status)
                `(tr
                  (td (@ (class "text-center"))
                      (a (@ (href
                             ,(build-url
                               (assoc-ref build-server-options build-server-url)
                               build-server-build-id
                               derivation-file-name)))
                         ,(build-status-span status)))
                  (td (a (@ (href ,derivation-file-name))
                         ,(display-store-item-short derivation-file-name)))
                  (td ,timestamp)
                  (td (a (@ (href ,(build-server-link-url
                                    build-server-url
                                    build-server-build-id
                                    derivation-file-name)))
                         "View build on " ,build-server-url)))))
             builds)))))))))
