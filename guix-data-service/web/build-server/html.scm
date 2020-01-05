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

(define-module (guix-data-service web build-server html)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service web view html)
  #:use-module (guix-data-service web html-utils)
  #:export (view-build
            view-build-server
            view-signing-key))

(define (view-build query-parameters
                    build
                    required-failed-builds)
  (define derivation
    (assq-ref query-parameters 'derivation_file_name))

  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 "Build")))
      (div
       (@ (class "row"))
       ,@(match build
           ((url statuses)
            `((div
               (@ (class "col-sm-6"))
               (dl
                (@ (class "dl-horizontal"))
                (dt "Derivation")
                (dd ,(display-possible-store-item derivation))
                (dt "Build server URL")
                (dd (a (@ (href ,url))
                       ,url))))
              (div
               (@ (class "col-sm-6"))
               (h3 "Timeline")
               (table
                (@ (class "table"))
                (thead
                 (tr
                  (th "Timestamp")
                  (th "Status")))
                (tbody
                 ,@(map (lambda (status)
                          `(tr
                            (td ,(assoc-ref status "timestamp"))
                            (td ,(build-status-span
                                  (assoc-ref status "status")))))
                        (vector->list statuses)))))))))
      ,@(if required-failed-builds
            `((div
               (@ (class "row"))
               (div
                (@ (class "col-sm-6"))
                (h3 "Required failed builds")
                (table
                 (@ (class "table"))
                 (thead
                  (tr
                   (th "Derivation")
                   (th "Status")))
                 (tbody
                  ,@(map (match-lambda
                           ((derivation status)
                            `(tr
                              (td ,(display-possible-store-item derivation))
                              (td ,(build-status-span status)))))
                         required-failed-builds))))))
            '())))))

(define (view-build-server build-server)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h2 "Build server")
        ,(match build-server
           ((url lookup-all-derivations?)
            `(dl
              (@ (class "dl-horizontal"))
              (dt "URL")
              (dd (a (@ (href ,url))
                     ,url))
              (dt "Lookup all " (br) "derivations?")
              (dd ,(if lookup-all-derivations?
                       "Yes"
                       "No")))))))))))

(define (view-signing-key sexp)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h2 "Signing key")
        ,(sexp-div sexp)))))))
