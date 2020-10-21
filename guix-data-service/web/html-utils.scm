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

(define-module (guix-data-service web html-utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service web query-parameters)
  #:export (sexp-div

            next-page-link

            build-status-value->display-string
            build-status-span
            build-url
            build-server-link-url
            build-status-alist->build-icon))

(define (sexp-div sexp)
  (match sexp
    (#(val rest ...)
     `(div (@ (style "margin-left: 1em;"))
           "( "
           ,val
           " "
           ,@(map sexp-div rest)
           " )"))
    ((("base16" . hash))
     `(span (@ (style "font-family: monospace;"))
            ,hash))
    ((and string val)
     val)))

(define (next-page-link path
                        query-parameters
                        field
                        value)
  (string-append
   path
   "?"
   (query-parameters->string
    `((,field . ,value)
      ,@(alist-delete
         field
         query-parameters)))))

(define (build-status-value->display-string value)
  (assoc-ref
   '(("scheduled" . "Scheduled")
     ("started" . "Started")
     ("succeeded" . "Succeeded")
     ("failed" . "Failed")
     ("failed-dependency" . "Failed (dependency)")
     ("failed-other" . "Failed (other)")
     ("canceled" . "Canceled")
     ("" . "Unknown"))
   value))

(define (build-url build-server-id build-server-build-id derivation-file-name)
  (if (and (string? build-server-build-id)
           (not (string-null? build-server-build-id)))
      (simple-format
       #f "/build-server/~A/build?build_server_build_id=~A"
       build-server-id
       build-server-build-id)
      (simple-format
       #f "/build-server/~A/build?derivation_file_name=~A"
       build-server-id
       derivation-file-name)))

(define (build-server-link-url url-base
                               build-server-build-id
                               derivation-file-name)
  (string-append
   url-base
   (if (string-suffix? "/" url-base)
       ""
       "/")
   "build/"
   (if (and (string? build-server-build-id)
            (eq? (string-length build-server-build-id)
                 36))                   ; crude UUID check
       build-server-build-id
       (string-drop
        derivation-file-name
        (string-length "/gnu/store/")))))

(define (build-status-span status)
  `(span (@ (class ,(string-append
                     "label label-"
                     (assoc-ref
                      '(("scheduled" . "info")
                        ("started" . "primary")
                        ("succeeded" . "success")
                        ("failed" . "danger")
                        ("failed-dependency" . "warning")
                        ("failed-other" . "danger")
                        ("canceled" . "default")
                        ("" . "default"))
                      status)))
            (style "display: inline-block; font-size: 1.2em; margin-top: 0.4em; margin-bottom: 0.4em;"))
         ,(build-status-value->display-string status)))

(define (build-status-alist->build-icon status)
  (build-status-span (assoc-ref status "status")))
