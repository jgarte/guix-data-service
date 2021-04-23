;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2020 Christopher Baines <mail@cbaines.net>
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

(define-module (guix-data-service web dumps html)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service web html-utils)
  #:use-module (guix-data-service web view html)
  #:export (view-dumps))

(define (view-dumps available-dumps)
  (define page-header "Database dumps")

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
        (h1 ,page-header)))
      ,@(map
         (match-lambda
           ((date-string . files)
            `(div
              (@ (class "row"))
              (div
               (@ (class "col-sm-12"))
               (h3 ,date-string)
               (ul
                ,@(map (lambda (name)
                         `(li (a (@ (href ,(string-join
                                            `("/dumps" ,date-string ,name)
                                            "/")))
                                 ,name)))
                       files))))))
         available-dumps)))))
