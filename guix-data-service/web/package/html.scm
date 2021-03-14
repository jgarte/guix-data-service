;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2021 Christopher Baines <mail@cbaines.net>
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

(define-module (guix-data-service web package html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service web html-utils)
  #:use-module (guix-data-service web view html)
  #:export (view-package))

(define* (view-package name package-version-with-branches)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (h1 "Package: " ,name)))
      ,@(map
         (match-lambda
           ((('version . version)
             ('branches . branches))
            `(div
              (@ (class "row"))
              (div
               (@ (class "col-md-12"))
               (h3 ,version)
               (ul
                (@ (class "list-inline"))
                ,@(map
                   (lambda (branch)
                     `((li
                        (a
                         (@
                          (href
                           ,(simple-format
                             #f
                             "/repository/~A/branch/~A/latest-processed-revision/package/~A/~A"
                             (assoc-ref branch "git_repository_id")
                             (assoc-ref branch "name")
                             name
                             version)))
                         ,(assoc-ref branch "name")))))
                   (vector->list branches)))))))
         (vector->list package-version-with-branches))))))
