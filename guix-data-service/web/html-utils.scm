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
  #:use-module (ice-9 match)
  #:export (sexp-div))

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
