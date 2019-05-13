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

(define-module (guix-data-service model location)
  #:use-module (ice-9 match)
  #:use-module (guix utils)
  #:use-module (squee)
  #:export (location->location-id))

(define select-existing-location
  (string-append
   "SELECT id "
   "FROM locations "
   "WHERE file = $1 AND line = $2 AND column_number = $3"))

(define insert-location
  (string-append
   "INSERT INTO locations "
   "(file, line, column_number) VALUES "
   "($1, $2, $3) "
   "RETURNING id"))

(define (location->location-id conn location)
  (match location
    (($ <location> file line column)
     (match (exec-query conn
                        select-existing-location
                        (list file
                              (number->string line)
                              (number->string column)))
       (((id))
        (string->number id))
       (()
        (string->number
         (caar
          (exec-query conn
                      insert-location
                      (list file
                            (number->string line)
                            (number->string column))))))))))
