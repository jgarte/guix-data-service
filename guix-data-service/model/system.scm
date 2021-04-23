;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2019, 2020, 2021 Christopher Baines <mail@cbaines.net>
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

(define-module (guix-data-service model system)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix-data-service model utils)
  #:export (system->system-id
            list-systems))

(define system->system-id-cache
  (make-hash-table))

(define systems-cache #f)

(define (system->system-id conn system)
  (let ((cached-value (hash-ref system->system-id-cache
                                system)))
    (or cached-value
        (match (insert-missing-data-and-return-all-ids
                conn
                "systems"
                '(system)
                `((,system)))
          ((id)
           (hash-set! system->system-id-cache
                      system
                      id)
           (set! systems-cache #f)
           id)))))

(define (list-systems conn)
  (if systems-cache
      systems-cache
      (let ((systems
             (map car
                  (exec-query
                   conn
                   "SELECT system FROM systems ORDER BY system"))))
        (set! systems-cache systems)
        systems)))
