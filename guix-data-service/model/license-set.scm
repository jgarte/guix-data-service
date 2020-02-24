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

(define-module (guix-data-service model license-set)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (squee)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service model license)
  #:export (inferior-packages->license-set-ids))

(define select-license-sets
  "
SELECT id, license_ids
FROM license_sets")

(define (insert-license-sets license-id-lists)
  (string-append
   "INSERT INTO license_sets (license_ids) VALUES "
   (string-join
    (map (lambda (license-ids)
           (string-append
            "('{"
            (string-join
             (map number->string
                  (sort license-ids <))
             ", ")
            "}')"))
         license-id-lists)
    ", ")
   " RETURNING id"))

(define (inferior-packages->license-set-ids conn license-id-lists)
  (let* ((unique-license-id-lists (delete-duplicates
                                   license-id-lists))
         (existing-license-sets
          (exec-query->vhash conn
                             select-license-sets
                             (lambda (results)
                               (if (string=? (second results) "{}")
                                   '()
                                   (map
                                    string->number
                                    (string-split
                                     (string-drop-right
                                      (string-drop (second results) 1)
                                      1)
                                     #\,))))
                             (lambda (result)
                               (string->number (first result))))) ;; id
         (missing-license-sets
          (delete-duplicates
           (filter (lambda (license-set-license-ids)
                     (not (vhash-assoc license-set-license-ids
                                       existing-license-sets)))
                   unique-license-id-lists)))
         (new-license-set-entries
          (if (null? missing-license-sets)
              '()
              (map (lambda (result)
                     (string->number (first result)))
                   (exec-query conn
                               (insert-license-sets missing-license-sets)))))
         (new-entries-id-lookup-vhash
          (two-lists->vhash missing-license-sets
                            new-license-set-entries)))

    (map (lambda (license-id-list)
           (cdr
            (or (vhash-assoc license-id-list
                             existing-license-sets)
                (vhash-assoc license-id-list
                             new-entries-id-lookup-vhash)
                (begin
                  (error "missing license set entry"
                         license-id-list)))))
         license-id-lists)))
