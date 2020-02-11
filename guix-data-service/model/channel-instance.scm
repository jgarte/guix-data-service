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

(define-module (guix-data-service model channel-instance)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (json)
  #:use-module (guix utils)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service model derivation)
  #:export (insert-channel-instances))

(define (insert-channel-instances conn
                                  guix-revision-id
                                  derivations-by-system)
  (let ((derivation-ids
         (derivation-file-names->derivation-ids
          conn
          (map cdr derivations-by-system))))

    (exec-query
     conn
     (string-append
      "
INSERT INTO channel_instances
  (guix_revision_id, system, derivation_id)
VALUES "
      (string-join
       (map (lambda (system derivation-id)
              (simple-format #f "(~A, '~A', ~A)"
                             guix-revision-id
                             system
                             derivation-id))
            (map car derivations-by-system)
            derivation-ids)
       ", "))))
  #t)
