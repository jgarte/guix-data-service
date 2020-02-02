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

(define-module (guix-data-service model system-test)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix utils)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service model location)
  #:use-module (guix-data-service model derivation)
  #:export (insert-system-tests-for-guix-revision))

(define (insert-system-tests-for-guix-revision conn
                                               guix-revision-id
                                               system-test-data)
  (let ((system-test-ids
         (insert-missing-data-and-return-all-ids
          conn
          "system_tests"
          '(name description location_id)
          (map (match-lambda
                 ((name description derivation-file-name location-data)
                  (list name
                        description
                        (location->location-id
                         conn
                         (apply location location-data)))))
               system-test-data)))
        (derivation-ids
         (derivation-file-names->derivation-ids
          conn
          (map third system-test-data))))

    (exec-query
     conn
     (string-append
      "
INSERT INTO guix_revision_system_test_derivations
  (guix_revision_id, system_test_id, derivation_id)
VALUES "
      (string-join
       (map (lambda (system-test-id derivation-id)
              (simple-format #f "(~A, ~A, ~A)"
                             guix-revision-id
                             system-test-id
                             derivation-id))
            system-test-ids
            derivation-ids)
       ", "))))
  #t)
