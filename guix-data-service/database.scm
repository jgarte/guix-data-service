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

(define-module (guix-data-service database)
  #:use-module (squee)
  #:export (with-postgresql-connection
            with-postgresql-transaction))

;; TODO This isn't exported for some reason
(define pg-conn-finish
  (@@ (squee) pg-conn-finish))

(define (with-postgresql-connection f)
  (define paramstring
    (or (getenv "GUIX_DATA_SERVICE_DATABASE_PARAMSTRING")
        "dbname=guix_data_service user=guix_data_service"))

  (let* ((conn (connect-to-postgres-paramstring paramstring)))
    (with-throw-handler
      #t
      (lambda ()
        (let ((result (f conn)))
          (pg-conn-finish conn)
          result))
      (lambda (key . args)
        (pg-conn-finish conn)))))

(define* (with-postgresql-transaction conn f
                                      #:key always-rollback?)
  (exec-query conn "BEGIN;")

  (with-throw-handler #t
    (lambda ()
      (let ((result (f conn)))
        (exec-query conn (if always-rollback?
                             "ROLLBACK;"
                             "COMMIT;"))
        result))
    (lambda (key . args)
      (exec-query conn "ROLLBACK;"))))
