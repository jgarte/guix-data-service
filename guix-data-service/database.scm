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
  #:use-module (system foreign)
  #:use-module (squee)
  #:export (with-postgresql-connection
            with-postgresql-transaction

            with-advisory-session-lock
            obtain-advisory-transaction-lock

            exec-query-with-null-handling))

;; TODO This isn't exported for some reason
(define pg-conn-finish
  (@@ (squee) pg-conn-finish))

(define* (with-postgresql-connection name f)
  (define paramstring
    (string-append
     (or (getenv "GUIX_DATA_SERVICE_DATABASE_PARAMSTRING")
         "dbname=guix_data_service user=guix_data_service")
     " application_name='guix-data-service " name "'"))

  (let* ((conn (connect-to-postgres-paramstring
                (or (getenv "GUIX_DATA_SERVICE_DATABASE_URI")
                    paramstring))))
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

(define (with-advisory-session-lock conn lock f)
  (let ((lock-number (number->string (symbol-hash lock))))
    (exec-query conn
                "SELECT pg_advisory_lock($1)"
                (list lock-number))
    (with-throw-handler #t
      (lambda ()
        (let ((result (f)))
          (exec-query conn
                      "SELECT pg_advisory_unlock($1)"
                      (list lock-number))
          result))
      (lambda (key . args)
        (exec-query conn
                    "SELECT pg_advisory_unlock($1)"
                    (list lock-number))))))

(define (obtain-advisory-transaction-lock conn lock)
  (let ((lock-number (number->string (symbol-hash lock))))
    (exec-query conn
                "SELECT pg_advisory_xact_lock($1)"
                (list lock-number))))

(define squee/libpq
  (@@ (squee) libpq))

(define squee/unwrap-result-ptr
  (@@ (squee) unwrap-result-ptr))

(define %PQgetisnull
  (pointer->procedure int
                      (dynamic-func "PQgetisnull" squee/libpq)
                      (list '* int int)))

(define (result-serializer-simple-list-with-null-handling result-ptr)
  "Get a simple list of lists representing the result of the query"
  (let ((rows-range (iota (result-num-rows result-ptr)))
        (cols-range (iota (result-num-cols result-ptr))))
    (map
     (lambda (row-i)
       (map
        (lambda (col-i)
          (let ((val (result-get-value result-ptr row-i col-i)))
            (if (string-null? val)
                (if (eq? 1 (%PQgetisnull
                            (squee/unwrap-result-ptr result-ptr) row-i col-i))
                    '()
                    val)
                val)))
        cols-range))
     rows-range)))

(define* (exec-query-with-null-handling pg-conn command #:optional (params '()))
  (exec-query pg-conn command params
              #:serializer result-serializer-simple-list-with-null-handling))
