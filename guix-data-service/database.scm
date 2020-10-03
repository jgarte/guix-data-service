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
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (squee)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (guix-data-service config)
  #:export (with-postgresql-connection

            make-postgresql-connection-channel
            close-postgresql-connection-channel
            exec-query/through-channel
            with-postgresql-transaction/through-channel

            with-postgresql-transaction

            check-test-database!

            with-advisory-session-lock
            obtain-advisory-transaction-lock

            exec-query-with-null-handling))

;; TODO This isn't exported for some reason
(define pg-conn-finish
  (@@ (squee) pg-conn-finish))

(define (open-postgresql-connection name statement-timeout)
  (define paramstring
    (string-append
     (or (getenv "GUIX_DATA_SERVICE_DATABASE_PARAMSTRING")
         (simple-format
          #f "dbname=~A user=~A"
          (%config 'database-name)
          (%config 'database-user)))
     " application_name='guix-data-service " name "'"))

  (let* ((conn (connect-to-postgres-paramstring
                (or (getenv "GUIX_DATA_SERVICE_DATABASE_URI")
                    paramstring))))
    (when statement-timeout
      (exec-query
       conn
       (simple-format #f "SET statement_timeout = ~A"
                      statement-timeout)))

    conn))

(define* (with-postgresql-connection name f #:key (statement-timeout #f))
  (let ((conn (open-postgresql-connection name statement-timeout)))
    (with-throw-handler
        #t
      (lambda ()
        (call-with-values
            (lambda ()
              (f conn))
          (lambda vals
            (pg-conn-finish conn)
            (apply values vals))))
      (lambda (key . args)
        (pg-conn-finish conn)))))

(define* (make-postgresql-connection-channel name
                                             #:key
                                             (statement-timeout #f)
                                             (threads 2))
  (parameterize (((@@ (fibers internal) current-fiber) #f))
    (let ((channel (make-channel)))
      (for-each
       (lambda _
         (call-with-new-thread
          (lambda ()
            (with-postgresql-connection
             name
             (lambda (conn)
               (let loop ()
                 (match (get-message channel)
                   (((? channel? reply) f (? boolean? allways-rollback?))
                    (put-message
                     reply
                     (with-exception-handler
                         (lambda (exn)
                           (cons 'worker-thread-error exn))
                       (lambda ()
                         (with-exception-handler
                             (lambda (exn)
                               (simple-format
                                (current-error-port)
                                "postgresql connection thread: exception: ~A\n"
                                exn)
                               (backtrace)
                               (raise-exception exn))
                           (lambda ()
                             (call-with-values
                                 (lambda ()
                                   (with-postgresql-transaction
                                    conn
                                    (lambda (conn)
                                      (f conn))))
                               (lambda vals vals)))))
                       #:unwind? #t))
                    (loop))
                   (((? channel? reply) . (? list? args))
                    (put-message
                     reply
                     (with-exception-handler
                         (lambda (exn)
                           (cons 'worker-thread-error exn))
                       (lambda ()
                         (with-exception-handler
                             (lambda (exn)
                               (simple-format
                                (current-error-port)
                                "postgresql connection thread: exception: ~A\n"
                                exn)
                               (backtrace)
                               (raise-exception exn))
                           (lambda ()
                             (call-with-values
                                 (lambda ()
                                   (apply exec-query
                                          conn
                                          args))
                               (lambda vals vals)))))
                       #:unwind? #t))
                    (loop))
                   (_ #f))))
             #:statement-timeout statement-timeout))))
       (iota threads))
      channel)))

(define (close-postgresql-connection-channel channel)
  (put-message channel #f))

(define (exec-query/through-channel channel . args)
  (let ((reply (make-channel)))
    (put-message channel (cons reply args))
    (match (get-message reply)
      (('worker-thread-error . exn)
       (raise-exception exn))
      (result
       (apply values result)))))

(define* (with-postgresql-transaction/through-channel channel
                                                      f
                                                      #:key always-rollback?)
  (let ((reply (make-channel)))
    (put-message channel (list reply f always-rollback?))
    (match (get-message reply)
      (('worker-thread-error . exn)
       (raise-exception exn))
      (result
       (apply values result)))))

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

(define (check-test-database! conn)
  (match (exec-query conn "SELECT current_database()")
    (((name))
     (unless (string=? name "guix_data_service_test")
       (error "tests being run against non test database")))))

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
