;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix-data-service web server)
  #:use-module (srfi srfi-1)
  #:use-module (web http)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (squee)
  #:use-module (fibers web server)
  #:use-module (guix-data-service web controller)
  #:use-module (guix-data-service web util)
  #:export (start-guix-data-service-web-server))

;; TODO This isn't exported for some reason
(define pg-conn-finish
  (@@ (squee) pg-conn-finish))

(define (with-postgresql-connection paramstring f)
  (let* ((conn (connect-to-postgres-paramstring paramstring)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (f conn))
      (lambda ()
        (pg-conn-finish conn)))))

(define (run-controller controller request body)
  (with-postgresql-connection
   "dbname=guix_data_service"
   (lambda (conn)
     ((controller request body conn)
      (cons (request-method request)
            (request-path-components request))))))

(define (handler request body controller)
  (format #t "~a ~a\n"
          (request-method request)
          (uri-path (request-uri request)))
  (apply values
         (run-controller controller request body)))

(define (start-guix-data-service-web-server port)
  (run-server (lambda (request body)
                (handler request body controller))
              #:addr INADDR_ANY
              #:port port))