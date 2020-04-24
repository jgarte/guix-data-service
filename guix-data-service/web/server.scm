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
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (web http)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (system repl error-handling)
  #:use-module (fibers web server)
  #:use-module (guix-data-service web controller)
  #:use-module (guix-data-service web util)
  #:export (start-guix-data-service-web-server))

(define (handler request body controller secret-key-base
                 postgresql-statement-timeout)
  (display
   (format #f "~a ~a\n"
           (request-method request)
           (uri-path (request-uri request))))
  (apply values
         (let-values (((request-components mime-types)
                       (request->path-components-and-mime-type request)))
           (controller request
                       (cons (request-method request)
                             request-components)
                       mime-types
                       body
                       secret-key-base
                       #:postgresql-statement-timeout
                       postgresql-statement-timeout))))

(define* (start-guix-data-service-web-server port host secret-key-base
                                             #:key postgresql-statement-timeout)
  (call-with-error-handling
   (lambda ()
     (run-server (lambda (request body)
                   (handler request body controller
                            secret-key-base
                            postgresql-statement-timeout))
                 #:host host
                 #:port port))
   #:on-error 'backtrace
   #:post-error (lambda (key . args)
                  (when (eq? key 'system-error)
                    (match args
                      (("bind" "~A" ("Address already in use") _)
                       (simple-format
                        (current-error-port)
                        "\n
error: guix-data-service could not start, as it could not bind to port ~A

Check if it's already running, or whether another process is using that
port. Also, the port used can be changed by passing the --port option.\n"
                        port)))))))
