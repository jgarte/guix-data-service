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

(define-module (guix-data-service web build controller)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service utils)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service model build)
  #:use-module (guix-data-service model build-status)
  #:use-module (guix-data-service model build-server)
  #:use-module (guix-data-service web build html)
  #:export (build-controller))

(define (parse-build-status status)
  (if (member status build-status-strings)
      status
      (make-invalid-query-parameter
       status
       (string-append "unknown build status: "
                      status))))

(define parse-build-server
  (lambda (v)
    (letpar& ((build-servers
               (with-thread-postgresql-connection
                select-build-servers)))
      (or (any (match-lambda
                 ((id url lookup-all-derivations? lookup-builds?)
                  (if (eq? (string->number v)
                           id)
                      id
                      #f)))
               build-servers)
          (make-invalid-query-parameter
           v
           "unknown build server")))))

(define (build-controller request
                          method-and-path-components
                          mime-types
                          body)
  (match method-and-path-components
    (('GET "builds")
     (render-builds request
                    mime-types))
    (_ #f)))

(define (render-builds request mime-types)
  (let ((parsed-query-parameters
         (parse-query-parameters
          request
          `((build_status ,parse-build-status #:multi-value)
            (build_server ,parse-build-server #:multi-value)))))
    (if (any-invalid-query-parameters? parsed-query-parameters)
        (render-html
         #:sxml (view-builds parsed-query-parameters
                             build-status-strings
                             '()
                             '()
                             '()))
        (letpar& ((build-server-options
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (map (match-lambda
                             ((id url lookup-all-derivations
                                  lookup-builds)
                              (cons url id)))
                           (select-build-servers conn)))))
                  (build-stats
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-build-stats
                       conn
                       (assq-ref parsed-query-parameters
                                 'build_server)))))
                  (builds-with-context
                   (with-thread-postgresql-connection
                    (lambda (conn)
                      (select-builds-with-context
                       conn
                       (assq-ref parsed-query-parameters
                                 'build_status)
                       (assq-ref parsed-query-parameters
                                 'build_server)
                       #:limit 50)))))

          (render-html
           #:sxml (view-builds parsed-query-parameters
                               build-status-strings
                               build-server-options
                               build-stats
                               builds-with-context))))))
