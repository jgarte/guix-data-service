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
  #:use-module (ice-9 match)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service model build)
  #:use-module (guix-data-service model build-status)
  #:use-module (guix-data-service web build html)
  #:export (build-controller))

(define (parse-build-status status)
  (if (member status build-status-strings)
      status
      (make-invalid-query-parameter
       status
       (string-append "unknown build status: "
                      status))))

(define (build-controller request
                          method-and-path-components
                          mime-types
                          body
                          conn)
  (match method-and-path-components
    (('GET "builds")
     (render-builds request
                    mime-types
                    conn))
    (_ #f)))

(define (render-builds request mime-types conn)
  (let ((parsed-query-parameters
         (parse-query-parameters
          request
          `((build_status ,parse-build-status #:multi-value)))))
    (if (any-invalid-query-parameters? parsed-query-parameters)
        (render-html
         #:sxml (view-builds parsed-query-parameters
                             build-status-strings
                             '()
                             '()))
        (render-html
         #:sxml (view-builds parsed-query-parameters
                             build-status-strings
                             (select-build-stats conn)
                             (select-builds-with-context
                              conn
                              (assq-ref parsed-query-parameters
                                        'build_status)))))))
