;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2021 Christopher Baines <mail@cbaines.net>
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

(define-module (guix-data-service web package controller)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (guix-data-service utils)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service model package)
  #:use-module (guix-data-service web package html)
  #:export (package-controller))

(define (package-controller request
                            method-and-path-components
                            mime-types
                            body)
  (match method-and-path-components
    (('GET "package" name)
     (let ((parsed-query-parameters
            (parse-query-parameters
             request
             `((system ,parse-system #:default "x86_64-linux")
               (target ,parse-target #:default "")))))
       (letpar& ((package-versions-with-branches
                  (with-thread-postgresql-connection
                   (lambda (conn)
                     (branches-by-package-version conn name
                                                  (assq-ref parsed-query-parameters
                                                            'system)
                                                  (assq-ref parsed-query-parameters
                                                            'target))))))
         (case (most-appropriate-mime-type
                '(application/json text/html)
                mime-types)
           ((application/json)
            (render-json
             `((name . ,name)
               (versions . ,package-versions-with-branches))))
           (else
            (render-html
             #:sxml
             (view-package name package-versions-with-branches)))))))))


