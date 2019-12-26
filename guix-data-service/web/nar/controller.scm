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

(define-module (guix-data-service web nar controller)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (guix serialization)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service model derivation)
  #:export (nar-controller))

(define (nar-controller request
                          method-and-path-components
                          mime-types
                          body
                          conn)
  (match method-and-path-components
    (('GET "nar" derivation)
     (render-nar request
                 mime-types
                 conn
                 (string-append "/gnu/store/" derivation)))
    (_ #f)))

(define (render-nar request
                    mime-types
                    conn
                    derivation-file-name)
  (let ((derivation-text
         (select-serialized-derivation-by-file-name
          conn
          derivation-file-name)))
    (if derivation-text
        (let ((derivation-bytevector
               (string->bytevector derivation-text
                                   "ISO-8859-1")))
          (list (build-response
                 #:code 200
                 #:headers '((content-type . (application/x-nix-archive
                                              (charset . "ISO-8859-1")))))
                (lambda (port)
                  (write-file-tree
                   derivation-file-name
                   port
                   #:file-type+size
                   (lambda (file)
                     (values 'regular
                             (bytevector-length derivation-bytevector)))
                   #:file-port
                   (lambda (file)
                     (open-bytevector-input-port derivation-bytevector))))))
        (not-found (request-uri request)))))
