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
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt pk-crypto)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (guix pki)
  #:use-module (guix base32)
  #:use-module (guix base64)
  #:use-module (guix serialization)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web nar html)
  #:use-module (guix-data-service model derivation)
  #:export (nar-controller

            %narinfo-signing-private-key
            %narinfo-signing-public-key))


(define %narinfo-signing-private-key
  (make-parameter #f))

(define %narinfo-signing-public-key
  (make-parameter #f))

(define %nix-cache-info
  `(("StoreDir" . "/gnu/store")
    ("WantMassQuery" . 0)
    ("Priority" . 100)))

(define (nar-controller request
                        method-and-path-components
                        mime-types
                        body
                        conn)
  (define (.narinfo-suffix s)
    (string-suffix? ".narinfo" s))

  (match method-and-path-components
    (('GET "nix-cache-info")
     (render-text
      (string-concatenate
       (map (match-lambda
              ((key . value)
               (format #f "~a: ~a~%" key value)))
            %nix-cache-info))))
    (('GET "substitutes")
     (render-html
      #:sxml (view-substitutes (%narinfo-signing-public-key))))
    (('GET "nar" file-name)
     (render-nar request
                 mime-types
                 conn
                 (string-append "/gnu/store/" file-name)))
    (('GET "nar" "lzip" file-name)
     (render-lzip-nar request
                      mime-types
                      conn
                      (string-append "/gnu/store/" file-name)))
    (('GET (? .narinfo-suffix path))
     (render-narinfo request
                     conn
                     (string-drop-right path
                                        (string-length ".narinfo"))))
    (_ #f)))

(define (render-nar request
                    mime-types
                    conn
                    file-name)
  (or
   (and=> (select-serialized-derivation-by-file-name conn
                                                     file-name)
          (lambda (derivation-text)
            (let ((derivation-bytevector
                   (string->bytevector derivation-text
                                       "ISO-8859-1")))
              (list (build-response
                     #:code 200
                     #:headers '((content-type . (application/x-nix-archive
                                                  (charset . "ISO-8859-1")))))
                    (lambda (port)
                      (write-file-tree
                       file-name
                       port
                       #:file-type+size
                       (lambda (file)
                         (values 'regular
                                 (bytevector-length derivation-bytevector)))
                       #:file-port
                       (lambda (file)
                         (open-bytevector-input-port derivation-bytevector))))))))
   (not-found (request-uri request))))

(define (render-lzip-nar request
                         mime-types
                         conn
                         file-name)
  (or
   (and=> (select-derivation-source-file-nar-data-by-file-name conn file-name)
          (lambda (data)
            (list (build-response
                   #:code 200
                   #:headers '((content-type . (application/x-nix-archive
                                                (charset . "ISO-8859-1")))))
                  (lambda (port)
                    (put-bytevector port data)))))
   (not-found (request-uri request))))

(define (render-narinfo request
                        conn
                        hash)
  (or
   (and=> (select-derivation-by-file-name-hash conn
                                               hash)
          (lambda (derivation)
            (list (build-response
                   #:code 200
                   #:headers '((content-type . (application/x-narinfo))))
                  (let* ((derivation-file-name
                          (second derivation))
                         (derivation-text
                          (select-serialized-derivation-by-file-name
                           conn
                           derivation-file-name))
                         (derivation-bytevector
                          (string->bytevector derivation-text
                                              "ISO-8859-1"))
                         (derivation-references
                          (select-derivation-references-by-derivation-id
                           conn
                           (first derivation)))
                         (nar-bytevector
                          (call-with-values
                              (lambda ()
                                (open-bytevector-output-port))
                            (lambda (port get-bytevector)
                              (write-file-tree
                               derivation-file-name
                               port
                               #:file-type+size
                               (lambda (file)
                                 (values 'regular
                                         (bytevector-length derivation-bytevector)))
                               #:file-port
                               (lambda (file)
                                 (open-bytevector-input-port derivation-bytevector)))
                              (get-bytevector)))))
                    (lambda (port)
                      (display (narinfo-string derivation-file-name
                                               nar-bytevector
                                               derivation-references)
                               port))))))
   (and=> (select-derivation-source-file-data-by-file-name-hash conn
                                                                hash)
          (match-lambda
            ((store-path compression compressed-size
                         hash-algorithm hash uncompressed-size)
             (list (build-response
                    #:code 200
                    #:headers '((content-type . (application/x-narinfo))))
                   (lambda (port)
                     (display (derivation-source-file-narinfo-string store-path
                                                                     compression
                                                                     compressed-size
                                                                     hash-algorithm
                                                                     hash
                                                                     uncompressed-size)
                              port))))))
   (not-found (request-uri request))))


(define* (narinfo-string store-item
                         nar-bytevector
                         references
                         #:key
                         (nar-path "nar"))
  (define (signed-string s)
    (let* ((public-key (%narinfo-signing-public-key))
           (hash (bytevector->hash-data (sha256 (string->utf8 s))
                                        #:key-type (key-type public-key))))
      (signature-sexp hash (%narinfo-signing-private-key) public-key)))

  (let* ((hash       (bytevector->nix-base32-string
                      (sha256 nar-bytevector)))
         (size       (bytevector-length nar-bytevector))
         (references (string-join
                      (map basename references)
                      " "))
         (info (format #f
                             "\
StorePath: ~a
URL: ~a
Compression: none
NarHash: sha256:~a
NarSize: ~d
References: ~a~%"
                             store-item
                             (encode-and-join-uri-path
                              (list nar-path
                                    (basename store-item)))
                             hash
                             size
                             references)))
    (if (%narinfo-signing-private-key)
        (format #f "~aSignature: 1;~a;~a~%"
                info
                (gethostname)
                (base64-encode
                 (string->utf8
                  (canonical-sexp->string (signed-string info)))))
        info)))

(define* (derivation-source-file-narinfo-string store-item
                                                compression
                                                compressed-size
                                                hash-algorithm
                                                hash
                                                uncompressed-size
                                                #:key (nar-path "nar"))
  (define (signed-string s)
    (let* ((public-key (%narinfo-signing-public-key))
           (hash (bytevector->hash-data (sha256 (string->utf8 s))
                                        #:key-type (key-type public-key))))
      (signature-sexp hash (%narinfo-signing-private-key) public-key)))

  (let* ((info (format #f
                             "\
StorePath: ~a
URL: ~a
Compression: ~a
FileSize: ~d
NarHash: ~a:~a
NarSize: ~d
References: ~%"
                             store-item
                             (encode-and-join-uri-path
                              (list nar-path
                                    compression
                                    (basename store-item)))
                             compression
                             compressed-size
                             hash-algorithm
                             hash
                             uncompressed-size)))
    (if (%narinfo-signing-private-key)
        (format #f "~aSignature: 1;~a;~a~%"
                info
                (gethostname)
                (base64-encode
                 (string->utf8
                  (canonical-sexp->string (signed-string info)))))
        info)))
