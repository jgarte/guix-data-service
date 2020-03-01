;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2020 Christopher Baines <mail@cbaines.net>
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

(define-module (guix-data-service web dumps controller)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (guix-data-service config)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web dumps html)
  #:export (dumps-controller))

(define (dumps-controller request
                          method-and-path-components
                          mime-types
                          body
                          conn)
  (match method-and-path-components
    (('GET "dumps")
     (render-dumps request
                   mime-types))
    (('GET "dumps" "latest" file)
     (render-latest-dumps request file))
    (('GET "dumps" _ ...)
     (list (build-response #:code 504)
           "requests for individual files should be handled before the request
reaches the Guix Data Service"))
    (_ #f)))

(define (available-dumps)
  (define (enter? name stat result)
    (or (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
                      (basename name))
        (string=? (%config 'dumps-dir)
                  name)))

  (define (leaf name stat result)
    (match result
      (() result)
      (((directory-name . files) other-directories ...)
       `((,directory-name . ,(sort (cons (basename name) files)
                                   string<?))
         ,@other-directories))))

  (define (down name stat result)
    (if (string=? (%config 'dumps-dir)
                  name)
        result
        `((,(basename name) . ())
          ,@result)))

  (define (up name stat result) result)
  (define (skip name stat result) result)

  (define (error name stat errno result)
    (format (current-error-port) "warning: ~a: ~a~%"
            name (strerror errno))
    result)

  (sort (file-system-fold enter? leaf down up skip error
                          '()                 ; Start with an empty alist
                          (%config 'dumps-dir)
                          (lambda args
                            ;; Use stat, then fall back to lstat if that fails
                            (catch #t
                              (lambda ()
                                (apply stat args))
                              (lambda _
                                (apply lstat args)))))
        (lambda (a b)
          ;; Sort so that the recent dumps are first
          (string>? (car a) (car b)))))

(define (render-dumps request mime-types)
  (render-html
   #:sxml (view-dumps (available-dumps))))

(define (render-latest-dumps request file)
  (or (any (match-lambda
             ((date-string . files)
              (if (member file files)
                  (let ((uri
                         (build-uri
                          #f
                          #:path (string-append
                                  "/"
                                  (encode-and-join-uri-path
                                   (list "dumps" date-string file)))
                          #:validate? #f)))
                    (list (build-response
                           #:code 302
                           #:headers `((content-type . (text/html))
                                       (location . ,uri)))
                          (format #f "Redirect to ~a" (uri->string uri))))
                  #f)))
           (available-dumps))
      (not-found (request-uri request))))




