;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2014 David Thompson <davet@gnu.org>
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

;; This code was snarfed from David Thompson's guix-web.

(define-module (guix-data-service web render)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 binary-ports)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (json)
  #:use-module (guix-data-service config)
  #:use-module (guix-data-service web sxml)
  #:use-module (guix-data-service web util)
  #:export (static-asset-from-store-renderer
            render-static-asset
            render-html
            render-json
            render-text
            not-found
            unprocessable-entity
            created
            no-content
            redirect))

(define file-mime-types
  '(("css" . (text/css))
    ("js"  . (text/javascript))
    ("svg" . (image/svg+xml))
    ("png" . (image/png))
    ("gif" . (image/gif))
    ("woff" . (application/font-woff))
    ("ttf"  . (application/octet-stream))
    ("html" . (text/html))))

(define (static-asset-from-store-renderer)
  (define last-modified
    ;; Use the process start time as the last modified time, as the file
    ;; metadata in the store is normalised.
    (current-time))

  (define files
    (file-system-fold
     (const #t)                         ; enter
     (lambda (filename stat result)
       (let ((relative-filename (string-drop filename
                                             (+ 1 ; to account for the /
                                                (string-length
                                                 (%config 'assets-dir))))))
         (cons (cons relative-filename
                     (call-with-input-file filename
                       get-bytevector-all))
               result)))
     (lambda (name stat result) result) ; down
     (lambda (name stat result) result) ; up
     (lambda (name stat result) result) ; skip
     (lambda (name stat errno result)
       (error name))
     '()
     (%config 'assets-dir)))

  (define (send-file path contents)
    (list `((content-type
             . ,(assoc-ref file-mime-types
                           (file-extension path)))
            (last-modified . ,(time-utc->date last-modified))
            (cache-control . (public
                              ;; Set the max-age at 5 minutes, as the files
                              ;; could change when the code changes
                              (max-age . ,(* 60 5)))))
          contents))

  (lambda (path headers)
    (and=> (assoc-ref files path)
           (lambda (contents)
             (cond ((assoc-ref headers 'if-modified-since)
                    =>
                    (lambda (client-date)
                      (if (time>? last-modified
                                  (date->time-utc client-date))
                          (send-file path contents)
                          (list (build-response #:code 304) ; "Not Modified"
                                #f))))
                   (else
                    (send-file path contents)))))))

(define (render-static-asset path headers)
  (render-static-file (%config 'assets-dir) path headers))

(define %not-slash
  (char-set-complement (char-set #\/)))

(define (render-static-file root path headers)
  (let ((file-name (string-append root "/" path)))
    (if (not (any (cut string-contains <> "..")
                  (string-tokenize path %not-slash)))
        (let* ((stat (stat file-name #f))
               (modified (and stat
                              (make-time time-utc 0 (stat:mtime stat)))))
          (define (send-file)
            (list `((content-type
                     . ,(assoc-ref file-mime-types
                                   (file-extension file-name)))
                    (last-modified . ,(time-utc->date modified)))
                  (call-with-input-file file-name get-bytevector-all)))

          (if (and stat (not (eq? 'directory (stat:type stat))))
              (cond ((assoc-ref headers 'if-modified-since)
                     =>
                     (lambda (client-date)
                       (if (time>? modified (date->time-utc client-date))
                           (send-file)
                           (list (build-response #:code 304) ;"Not Modified"
                                 #f))))
                    (else
                     (send-file)))
              #f))
        #f)))

(define* (render-html #:key sxml (extra-headers '())
                      (code 200))
  (list (build-response
         #:code code
         #:headers (append extra-headers
                           '((content-type . (text/html))
                             (vary . (accept)))))
        (lambda (port)
          (sxml->html sxml port))))

(define* (render-json json #:key (extra-headers '())
                      (code 200))
  (list (build-response
         #:code code
         #:headers (append extra-headers
                           '((content-type . (application/json))
                             (vary . (accept)))))
        (lambda (port)
          (scm->json json port))))

(define* (render-text text #:key (extra-headers '())
                      (code 200))
  (list (build-response
         #:code code
         #:headers (append extra-headers
                           '((content-type . (text/plain))
                             (vary . (accept)))))
        (lambda (port)
          (display text port))))

(define (not-found uri)
  (list (build-response #:code 404)
        (string-append "Resource not found: " (uri->string uri))))

(define (unprocessable-entity)
  (list (build-response #:code 422)
        ""))

(define (created)
  (list (build-response #:code 201)
        ""))

(define (no-content)
  (list (build-response #:code 204)
        ""))

(define (redirect path)
  (let ((uri (build-uri 'http
                        #:host (%config 'host)
                        #:port (%config 'port)
                        #:path (string-append
                                "/" (encode-and-join-uri-path path)))))
    (list (build-response
           #:code 301
           #:headers `((content-type . (text/html))
                       (location . ,uri)))
          (format #f "Redirect to ~a" (uri->string uri)))))
