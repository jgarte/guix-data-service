;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2016, 2017  Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2014  David Thompson <davet@gnu.org>
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

(define-module (guix-data-service web util)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (web request)
  #:use-module (web uri)
  #:export (most-appropriate-mime-type
            request->path-components-and-mime-type
            file-extension
            directory?

            hyphenate-words
            underscore-join-words))

(define (most-appropriate-mime-type accepted-mime-types
                                    supported-mime-types)
  (or
   ;; Pick the first supported mime-type
   (find (lambda (accepted-mime-type)
           (memq accepted-mime-type
                 supported-mime-types))
         accepted-mime-types)
   ;; Default to the first supported mime-type if none are accepted
   (first supported-mime-types)))

(define (request->path-components-and-mime-type request)
  (define extensions-to-mime-types
    '(("json" . application/json)
      ("html" . text/html)))

  (define (ends-with-recognised-extension? path)
    (any (lambda (extension)
           (string-suffix? (string-append "." extension)
                           path))
         (map car extensions-to-mime-types)))

  (match (split-and-decode-uri-path (uri-path (request-uri request)))
    (()
     (values '()
             (or (request-accept request)
                 (list 'text/html))))
    ((single-component)
     (if (ends-with-recognised-extension? single-component)
         (match (string-split single-component #\.)
           ((first-parts ... extension)
            (values (list (string-join first-parts "."))
                    (or (cons
                         (assoc-ref extensions-to-mime-types extension)
                         (or (request-accept request)
                             (list 'text/html)))))))
         (values (list single-component)
                 (or (request-accept request)
                     (list 'text/html)))))
    ((first-components ... last-component)
     (if (ends-with-recognised-extension? last-component)
         (match (string-split last-component #\.)
           ((first-parts ... extension)
            (values (append first-components
                            (list (string-join first-parts ".")))
                    (or (cons
                         (assoc-ref extensions-to-mime-types extension)
                         (or (request-accept request)
                             (list 'text/html)))))))
         (values (append first-components
                         (list last-component))
                 (or (request-accept request)
                     (list 'text/html)))))))

(define (file-extension file-name)
  (last (string-split file-name #\.)))

(define (directory? filename)
  (string=? filename (dirname filename)))

(define (hyphenate-words words)
  (string-join
   (string-split words #\space)
   "-"))

(define (underscore-join-words words)
  (string-join
   (string-split words #\space)
   "_"))
