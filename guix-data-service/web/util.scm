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
  #:export (request-path-components
            file-extension
            directory?

            hyphenate-words
            underscore-join-words))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

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
