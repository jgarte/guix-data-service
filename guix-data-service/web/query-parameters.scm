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

(define-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service web util)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-19)
  #:use-module (web request)
  #:use-module (web uri)
  #:export (parse-query-string

            <invalid-query-parameter>
            make-invalid-query-parameter
            invalid-query-parameter?
            invalid-query-parameter-value
            invalid-query-parameter-message

            any-invalid-query-parameters?

            parse-query-parameters

            parse-datetime
            parse-result-limit))

(define (parse-query-string query)
  "Parse and decode the URI query string QUERY and return an alist."
  (let lp ((lst (map uri-decode (string-split query (char-set #\& #\=)))))
    (match lst
      ((key value . rest)
       (cons (cons key value) (lp rest)))
      (("") '())
      (() '()))))

(define-immutable-record-type <invalid-query-parameter>
  (make-invalid-query-parameter value message)
  invalid-query-parameter?
  (value   invalid-query-parameter-value)
  (message invalid-query-parameter-message))

(define (parse-query-parameters request
                                accepted-query-parameters)
  (define request-query-parameters
    (let ((query (uri-query (request-uri request))))
      (if query
          (map (match-lambda
                 ((name . value)
                  (cons (string->symbol name)
                        value)))
               (parse-query-string query))
          '())))

  (filter-map
   (match-lambda
     ((name processor)
      (match (assq name request-query-parameters)
        (#f #f)
        ((_ . "") #f)
        ((_ . value) (cons name
                           (processor value)))))
     ((name processor #:default default)
      (match (assq name request-query-parameters)
        (#f (cons name default))
        ((_ . "") (cons name default))
        ((_ . value) (cons name
                           (processor value))))))
   accepted-query-parameters))

(define (parse-datetime s)
  (catch
    'misc-error
    (lambda ()
      (string->date s "~Y-~m-~d ~H:~M:~S"))
    (lambda (key . args)
      (make-invalid-query-parameter s #f))))

(define (parse-result-limit s)
  (match (string->number s)
    (#f (make-invalid-query-parameter s #f))
    ((? number? num) num)))

(define (any-invalid-query-parameters? query-parameters)
  (->bool (any invalid-query-parameter? (map cdr query-parameters))))
