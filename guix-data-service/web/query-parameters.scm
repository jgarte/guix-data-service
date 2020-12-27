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
  #:use-module (guix-data-service model build-status)
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
            guard-against-mutually-exclusive-query-parameters
            query-parameters->string

            parse-datetime
            parse-checkbox-value
            parse-number
            parse-result-limit
            parse-system
            parse-target
            parse-build-status
            parse-derivation-build-status

            valid-targets->options))

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

(define (guard-against-mutually-exclusive-query-parameters
         parsed-query-parameters
         mutually-exclusive-groups)
  (map (match-lambda
         ((name . value)
          (if (invalid-query-parameter? value)
              (cons name value)
              (or
               (any (lambda (group)
                       (if (memq name group)
                           (let ((other-names
                                  (filter (lambda (other-name)
                                            (and (not (eq? name other-name))
                                                 (memq other-name group)))
                                          (map car parsed-query-parameters))))
                             (if (not (null? other-names))
                                 (cons
                                  name
                                  (make-invalid-query-parameter
                                   value
                                   (string-append
                                    "cannot be specified along with "
                                    (string-join (map symbol->string
                                                      other-names)
                                                 ", "))))
                                 #f))
                           #f))
                     mutually-exclusive-groups)
               (cons name value)))))
       parsed-query-parameters))

(define (parse-query-parameters request
                                accepted-query-parameters)
  (define request-query-parameters
    (let ((query (uri-query (request-uri request))))
      (if (and query
               (not (string-null? query)))
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

     ((name processor #:required)
      (match (assq name request-query-parameters)
        (#f (cons name
                  (make-invalid-query-parameter
                   #f "this value is required.")))
        ((_ . "") (cons name
                        (make-invalid-query-parameter
                         #f "this value is required.")))
        ((_ . value) (cons name
                           (processor value)))))

     ((name processor #:multi-value)
      (match (filter-map
              (match-lambda
                ((k . value)
                 (and
                  (eq? k name)
                  (match value
                    (#f #f)
                    ("" #f)
                    (value (processor value))))))
              request-query-parameters)
        (() #f)
        (x (cons name x))))

     ((name processor #:multi-value #:default default)
      (match (filter-map
              (match-lambda
                ((k . value)
                 (and
                  (eq? k name)
                  (match value
                    (#f #f)
                    ("" #f)
                    (value (processor value))))))
              request-query-parameters)
        (() (cons name default))
        (x (cons name x))))

     ((name processor #:no-default-when fields #:default default)
      (let ((use-default?
             (every (lambda (field)
                      (not (memq field fields)))
                    (map car request-query-parameters))))
        (match (assq name request-query-parameters)
          (#f (if use-default?
                  (cons name default)
                  #f))
          ((_ . "") (if use-default?
                        (cons name default)
                        #f))
          ((_ . value) (cons name
                             (processor value))))))

     ((name processor #:default default)
      (match (assq name request-query-parameters)
        (#f (cons name default))
        ((_ . "") (cons name default))
        ((_ . value) (cons name
                           (processor value))))))
   accepted-query-parameters))

(define (query-parameters->string query-parameters)
  (define (value->text value)
    (match value
      (#f "")
      (#t "on")
      ((? date? date)
       (date->string date "~1 ~3"))
      ;; TODO Hardcoding none here is a bit of a hack, but it does make the
      ;; Next page link work for revision derivations.
      ("" "none")
      (other other)))

  (string-join
   (concatenate
    (map
     (match-lambda
       ((key . ($ <invalid-query-parameter>))
        '())
       ((key . value)
        (if (list? value)
            (map (lambda (value)
                   (simple-format #f "~A=~A"
                                  key (value->text value)))
                 value)
            (list (simple-format
                   #f "~A=~A"
                   key (value->text value))))))
     query-parameters))
   "&"))

(define (parse-datetime s)
  (catch
    'misc-error
    (lambda ()
      (string->date s "~Y-~m-~d ~H:~M:~S"))
    (lambda (key . args)
      (make-invalid-query-parameter
       s
       "Unable to parse date, please use YYYY-MM-DD HH:MM:SS as the format."))))

(define (parse-checkbox-value s)
  (string=? s "on"))

(define (parse-number s)
  (match (string->number s)
    (#f (make-invalid-query-parameter s #f))
    ((? number? num) num)))

(define parse-result-limit parse-number)

(define parse-system identity)

(define (parse-target target)
  (if (string=? target "none")
      ""
      target))

(define (parse-build-status status)
  (if (member status build-status-strings)
      status
      (make-invalid-query-parameter
       status
       (string-append "unknown build status: "
                      status))))

(define (parse-derivation-build-status status)
  (define options
    '("none" "working" "failing" "unknown"))

  (if (member status options)
      (if (string=? status "none")
          #f
          status)
      (make-invalid-query-parameter
       status
       (string-append "unknown derivation build status: "
                      status))))

(define (valid-targets->options targets)
  `(("(no target)" . "none")
    ,@(map (lambda (target)
             (cons target target))
           targets)))

(define (any-invalid-query-parameters? query-parameters)
  (->bool (any (lambda (val)
                 (if (list? val)
                     (any invalid-query-parameter? val)
                     (invalid-query-parameter? val)))
               (map cdr query-parameters))))
