;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2019, 2020 Christopher Baines <mail@cbaines.net>
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

(define-module (guix-data-service substitutes)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix substitutes)
  #:use-module (guix narinfo)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model build-server)
  #:use-module (guix-data-service model nar)
  #:export (query-build-server-substitutes))

(define verbose-output?
  (make-parameter #f))

(define* (query-build-server-substitutes conn build-server-ids revision-commits
                                         outputs
                                         #:key verbose?)
  (parameterize
      ((verbose-output? verbose?))
    (while #t
      (let ((build-servers (select-build-servers conn)))
        (for-each
         (match-lambda
           ((id url lookup-all-derivations? lookup-builds?)
            (when (or (or (not build-servers)
                          (not build-server-ids))
                      (member id build-server-ids))
              (when lookup-all-derivations?
                (simple-format #t "\nQuerying ~A\n" url)
                (catch #t
                  (lambda ()
                    (simple-format #t "\nFetching narinfo files\n")
                    (fetch-narinfo-files conn id url revision-commits
                                         #:specific-outputs
                                         outputs))
                  (lambda (key . args)
                    (simple-format
                     (current-error-port)
                     "exception in query-build-server: ~A ~A\n"
                     key args)))))))
         build-servers)))))

(define %narinfo-max-size
  (- (expt 2 (- (* 8 8) ;; 8 bytes
                1))
     1))

(define* (fetch-narinfo-files conn build-server-id build-server-url
                              revision-commits
                              #:key specific-outputs)
  (define outputs
    (or specific-outputs
        (select-outputs-without-known-nar-entries
         conn
         build-server-id
         revision-commits)))

  (simple-format #t "Querying ~A outputs\n"
                 (length outputs))

  (let ((narinfos
         (lookup-narinfos (string-trim-right build-server-url #\/) outputs)))

    (simple-format #t "Got ~A narinfo files\n"
                   (length narinfos))

    (unless (eq? (length narinfos) 0)
      (with-postgresql-transaction
       conn
       (lambda (conn)
         (record-narinfo-details-and-return-ids
          conn
          build-server-id
          (filter-map
           (lambda (narinfo)
             (if (> (narinfo-size narinfo)
                    %narinfo-max-size)
                 (begin
                   (simple-format (current-error-port)
                                  "narinfo ~A has excessive size ~A\n"
                                  (narinfo-path narinfo)
                                  (narinfo-size narinfo))
                   #f)
                 narinfo))
           narinfos)))))))
