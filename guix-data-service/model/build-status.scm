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

(define-module (guix-data-service model build-status)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model utils)
  #:export (build-statuses
            build-status-strings
            select-build-statuses-by-build-id
            insert-build-status
            insert-build-statuses))

(define build-statuses
  '((-2 . "scheduled")
    (-1 . "started")
    (0 . "succeeded")
    (1 . "failed")
    (2 . "failed-dependency")
    (3 . "failed-other")
    (4 . "canceled")))

(define build-status-strings
  (map cdr build-statuses))

(define (select-build-statuses-by-build-id conn
                                           build-id
                                           build-server-id)
  (define query
    "
SELECT timestamp, status
FROM build_status
INNER JOIN builds ON builds.id = build_status.build_id
WHERE builds.build_server_id = $1 AND
      builds.id = $2")

  (exec-query conn query (list (number->string build-server-id)
                               (number->string build-id))))

(define (insert-build-status conn build-id timestamp status)
  (insert-build-statuses
   conn
   (list build-id)
   `((,timestamp ,status))))

(define (insert-build-statuses conn build-ids data)
  (define query
    (string-append
     "
INSERT INTO build_status (build_id, timestamp, status)
VALUES "
     (string-join
      (map (match-lambda*
             (((timestamp status) build-id)
              (unless (member status build-status-strings)
                (throw
                 'invalid-status
                 status))

              (string-append
               "("
               (number->string build-id)
               ","
               (if timestamp
                   (string-append "to_timestamp("
                                  (number->string timestamp)
                                  ")")
                   "NULL")
               ","
               (quote-string status)
               ")")))
           data
           build-ids)
      ", ")
     "
ON CONFLICT DO NOTHING"))

  (define (delete-old-latest-status-entries conn)
    (define query
      (string-append
       "
DELETE FROM latest_build_status
WHERE build_id IN ("
       (string-join
        (map number->string build-ids)
        ",")
       ")"))

    (exec-query conn query))

  (define (insert-new-latest-status-entries conn)
    (define query
      (string-append
       "
INSERT INTO latest_build_status
SELECT DISTINCT build_id,
                first_value(timestamp) OVER rows_for_build AS timestamp,
                first_value(status) OVER rows_for_build AS status
FROM build_status
WHERE build_id IN ("
       (string-join
        (map number->string build-ids)
        ",")
       ")
WINDOW rows_for_build AS (
  PARTITION BY build_id
  ORDER BY
    CASE WHEN status = 'scheduled' THEN -2
         WHEN status = 'started' THEN -1
         ELSE 0
    END DESC,
    timestamp DESC
    RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
)"))

    (exec-query conn query))

  (with-postgresql-transaction
   conn
   (lambda (conn)
     (exec-query conn query '())

     (delete-old-latest-status-entries conn)
     (insert-new-latest-status-entries conn))))
