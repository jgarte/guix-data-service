(define-module (guix-data-service model build-status)
  #:use-module (ice-9 match)
  #:use-module (squee)
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
  (define query
    (string-append
     "
INSERT INTO build_status (build_id, timestamp, status)
VALUES ("
     (number->string build-id)
     ", "
     (string-append "to_timestamp("
                    (number->string timestamp)
                    ")")
     ", "
     (quote-string status)
     ")"))

  (exec-query conn query '()))

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
               (string-append "to_timestamp("
                              (number->string timestamp)
                              ")")
               ","
               (quote-string status)
               ")")))
           data
           build-ids)
      ", ")))

  (exec-query conn query '()))
