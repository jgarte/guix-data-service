(define-module (guix-data-service model build-status)
  #:use-module (squee)
  #:export (build-statuses
            insert-build-status))

(define build-statuses
  '((-2 . "scheduled")
    (-1 . "started")
    (0 . "succeeded")
    (1 . "failed")
    (2 . "failed-dependency")
    (3 . "failed-other")
    (4 . "canceled")))

(define (insert-build-status conn internal-build-id
                             starttime stoptime status)
  (exec-query conn
              (string-append
               "INSERT INTO build_status "
               "(internal_build_id, starttime, stoptime, status) "
               "VALUES "
               "(" internal-build-id ", "
               (if (eq? starttime 0)
                   "NULL"
                   (string-append "to_timestamp("
                                  (number->string starttime)
                                  ")"))
               ", "
               (if (eq? stoptime 0)
                   "NULL"
                   (string-append "to_timestamp("
                                  (number->string stoptime)
                                  ")"))
               ", "
               "'" status "'"
               ")")))
