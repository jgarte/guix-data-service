(define-module (guix-data-service jobs)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service jobs load-new-guix-revision)
  #:export (process-jobs))

(define (process-jobs conn)
  (while #t
    (match (process-next-load-new-guix-revision-job conn)
      (#f (begin
            (simple-format #t "Waiting for new jobs...")
            (unless (eq? 0 (sleep 60))
              (exit 0))))
      (_ #f))))
