(define-module (guix-data-service jobs)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service jobs load-new-guix-revision)
  #:export (process-jobs))

(define (process-jobs conn)
  (while #t
    (match (process-next-load-new-guix-revision-job conn)
      (#f (unless (eq? 0 (sleep 5))
            (exit 0)))
      (_ (simple-format #t "\nFinished processing job\n\n")))))
