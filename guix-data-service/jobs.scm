(define-module (guix-data-service jobs)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (guix-data-service jobs load-new-guix-revision)
  #:export (process-jobs))

(define (process-jobs conn)
  (define (fetch-new-jobs)
    (fetch-unlocked-jobs conn))

  (define (process-job job-id)
    (execlp "guix-data-service-process-job"
            "guix-data-service-process-job"
            job-id))

  (process-jobs-concurrently fetch-new-jobs
                             process-job))

(define default-max-processes
  (max (round (/ (current-processor-count)
                 4))
       1))

(define default-timeout
  (* (* 60 60) ;; 1 hour in seconds
     24))

(define* (process-jobs-concurrently fetch-new-jobs
                                    process-job
                                    #:key (max-processes
                                           default-max-processes)
                                    (timeout default-timeout))
  (define processes
    (make-hash-table))

  (define (display-status)
    (display
     (string-append
      "\n\n"
      (let ((running-jobs (hash-count (const #t) processes)))
        (cond
         ((eq? running-jobs 0)
          "status: 0 running jobs")
         ((eq? running-jobs 1)
          "status: 1 running job")
         (else
          (simple-format #f "status: ~A running jobs"
                         running-jobs))))
      "\n"
      (string-concatenate
       (hash-map->list
        (match-lambda*
          ((pid (start-time job-args))
           (format #f "  pid: ~5d  job args: ~a\n"
                   pid job-args)))
        processes))
      "\n")))

  (define (wait-on-processes)
    (catch
      #t
      (lambda ()
        (match (waitpid WAIT_ANY WNOHANG)
          ((0 . status)
           ;; No process to wait for
           #f)
          ((pid . status)
           (hashv-remove! processes pid)
           (simple-format (current-error-port)
                          "pid ~A failed with status ~A\n"
                          pid status)

           ;; Recurse, to check for other finished processes.
           (wait-on-processes))))
      (lambda (key . args)
        (simple-format #t "key ~A args ~A\n"
                       key args))))

  (define (kill-long-running-processes)
    (hash-map->list
     (match-lambda*
       ((pid (start-time job-args))
        (let ((running-for
               (- (current-time) start-time)))
          (when (> running-for timeout)
            (display
             (simple-format
              #f "sending SIGTERM to pid ~A started at ~A, now running for ~A\n"
              pid start-time running-for)
             (current-error-port))
            (kill pid SIGTERM)))))
     processes))

  (define (fork-and-process-job job-args)
    (match (primitive-fork)
      (0
       (dynamic-wind
         (const #t)
         (lambda ()
           (apply process-job job-args))
         (lambda ()
           (primitive-exit 127))))
      (pid
       (hashv-set! processes pid
                   (list (current-time) job-args))
       #t)))

  (while #t
    (kill-long-running-processes)
    (wait-on-processes)
    (display-status)
    (match (fetch-new-jobs)
      (()
       ;; Nothing to do
       #f)
      ((jobs ...)
       (for-each
        (lambda (job-args)
          (let ((current-processes
                 (hash-count (const #t) processes)))
            (when (< current-processes
                     max-processes)
              (fork-and-process-job job-args))))
        jobs)))
    (unless (eq? 0 (sleep 15))
      (exit 0))))
