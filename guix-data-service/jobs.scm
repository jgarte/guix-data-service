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

(define* (process-jobs-concurrently fetch-new-jobs
                                    process-job
                                    #:key (max-processes
                                           default-max-processes))
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
        (lambda (pid job-args)
          (format #f "  pid: ~5d  job args: ~a\n"
                  pid job-args))
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
           (let ((job-args (hashv-ref processes pid)))
             (hashv-remove! processes pid)
             (simple-format
              (current-error-port)
              "pid ~A failed with status ~A\n"
              pid status))
           (wait-on-processes))))
      (lambda (key . args)
        (simple-format #t "key ~A args ~A\n"
                       key args))))

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
       (hashv-set! processes pid job-args)
       #t)))

  (while #t
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
