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

(define-module (guix-data-service jobs)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 atomic)
  #:use-module (guix-data-service jobs load-new-guix-revision)
  #:export (process-jobs

            default-max-processes))

(define* (process-jobs conn #:key max-processes
                       latest-branch-revision-max-processes)
  (define (fetch-new-jobs)
    (fetch-unlocked-jobs conn))

  (define (process-job job-id)
    (execlp "guix-data-service-process-job"
            "guix-data-service-process-job"
            job-id))

  (define (handle-job-failure job-id)
    (record-job-event conn job-id "failure")
    (display (simple-format #f "recording failure for job ~A\n" job-id)
             (current-error-port))
    (when (> (count-log-parts conn job-id)
             0)
      (combine-log-parts! conn job-id)))

  (process-jobs-concurrently fetch-new-jobs
                             process-job
                             handle-job-failure
                             #:max-processes max-processes
                             #:priority-max-processes
                             latest-branch-revision-max-processes))

(define default-max-processes
  (max (round (/ (current-processor-count)
                 4))
       1))

(define default-timeout
  (* (* 60 60) ;; 1 hour in seconds
     72))

(define* (process-jobs-concurrently
          fetch-new-jobs
          process-job
          handle-job-failure
          #:key
          (max-processes default-max-processes)
          (priority-max-processes (* 2 max-processes))
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
            (kill pid SIGTERM)

            (match job-args
              ((id)
               (handle-job-failure id)))))))
     processes))

  (define (stop-running-processes)
    (hash-map->list
     (match-lambda*
       ((pid (start-time job-args))
        (display
         (simple-format
          #f "sending SIGTERM to pid ~A\n"
          pid)
         (current-error-port))
        (kill pid SIGTERM)))
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

  (define exit?
    (make-atomic-box #f))

  (sigaction SIGTERM
    (lambda args
      (simple-format (current-error-port) "exiting due to SIGTERM\n")
      (atomic-box-set! exit? #t)))

  (while #t
    (kill-long-running-processes)
    (wait-on-processes)
    (display-status)

    (when (atomic-box-ref exit?)
      (stop-running-processes)
      (exit 0))

    (match (fetch-new-jobs)
      (()
       ;; Nothing to do
       #f)
      ((jobs ...)
       (for-each
        (match-lambda
          ((job-id priority?)
           (let ((current-processes
                  (hash-count (const #t) processes)))
             (when (< current-processes
                      (if priority?
                          priority-max-processes
                          max-processes))
               (fork-and-process-job (list job-id))))))
        jobs)))
    (sleep 15)))
