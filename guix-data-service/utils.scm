;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2020 Christopher Baines <mail@cbaines.net>
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

(define-module (guix-data-service utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:export (call-with-time-logging
            with-time-logging
            prevent-inlining-for-tests

            parallel-via-thread-pool-channel
            par-map&
            letpar&

            chunk!))

(define (call-with-time-logging action thunk)
  (simple-format #t "debug: Starting ~A\n" action)
  (let ((start-time (current-time)))
    (let-values
        ((result (thunk)))
      (let ((time-taken (- (current-time) start-time)))
        (simple-format #t "debug: Finished ~A, took ~A seconds\n"
                       action time-taken))
      (apply values result))))

(define-syntax-rule (with-time-logging action exp ...)
  "Log under NAME the time taken to evaluate EXP."
  (call-with-time-logging action (lambda () exp ...)))

(define-syntax-rule (prevent-inlining-for-tests var)
  (set! var var))


(define* (make-thread-pool-channel #:key (threads 8))
  (parameterize (((@@ (fibers internal) current-fiber) #f))
    (let ((channel (make-channel)))
      (for-each
       (lambda _
         (call-with-new-thread
          (lambda ()
            (let loop ()
              (match (get-message channel)
                (((? channel? reply) . (? procedure? proc))
                 (put-message
                  reply
                  (with-exception-handler
                      (lambda (exn)
                        (cons 'worker-thread-error exn))
                    (lambda ()
                      (with-exception-handler
                          (lambda (exn)
                            (simple-format
                             (current-error-port)
                             "worker thread: exception: ~A\n"
                             exn)
                            (backtrace)
                            (raise-exception exn))
                        (lambda ()
                          (call-with-values
                              proc
                            (lambda vals
                              vals)))))
                    #:unwind? #t))
                 (loop))
                (_ #f))))))
       (iota threads))
      channel)))

(define %thread-pool-mutex (make-mutex))
(define %thread-pool-channel #f)

(define (make-thread-pool-channel!')
  (with-mutex %thread-pool-mutex
    (unless %thread-pool-channel
      (set! %thread-pool-channel (make-thread-pool-channel))
      (set! make-thread-pool-channel! (lambda () #t)))))

(define make-thread-pool-channel!
  (lambda () (make-thread-pool-channel!')))

(define (defer-to-thread-pool-channel thunk)
  (make-thread-pool-channel!)
  (let ((reply (make-channel)))
    (spawn-fiber
     (lambda ()
       (put-message %thread-pool-channel (cons reply thunk))))
    reply))

(define (fetch-result-of-defered-thunk reply-channel)
  (match (get-message reply-channel)
    (('worker-thread-error . exn)
     (raise-exception exn))
    (result
     (apply values result))))

(define (fetch-result-of-defered-thunks . reply-channels)
  (let ((responses (map get-message reply-channels)))
    (map
     (match-lambda
       (('worker-thread-error . exn)
        (raise-exception exn))
       (result
        (apply values result)))
     responses)))

(define-syntax parallel-via-thread-pool-channel
  (lambda (x)
    (syntax-case x ()
      ((_ e0 ...)
       (with-syntax (((tmp0 ...) (generate-temporaries (syntax (e0 ...)))))
         #'(let ((tmp0 (defer-to-thread-pool-channel
                         (lambda ()
                           e0)))
                 ...)
             (apply values (fetch-result-of-defered-thunks tmp0 ...))))))))

(define-syntax-rule (letpar& ((v e) ...) b0 b1 ...)
  (call-with-values
      (lambda () (parallel-via-thread-pool-channel e ...))
    (lambda (v ...)
      b0 b1 ...)))

(define (par-mapper' mapper cons)
  (lambda (proc . lists)
    (let loop ((lists lists))
      (match lists
        (((heads tails ...) ...)
         (let ((tail (loop tails))
               (head (defer-to-thread-pool-channel
                       (lambda ()
                         (apply proc heads)))))
           (cons (fetch-result-of-defered-thunk head) tail)))
        (_
         '())))))

(define par-map& (par-mapper' map cons))

(define (chunk! lst max-length)
  (if (> (length lst)
         max-length)
      (call-with-values (lambda ()
                          (split-at! lst max-length))
        (lambda (first-lst rest)
          (cons first-lst
                (chunk! rest max-length))))
      (list lst)))
