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
  #:export (call-with-time-logging
            with-time-logging))

(define (call-with-time-logging action thunk)
  (simple-format #t "debug: Starting ~A\n" action)
  (let* ((start-time (current-time))
         (result (thunk))
         (time-taken (- (current-time) start-time)))
    (simple-format #t "debug: Finished ~A, took ~A seconds\n"
                   action time-taken)
    result))

(define-syntax-rule (with-time-logging action exp ...)
  "Log under NAME the time taken to evaluate EXP."
  (call-with-time-logging action (lambda () exp ...)))
