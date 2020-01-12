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

(define-module (guix-data-service web jobs controller)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service jobs load-new-guix-revision)
  #:use-module (guix-data-service web jobs html)
  #:export (jobs-controller))

(define (jobs-controller request
                         method-and-path-components
                         mime-types
                         body
                         conn)
  (match method-and-path-components
    (('GET "jobs")
     (let ((parsed-query-parameters
            (guard-against-mutually-exclusive-query-parameters
             (parse-query-parameters
              request
              `((before_id  ,parse-number)
                (limit_results  ,parse-result-limit
                                #:no-default-when (all_results)
                                #:default 20)
                (all_results    ,parse-checkbox-value)))
             '((limit_results all_results)))))
       (render-jobs mime-types
                    conn
                    parsed-query-parameters)))
    (('GET "jobs" "queue")
     (render-job-queue mime-types
                       conn))
    (('GET "job" job-id)
     (let ((parsed-query-parameters
            (parse-query-parameters
             request
             `((start_character ,parse-number)
               (characters ,parse-number #:default 1000000)))))
       (render-job mime-types
                   conn
                   job-id
                   parsed-query-parameters)))
    (_ #f)))

(define (render-jobs mime-types conn query-parameters)
  (let* ((limit-results
          (assq-ref query-parameters 'limit_results))
         (jobs (select-jobs-and-events
                conn
                (assq-ref query-parameters 'before_id)
                limit-results))
         (recent-events (select-recent-job-events conn)))
    (render-html
     #:sxml (view-jobs
             query-parameters
             jobs
             recent-events
             (and limit-results
                  (>= (length jobs)
                      limit-results))))))

(define (render-job-queue mime-types conn)
  (render-html
   #:sxml (view-job-queue
           (select-unprocessed-jobs-and-events conn))))

(define (render-job mime-types conn job-id query-parameters)
  (render-html
   #:sxml (view-job
           job-id
           query-parameters
           (log-for-job conn job-id
                        #:character-limit
                        (assq-ref query-parameters 'characters)
                        #:start-character
                        (assq-ref query-parameters 'start_character)))))

