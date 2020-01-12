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

(define-module (guix-data-service web jobs html)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service web html-utils)
  #:use-module (guix-data-service web view html)
  #:export (view-jobs
            view-job-queue
            view-job))

(define (view-jobs query-parameters
                   jobs-and-events
                   recent-events
                   show-next-page?)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 (@ (style "display: inline-block;"))
            "Jobs")
        (div
         (@ (class "btn-group pull-right")
            (style "margin-top: 1.3rem;")
            (role "group"))
         (a (@ (class "btn btn-lg btn-default")
               (href "/jobs/queue")
               (role "button"))
            "Queue"))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-1")))
       (div
        (@ (class "col-sm-10"))
        (table
         (@ (class "table"))
         (thead
          (tr
           (th "Job")
           (th "Event")
           (th "Occurred at")))
         (tbody
          ,@(map
             (match-lambda
               ((id commit source git-repository-id event occurred-at)
                `(tr
                  (td (a (@ (href
                             ,(string-append
                               "/revision/" commit)))
                         (samp ,commit)))
                  (td ,@(let ((classes '(("start" . "info")
                                         ("success" . "success")
                                         ("failure" . "danger"))))
                          (or (and=> (assoc-ref classes event)
                                     (lambda (class)
                                       `((@ (class ,class)))))
                              '()))
                      ,event)
                  (td ,occurred-at))))
             recent-events)))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (div
         (@ (class "well"))
         (form
          (@ (method "get")
             (action "")
             (style "padding-bottom: 0")
             (class "form-horizontal"))
          ,(form-horizontal-control
            "Before ID" query-parameters
            #:help-text
            "List jobs with identifiers before this value.")
          ,(form-horizontal-control
            "Limit results" query-parameters
            #:help-text "The maximum number of jobs to return.")
          ,(form-horizontal-control
            "All results" query-parameters
            #:type "checkbox"
            #:help-text "Return all results.")
          (div (@ (class "form-group form-group-lg"))
               (div (@ (class "col-sm-offset-2 col-sm-10"))
                    (button (@ (type "submit")
                               (class "btn btn-lg btn-primary"))
                            "Update results")))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (table
         (@ (class "table"))
         (thead
          (tr
           (th "Commit")
           (th "Source")
           (th "Events")
           (th "")))
         (tdata
          ,@(map (match-lambda
                   ((id commit source git-repository-id created-at succeeded-at
                        events log-exists?)
                    `(tr
                      (@ (class
                           ,(let ((event-names
                                   (map (lambda (event)
                                          (assoc-ref event "event"))
                                        (vector->list events))))
                              (cond
                               ((member "success" event-names)
                                "success")
                               ((member "failure" event-names)
                                "danger")
                               ((member "start" event-names)
                                "info")
                               (else
                                "")))))
                      (td (a (@ (href
                                 ,(string-append
                                   "/revision/" commit)))
                             (samp ,commit)))
                      (td ,source)
                      (td
                       (dl
                        (@ (class "dl-horizontal"))
                        ,@(map
                           (lambda (event)
                             `((dt ,(assoc-ref event "event"))
                               (dd ,(assoc-ref event "occurred_at"))))
                           (cons
                            `(("event" . "created")
                              ("occurred_at" . ,created-at))
                            (vector->list events)))))
                      (td
                       ,@(if log-exists?
                             `((a (@ (href ,(string-append "/job/" id)))
                                  "View log"))
                             '())))))
                 jobs-and-events)))
        ,@(if show-next-page?
              `((div
                 (@ (class "row"))
                 (a (@ (href
                        ,(next-page-link "/jobs"
                                         query-parameters
                                         'before_id
                                         (car (last jobs-and-events)))))
                    "Next page")))
              '())))))))

(define (view-job-queue jobs-and-events)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (a (@ (href "/jobs"))
           (h3 "Jobs"))
        (h1 "Queued jobs ("
            ,(length jobs-and-events)
            ")")))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (table
         (@ (class "table"))
         (thead
          (tr
           (th "Commit")
           (th "Source")
           (th "Events")
           (th "")))
         (tdata
          ,@(map (match-lambda
                   ((id commit source git-repository-id created-at
                        events log-exists? latest-branch-commit?)
                    `(tr
                      (@ (class
                           ,(let ((event-names
                                   (map (lambda (event)
                                          (assoc-ref event "event"))
                                        (vector->list events))))
                              (cond
                               ((member "success" event-names)
                                "success")
                               ((member "failure" event-names)
                                "danger")
                               ((member "start" event-names)
                                "info")
                               (else
                                "")))))
                      (td (a (@ (href
                                 ,(string-append
                                   "/revision/" commit)))
                             (samp ,commit)
                             ,@(if latest-branch-commit?
                                   '((br)
                                     (span (@ (class "text-danger"))
                                           "(latest branch commit)"))
                                   '())))
                      (td ,source)
                      (td
                       (dl
                        (@ (class "dl-horizontal"))
                        ,@(map
                           (lambda (event)
                             `((dt ,(assoc-ref event "event"))
                               (dd ,(assoc-ref event "occurred_at"))))
                           (cons
                            `(("event" . "created")
                              ("occurred_at" . ,created-at))
                            (vector->list events)))))
                      (td
                       ,@(if log-exists?
                             `((a (@ (href ,(string-append "/job/" id)))
                                  "View log"))
                             '())))))
                 jobs-and-events)))))))))

(define (view-job job-id query-parameters log)
  (layout
   #:body
   `(,(header)
     (div
      (@ (class "container"))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (h1 "Job " ,job-id)))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-md-12"))
        (div
         (@ (class "well"))
         (form
          (@ (method "get")
             (action "")
             (class "form-horizontal"))
          ,(form-horizontal-control
            "Characters" query-parameters
            #:help-text "Return at most this many characters.")
          ,(form-horizontal-control
            "Start character" query-parameters
            #:help-text "Start reading the log from this character.")
          (div (@ (class "form-group form-group-lg"))
               (div (@ (class "col-sm-offset-2 col-sm-10"))
                    (button (@ (type "submit")
                               (class "btn btn-lg btn-primary"))
                            "Update log")))))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (a (@ (class "btn btn-default btn-lg pull-right")
              (style "margin-bottom: 20px;")
              (href "#bottom"))
           "Scroll to the bottom of the page")))
      (div
       (@ (class "row"))
       (div
        (pre (raw ,log))
        (a (@ (id "bottom")))))
      (div
       (@ (class "row"))
       (div
        (@ (class "col-sm-12"))
        (a (@ (class "btn btn-default btn-lg pull-right")
              (href "#top"))
           "Scroll to the top of the page")))))))
