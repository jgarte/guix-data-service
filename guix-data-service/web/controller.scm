;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix-data-service web controller)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (squee)
  #:use-module (guix-data-service comparison)
  #:use-module (guix-data-service model guix-revision)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service web view html)
  #:export (controller))

(define-syntax-rule (-> target functions ...)
  (fold (lambda (f val) (and=> val f))
        target
        (list functions ...)))

(define (render-with-error-handling page message)
  (apply render-html (page))
  ;; (catch #t
  ;;   (lambda ()
  ;;     (receive (sxml headers)
  ;;         (pretty-print (page))
  ;;       (render-html sxml headers)))
  ;;   (lambda (key . args)
  ;;     (format #t "ERROR: ~a ~a\n"
  ;;             key args)
  ;;     (render-html (error-page message))))
  )

(define (controller request body)
  (define conn (connect-to-postgres-paramstring "dbname=guix_data_service"))

  (match-lambda
    ((GET)
     (apply render-html (index (most-recent-n-guix-revisions conn 10))))
    ((GET "compare")
     (let ((base-commit (-> request
                            request-uri
                            uri-query
                            parse-query-string
                            (cut assoc-ref <> "base_commit")))
           (target-commit (-> request
                              request-uri
                              uri-query
                              parse-query-string
                              (cut assoc-ref <> "target_commit"))))
       (let-values
           (((base-packages-vhash target-packages-vhash)
             (package-data->package-data-vhashes
              (package-differences-data conn
                                        (commit->revision-id conn base-commit)
                                        (commit->revision-id conn target-commit)))))
         (let* ((new-packages
                 (package-data-vhashes->new-packages base-packages-vhash
                                                     target-packages-vhash))
                (removed-packages
                 (package-data-vhashes->removed-packages base-packages-vhash
                                                         target-packages-vhash))
                (version-changes
                 (package-data-version-changes base-packages-vhash
                                               target-packages-vhash))
                (other-changes
                 (package-data-other-changes base-packages-vhash
                                             target-packages-vhash)))
           (apply render-html
                  (compare base-commit
                           target-commit
                           new-packages
                           removed-packages
                           version-changes
                           other-changes))))))
    ((GET path ...)
     (render-static-asset request))))
