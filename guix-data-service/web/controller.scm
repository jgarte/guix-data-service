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
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (squee)
  #:use-module (guix-data-service comparison)
  #:use-module (guix-data-service model guix-revision)
  #:use-module (guix-data-service model package)
  #:use-module (guix-data-service model derivation)
  #:use-module (guix-data-service model build)
  #:use-module (guix-data-service jobs load-new-guix-revision)
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

(define (with-base-and-target-commits request conn f)
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
    (f base-commit
       (commit->revision-id conn base-commit)
       target-commit
       (commit->revision-id conn target-commit))))

(define (render-compare-unknown-commit content-type
                                       conn
                                       base-commit
                                       base-revision-id
                                       target-commit
                                       target-revision-id)
  (cond
   ((eq? content-type 'json)
    (render-json
     '((unknown_commit . #t))))
   (else
    (apply render-html
           (compare-unknown-commit base-commit
                                   target-commit
                                   (if base-revision-id #t #f)
                                   (if target-revision-id #t #f)
                                   (select-job-for-commit conn
                                                          base-commit)
                                   (select-job-for-commit conn
                                                          target-commit))))))

(define (render-compare content-type
                        conn
                        base-commit
                        base-revision-id
                        target-commit
                        target-revision-id)
  (let-values
      (((base-packages-vhash target-packages-vhash)
        (package-data->package-data-vhashes
         (package-differences-data conn
                                   base-revision-id
                                   target-revision-id))))
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
      (cond
       ((eq? content-type 'json)
        (render-json
         `((new-packages . ,new-packages)
           (removed-packages . ,removed-packages)
           (version-changes . ,version-changes)
           (other-changes . ,other-changes))))
       (else
        (apply render-html
               (compare base-commit
                        target-commit
                        new-packages
                        removed-packages
                        version-changes
                        other-changes)))))))

(define (render-compare/derivations content-type
                                    conn
                                    base-commit
                                    base-revision-id
                                    target-commit
                                    target-revision-id)
  (let-values
      (((base-packages-vhash target-packages-vhash)
        (package-data->package-data-vhashes
         (package-differences-data conn
                                   base-revision-id
                                   target-revision-id))))
    (let ((base-derivations
           (package-data-vhash->derivations-and-build-status
            conn
            base-packages-vhash))
          (target-derivations
           (package-data-vhash->derivations-and-build-status
            conn
            target-packages-vhash)))
      (cond
       ((eq? content-type 'json)
        (render-json
         `((base . ((commit . ,base-commit)
                    (derivations . ,base-derivations)))
           (target . ((commit . ,target-commit)
                      (derivations . ,target-derivations))))))
       (else
        (apply render-html
               (compare/derivations
                base-commit
                target-commit
                base-derivations
                target-derivations)))))))

(define (render-compare/packages content-type
                                 conn
                                 base-commit
                                 base-revision-id
                                 target-commit
                                 target-revision-id)
  (define (package-data-vhash->json vh)
    (vhash-fold (lambda (name data result)
                  (cons (string-append name "@" (car data))
                        result))
                '()
                vh))

  (let-values
      (((base-packages-vhash target-packages-vhash)
        (package-data->package-data-vhashes
         (package-differences-data conn
                                   base-revision-id
                                   target-revision-id))))
    (cond
     ((eq? content-type 'json)
      (render-json
       `((base . ((commit . ,base-commit)
                  (packages . ,(package-data-vhash->json base-packages-vhash))))
         (target . ((commit . ,target-commit)
                    (packages . ,(package-data-vhash->json target-packages-vhash)))))))
     (else
      (apply render-html
             (compare/packages
              base-commit
              target-commit
              base-packages-vhash
              target-packages-vhash))))))

(define (render-derivation conn derivation-file-name)
  (let ((derivation (select-derivation-by-file-name conn
                                                    derivation-file-name)))
    (if derivation
        (let ((derivation-inputs (select-derivation-inputs-by-derivation-id
                                  conn
                                  (first derivation)))
              (derivation-outputs (select-derivation-outputs-by-derivation-id
                                   conn
                                   (first derivation))))
          (apply render-html
                 (view-derivation derivation
                                  derivation-inputs
                                  derivation-outputs)))
        #f ;; TODO
        )))

(define (render-store-item conn filename)
  (apply render-html
         (view-store-item filename)))

(define (controller request body conn)
  (match-lambda
    ((GET)
     (apply render-html (index
                         (most-recent-n-guix-revisions conn 10)
                         (most-recent-n-load-new-guix-revision-jobs conn 1000))))
    ((GET "builds")
     (apply render-html
            (view-builds (select-build-stats conn)
                         (select-builds-with-context conn))))
    ((GET "revision" commit-hash)
     (apply render-html
            (view-revision commit-hash
                           (select-packages-in-revision conn
                                                        commit-hash))))
    ((GET "gnu" "store" filename)
     (if (string-suffix? ".drv" filename)
         (render-derivation conn (string-append "/gnu/store/" filename))
         (render-store-item conn (string-append "/gnu/store/" filename))))
    ((GET "compare")
     (with-base-and-target-commits
      request conn
      (lambda (base-commit base-revision-id target-commit target-revision-id)
        (if (not (and base-revision-id target-revision-id))
            (render-compare-unknown-commit 'html
                                           conn
                                           base-commit
                                           base-revision-id
                                           target-commit
                                           target-revision-id)
            (render-compare 'html
                            conn
                            base-commit
                            base-revision-id
                            target-commit
                            target-revision-id)))))
    ((GET "compare.json")
     (with-base-and-target-commits
      request conn
      (lambda (base-commit base-revision-id target-commit target-revision-id)
        (if (not (and base-revision-id target-revision-id))
            (render-compare-unknown-commit 'json
                                           conn
                                           base-commit
                                           base-revision-id
                                           target-commit
                                           target-revision-id)
            (render-compare 'json
                            conn
                            base-commit
                            base-revision-id
                            target-commit
                            target-revision-id)))))
    ((GET "compare" "derivations")
     (with-base-and-target-commits
      request conn
      (lambda (base-commit base-revision-id target-commit target-revision-id)
        (if (not (and base-revision-id target-revision-id))
            (render-compare-unknown-commit 'html
                                           conn
                                           base-commit
                                           base-revision-id
                                           target-commit
                                           target-revision-id)
            (render-compare/derivations 'html
                                        conn
                                        base-commit
                                        base-revision-id
                                        target-commit
                                        target-revision-id)))))
    ((GET "compare" "derivations.json")
     (with-base-and-target-commits
      request conn
      (lambda (base-commit base-revision-id target-commit target-revision-id)
        (if (not (and base-revision-id target-revision-id))
            (render-compare-unknown-commit 'json
                                           conn
                                           base-commit
                                           base-revision-id
                                           target-commit
                                           target-revision-id)
            (render-compare/derivations 'json
                                        conn
                                        base-commit
                                        base-revision-id
                                        target-commit
                                        target-revision-id)))))
    ((GET "compare" "packages")
     (with-base-and-target-commits
      request conn
      (lambda (base-commit base-revision-id target-commit target-revision-id)
        (if (not (and base-revision-id target-revision-id))
            (render-compare-unknown-commit 'html
                                           conn
                                           base-commit
                                           base-revision-id
                                           target-commit
                                           target-revision-id)
            (render-compare/packages 'html
                                     conn
                                     base-commit
                                     base-revision-id
                                     target-commit
                                     target-revision-id)))))
    ((GET "compare" "packages.json")
     (with-base-and-target-commits
      request conn
      (lambda (base-commit base-revision-id target-commit target-revision-id)
        (if (not (and base-revision-id target-revision-id))
            (render-compare-unknown-commit 'json
                                           conn
                                           base-commit
                                           base-revision-id
                                           target-commit
                                           target-revision-id)
            (render-compare/packages 'json
                                     conn
                                     base-commit
                                     base-revision-id
                                     target-commit
                                     target-revision-id)))))
    ((GET path ...)
     (render-static-asset request))))
