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

(define-module (guix-data-service jobs load-new-guix-revision)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 hash-table)
  #:use-module (rnrs exceptions)
  #:use-module (json)
  #:use-module (squee)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix utils)
  #:use-module (guix progress)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix build utils)
  #:use-module (guix-data-service config)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service utils)
  #:use-module (guix-data-service model build)
  #:use-module (guix-data-service model channel-instance)
  #:use-module (guix-data-service model channel-news)
  #:use-module (guix-data-service model package)
  #:use-module (guix-data-service model package-derivation-by-guix-revision-range)
  #:use-module (guix-data-service model git-repository)
  #:use-module (guix-data-service model guix-revision)
  #:use-module (guix-data-service model package-derivation)
  #:use-module (guix-data-service model guix-revision-package-derivation)
  #:use-module (guix-data-service model license-set)
  #:use-module (guix-data-service model lint-checker)
  #:use-module (guix-data-service model lint-warning)
  #:use-module (guix-data-service model lint-warning-message)
  #:use-module (guix-data-service model location)
  #:use-module (guix-data-service model package-metadata)
  #:use-module (guix-data-service model derivation)
  #:use-module (guix-data-service model system-test)
  #:export (log-for-job
            count-log-parts
            combine-log-parts!
            fetch-unlocked-jobs
            process-load-new-guix-revision-job
            select-job-for-commit
            select-jobs-and-events
            select-recent-job-events
            select-unprocessed-jobs-and-events
            select-jobs-and-events-for-commit
            record-job-event
            enqueue-load-new-guix-revision-job
            most-recent-n-load-new-guix-revision-jobs))

(define (log-part-sequence-name job-id)
  (simple-format #f "load_new_guix_revision_job_log_parts_id_seq_~A" job-id))

(define* (log-port job-id conn
                   #:key
                   delete-existing-log-parts?
                   real-output-port)
  (define output-port
    (or real-output-port
        (current-output-port)))

  (define buffer "")

  (define (insert job_id s)
    (exec-query
     conn
     (string-append
      "INSERT INTO load_new_guix_revision_job_log_parts (id, job_id, contents) "
      "VALUES (nextval('" (log-part-sequence-name job_id) "'), $1, $2)")
     (list job_id s)))

  (define (log-string s)
    (if (string-contains s "\n")
        (let ((output (string-append buffer s)))
          (set! buffer "") ; clear the buffer
          (insert job-id output)
          (display output output-port))
        (set! buffer (string-append buffer s))))

  (exec-query
   conn
   (simple-format #f "CREATE SEQUENCE IF NOT EXISTS ~A"
                  (log-part-sequence-name job-id)))
  (when delete-existing-log-parts?
    ;; TODO, this is useful when re-running jobs, but I'm not sure that should
    ;; be a thing, jobs should probably be only attempted once.
    (exec-query
     conn
     "DELETE FROM load_new_guix_revision_job_log_parts WHERE job_id = $1"
     (list job-id)))

  (let ((port
         (make-soft-port
          (vector (lambda (c)
                    (set! buffer (string-append buffer (string c))))
                  log-string
                  (lambda ()
                    (force-output output-port))
                  #f ; fetch one character
                  (lambda ()
                    ;; close port
                    #f)
                  #f) ; number of characters that can be read
          "w")))
    (setvbuf port 'line)
    port))

(define (setup-port-for-inferior-error-output job-id real-output-port)
  (define (insert conn job_id s)
    (exec-query
     conn
     (string-append "
INSERT INTO load_new_guix_revision_job_log_parts (id, job_id, contents)
VALUES (nextval('" (log-part-sequence-name job_id) "'), $1, $2)")
     (list job_id s)))

  (match (pipe)
    ((port-to-read-from . port-to-write-to)

     (setvbuf port-to-read-from 'line)
     (setvbuf port-to-write-to 'line)
     (call-with-new-thread
      (lambda ()
        (with-postgresql-connection
         (simple-format #f "~A inferior error logging" job-id)
         (lambda (logging-conn)
           (let loop ((line (get-line port-to-read-from)))
             (let ((line-with-newline
                    (string-append line "\n")))
               (insert logging-conn job-id line-with-newline)
               (display line-with-newline real-output-port))
             (loop (get-line port-to-read-from)))))))3

     port-to-write-to)))

(define real-error-port
  (make-parameter (current-error-port)))

(define inferior-error-port
  (make-parameter (current-error-port)))

(define* (log-for-job conn job-id
                      #:key
                      character-limit
                      start-character)
  (define (sql-html-escape s)
    (string-append
     "replace("
     (string-append
      "replace("
      (string-append
       "replace("
       s
       ",'&','&amp;')")
      ",'<','&lt;')")
     ",'>','&gt;')"))

  (define (get-characters s)
    (if start-character
        (simple-format #f "substr(~A, ~A, ~A)"
                       s start-character
                       character-limit)
        (simple-format #f "right(~A, ~A)" s character-limit)))

  (define log-query
    (string-append
     "SELECT "
     (sql-html-escape (get-characters "contents"))
     " FROM load_new_guix_revision_job_logs"
     " WHERE job_id = $1 AND contents IS NOT NULL"))

  (define parts-query
    (string-append
     "SELECT "
     (sql-html-escape
      (get-characters "STRING_AGG(contents, '' ORDER BY id ASC)"))
     " FROM load_new_guix_revision_job_log_parts WHERE job_id = $1"))

  (match (exec-query conn log-query (list job-id))
    (((contents))
     contents)
    (()
     (match (exec-query conn parts-query (list job-id))
       (((contents))
        contents)))))

(define (insert-empty-log-entry conn job-id)
  (exec-query
   conn
   "DELETE FROM load_new_guix_revision_job_logs WHERE job_id = $1"
   (list job-id))
  (exec-query
   conn
   "INSERT INTO load_new_guix_revision_job_logs (job_id, contents) VALUES
($1, NULL)"
   (list job-id)))

(define (count-log-parts conn job-id)
  (match (exec-query
          conn
          "
SELECT COUNT(*)
FROM load_new_guix_revision_job_log_parts
WHERE job_id = $1"
          (list job-id))
    (((id))
     (string->number id))))

(define (combine-log-parts! conn job-id)
  (with-postgresql-transaction
   conn
   (lambda (conn)
     (exec-query
      conn
      (string-append
       "UPDATE load_new_guix_revision_job_logs SET contents = "
       "("
       "SELECT STRING_AGG(contents, '' ORDER BY id ASC) FROM "
       "load_new_guix_revision_job_log_parts WHERE job_id = $1 "
       "GROUP BY job_id"
       ")"
       "WHERE job_id = $1")
      (list job-id))
     (exec-query
      conn
      "DELETE FROM load_new_guix_revision_job_log_parts WHERE job_id = $1"
      (list job-id)))))

(define (drop-log-parts-sequence conn job-id)
  (exec-query
   conn
   (string-append
    "DROP SEQUENCE "
    (log-part-sequence-name job-id))))

(define inferior-package-id
  (@@ (guix inferior) inferior-package-id))

(define (record-start-time action)
  (simple-format #t "debug: Starting ~A\n" action)
  (cons action
        (current-time)))

(define record-end-time
  (match-lambda
    ((action . start-time)
     (let ((time-taken (- (current-time) start-time)))
       (simple-format #t "debug: Finished ~A, took ~A seconds\n"
                      action time-taken)))))

(define (with-advisory-session-lock/log-time conn lock f)
  (simple-format #t "debug: Acquiring advisory session lock: ~A\n" lock)
  (let ((start-time (current-time)))
    (with-advisory-session-lock
     conn
     lock
     (lambda ()
       (let ((time-taken (- (current-time) start-time)))
         (simple-format #t "debug: Finished aquiring lock ~A, took ~A seconds\n"
                        lock time-taken))
       (f)))))

(define (all-inferior-system-tests inf store)
  (define extract
    '(lambda (store)
       (map
        (lambda (system-test)
          (list (system-test-name system-test)
                (system-test-description system-test)
                (derivation-file-name
                 (run-with-store store
                   (mbegin %store-monad
                     (system-test-value system-test))))
                (match (system-test-location system-test)
                  (($ <location> file line column)
                   (list file
                         line
                         column)))))
        (all-system-tests))))

  (let ((system-test-data
         (with-time-logging "getting system tests"
           (inferior-eval-with-store inf store extract))))

    (for-each (lambda (derivation-file-name)
                (add-temp-root store derivation-file-name))
              (map third system-test-data))

    system-test-data))

(define (all-inferior-lint-warnings inf store)
  (define locales
    '("cs_CZ.utf8"
      "da_DK.utf8"
      "de_DE.utf8"
      "eo_EO.utf8"
      "es_ES.utf8"
      "fr_FR.utf8"
      "hu_HU.utf8"
      "pl_PL.utf8"
      "pt_BR.utf8"
      ;;"sr_SR.utf8"
      "sv_SE.utf8"
      "vi_VN.utf8"
      "zh_CN.utf8"))

  (define (lint-warnings-for-checker checker-name)
    `(lambda (store)
       (let* ((checker (find (lambda (checker)
                               (eq? (lint-checker-name checker)
                                    ',checker-name))
                             %local-checkers))
              (check (lint-checker-check checker)))

         (define (process-lint-warning lint-warning)
           (list
            (match (lint-warning-location lint-warning)
              (($ <location> file line column)
               (list (if (string-prefix? "/gnu/store/" file)
                         ;; Convert a string like
                         ;; /gnu/store/53xh0mpigin2rffg31s52x5dc08y0qmr-guix-module-union/share/guile/site/2.2/gnu/packages/xdisorg.scm
                         ;;
                         ;; This happens when the checker uses
                         ;; package-field-location.
                         (string-join (drop (string-split file #\/) 8) "/")
                         file)
                     line
                     column)))
            (let* ((source-locale "en_US.utf8")
                   (source-message
                    (begin
                      (setlocale LC_MESSAGES source-locale)
                      (lint-warning-message lint-warning)))
                   (messages-by-locale
                    (filter-map
                     (lambda (locale)
                       (catch 'system-error
                         (lambda ()
                           (setlocale LC_MESSAGES locale))
                         (lambda (key . args)
                           (error
                            (simple-format
                             #f
                             "error changing locale to ~A: ~A ~A"
                             locale key args))))
                       (let ((message
                              (lint-warning-message lint-warning)))
                         (setlocale LC_MESSAGES source-locale)
                         (if (string=? message source-message)
                             #f
                             (cons locale message))))
                     (list ,@locales))))
              (cons (cons source-locale source-message)
                    messages-by-locale))))

         (filter
          (match-lambda
            ((package-id . warnings)
             (not (null? warnings))))
          (hash-map->list
           (lambda (package-id package)
             (cons
              package-id
              (map process-lint-warning
                   (check package))))
           %package-table)))))

  (and
   (catch
     'misc-error
     (lambda ()
       (inferior-eval '(use-modules (guix lint)) inf)
       #t)
     (lambda (key . args)
       (simple-format (current-error-port)
                      "warning: failed to load the (guix lint) module: ~A ~A\n"
                      key args)
       #f))
   (let ((checkers
          (inferior-eval
           '(begin
              (map (lambda (checker)
                     (list (lint-checker-name checker)
                           (lint-checker-description checker)
                           (if (memq checker %network-dependent-checkers)
                               #t
                               #f)))
                   %all-checkers))
           inf)))
     (map
      (match-lambda
        ((name description network-dependent?)
         (cons
          (list name description network-dependent?)
          (if network-dependent?
              '()
              (with-time-logging (simple-format #f "getting ~A lint warnings"
                                                name)
                (inferior-eval-with-store inf store (lint-warnings-for-checker
                                                     name)))))))
      checkers))))

(define (all-inferior-package-derivations store inf packages)
  (define inferior-%supported-systems
    (inferior-eval '(@ (guix packages) %supported-systems) inf))

  (define cross-derivations
    `(("x86_64-linux" . ("arm-linux-gnueabihf"
                         "aarch64-linux-gnu"
                         "powerpc-linux-gnu"
                         "riscv64-linux-gnu"
                         "i586-pc-gnu"))))

  (define supported-system-pairs
    (map (lambda (system)
           (cons system #f))
         inferior-%supported-systems))

  (define supported-system-cross-build-pairs
    (append-map
     (match-lambda
       ((system . targets)
        (list
         (map (lambda (target)
                (cons system target))
              targets))))
     cross-derivations))

  (define (proc packages system-target-pairs)
    `(lambda (store)
       (define target-system-alist
         '(("arm-linux-gnueabihf" . "armhf-linux")
           ("aarch64-linux-gnu" . "aarch64-linux")
           ("powerpc-linux-gnu" . "")   ; TODO I don't know?
           ("riscv64-linux-gnu" . "")   ; TODO I don't know?
           ("i586-pc-gnu" . "i586-gnu")))

       (define package-transitive-supported-systems-supports-multiple-arguments? #t)

       (define (get-supported-systems package system)
         (or (and package-transitive-supported-systems-supports-multiple-arguments?
                  (catch
                    'wrong-number-of-args
                    (lambda ()
                      (package-transitive-supported-systems package system))
                    (lambda (key . args)
                      ;; Older Guix revisions don't support two
                      ;; arguments to
                      ;; package-transitive-supported-systems
                      (simple-format
                       (current-error-port)
                       "info: package-transitive-supported-systems doesn't support two arguments, falling back to one\n")
                      (set! package-transitive-supported-systems-supports-multiple-arguments? #f)
                      #f)))
             (catch
               #t
               (lambda ()
                 (package-transitive-supported-systems package))
               (lambda (key . args)
                 (simple-format
                  (current-error-port)
                  "error: while processing ~A, unable to compute transitive supported systems\n"
                  (package-name package))
                 (simple-format
                  (current-error-port)
                  "error ~A: ~A\n" key args)
                 #f))))

       (define (derivations-for-system-and-target inferior-package-id package system target)
         (catch
           'misc-error
           (lambda ()
             (guard (c ((package-cross-build-system-error? c)
                        #f))
               (list inferior-package-id
                     system
                     target
                     (let ((file-name
                            (derivation-file-name
                             (if target
                                 (package-cross-derivation store package
                                                           target
                                                           system)
                                 (package-derivation store package system)))))
                       (add-temp-root store file-name)
                       file-name))))
           (lambda args
             ;; misc-error #f ~A ~S (No
             ;; cross-compilation for
             ;; clojure-build-system yet:
             #f)))

       (append-map
        (lambda (inferior-package-id)
          (let ((package (hashv-ref %package-table inferior-package-id)))
            (catch
              #t
              (lambda ()
                (append-map
                 (lambda (system)
                   (let ((supported-systems (package-supported-systems package)))
                     (if supported-systems
                         (filter-map
                          (lambda (target)
                            (derivations-for-system-and-target inferior-package-id
                                                               package
                                                               system
                                                               target))
                          (filter
                           (match-lambda
                             (#f #t)  ; No target
                             (target
                              (let ((system-for-target
                                     (assoc-ref target-system-alist
                                                target)))
                                (member system-for-target
                                        supported-systems
                                        string=?))))
                           (list ,@(map cdr system-target-pairs))))
                         '())))
                 (delete-duplicates
                  (list ,@(map car system-target-pairs))
                  string=?)))
              (lambda (key . args)
                (if (and (eq? key 'system-error)
                         (eq? (car args) 'fport_write))
                    (begin
                      (simple-format
                       (current-error-port)
                       "error: while processing ~A, exiting: ~A: ~A\n"
                       (package-name package)
                       key
                       args)
                      (exit 1))
                    (begin
                      (simple-format
                       (current-error-port)
                       "error: while processing ~A ignoring error: ~A: ~A\n"
                       (package-name package)
                       key
                       args)
                      '()))))))
        (list ,@(map inferior-package-id packages)))))

  (append-map
   (lambda (system-target-pairs)
     (format (current-error-port)
             "heap size: ~a MiB~%"
             (round
              (/ (assoc-ref (gc-stats) 'heap-size)
                 (expt 2. 20))))
     (with-time-logging
         (simple-format #f "getting derivations for ~A" system-target-pairs)
       (catch
         'match-error
         (lambda ()
           (inferior-eval '(invalidate-derivation-caches!) inf))
         (lambda (key . args)
           (simple-format
            (current-error-port)
            "warning: ignoring match-error from calling inferior invalidate-derivation-caches!\n")))
       (inferior-eval-with-store inf store (proc packages system-target-pairs))))
   (append (map list supported-system-pairs)
           supported-system-cross-build-pairs)))

(define (deduplicate-inferior-packages packages)
  (pair-fold
   (lambda (pair result)
     (if (null? (cdr pair))
         (cons (first pair) result)
         (let* ((a (first pair))
                (b (second pair))
                (a-name (inferior-package-name a))
                (b-name (inferior-package-name b))
                (a-version (inferior-package-version a))
                (b-version (inferior-package-version b)))
           (if (and (string=? a-name b-name)
                    (string=? a-version b-version))
               (begin
                 (simple-format (current-error-port)
                                "warning: ignoring duplicate package: ~A (~A)\n"
                                a-name
                                a-version)
                 result)
               (cons a result)))))
   '()
   (sort packages
         (lambda (a b)
           (let ((a-name (inferior-package-name a))
                 (b-name (inferior-package-name b)))
             (if (string=? a-name b-name)
                 (let ((a-version (inferior-package-version a))
                       (b-version (inferior-package-version b)))
                   (if (string=? a-version b-version)
                       ;; The name and version are the same, so try and pick
                       ;; the same package each time, by looking at the
                       ;; location.
                       (let ((a-location (inferior-package-location a))
                             (b-location (inferior-package-location b)))
                         (< (location-line a-location)
                            (location-line b-location)))
                       (string<? (inferior-package-version a)
                                 (inferior-package-version b))))
                 (string<? a-name
                           b-name)))))))

(define (insert-packages conn inf packages)
  (let* ((package-license-set-ids
          (with-time-logging "fetching inferior package license metadata"
            (inferior-packages->license-set-ids conn inf
                                                packages)))
         (packages-metadata-ids
          (with-time-logging "fetching inferior package metadata"
            (inferior-packages->package-metadata-ids
             conn packages package-license-set-ids))))

    (with-time-logging "getting package-ids"
      (inferior-packages->package-ids
       conn
       (zip (map inferior-package-name packages)
            (map inferior-package-version packages)
            packages-metadata-ids)))))

(define (insert-lint-warnings conn inferior-package-id->package-database-id
                              lint-checker-ids
                              lint-warnings-data)
  (lint-warnings-data->lint-warning-ids
   conn
   (append-map
    (lambda (lint-checker-id warnings-by-package-id)
      (append-map
       (match-lambda
         ((package-id . warnings)
          (map
           (match-lambda
             ((location-data messages-by-locale)
              (let ((location-id
                     (location->location-id
                      conn
                      (apply location location-data)))
                    (lint-warning-message-set-id
                     (lint-warning-message-data->lint-warning-message-set-id
                      conn
                      messages-by-locale)))
                (list lint-checker-id
                      (inferior-package-id->package-database-id package-id)
                      location-id
                      lint-warning-message-set-id))))
           (fold (lambda (location-and-messages result)
                   (if (member location-and-messages result)
                       (begin
                         (apply
                          simple-format
                          (current-error-port)
                          "warning: skipping duplicate lint warning ~A ~A\n"
                          location-and-messages)
                         result)
                       (append result
                               (list location-and-messages))))
                 '()
                 warnings))))
       warnings-by-package-id))
    lint-checker-ids
    (map cdr lint-warnings-data))))

(define (inferior-data->package-derivation-ids
         conn inf
         inferior-package-id->package-database-id
         inferior-data-4-tuples)
  (let ((derivation-ids
         (derivation-file-names->derivation-ids
          conn
          (map fourth inferior-data-4-tuples)))
        (flat-package-ids-systems-and-targets
         (map
          (match-lambda
            ((inferior-package-id system target derivation-file-name)
             (list (inferior-package-id->package-database-id
                    inferior-package-id)
                   system
                   (or target ""))))
          inferior-data-4-tuples)))


    (insert-package-derivations conn
                                flat-package-ids-systems-and-targets
                                derivation-ids)))

(define guix-store-path
  (let ((store-path #f))
    (lambda (store)
      (if (and store-path
               (file-exists? store-path))
          store-path
          (let ((config-guix (%config 'guix)))
            (if (and (file-exists? config-guix)
                     (string-prefix? "/gnu/store/" config-guix))
                (begin
                  (set! store-path
                    (dirname
                     (dirname
                      (%config 'guix))))
                  store-path)
                (begin
                  (invalidate-derivation-caches!)
                  (hash-clear! (@@ (guix packages) %derivation-cache))
                  (let* ((guix-package (@ (gnu packages package-management)
                                          guix))
                         (derivation (package-derivation store guix-package)))
                    (with-time-logging "building the guix derivation"
                      (build-derivations store (list derivation)))

                    (let ((new-store-path
                           (derivation->output-path derivation)))
                      (set! store-path new-store-path)
                      (simple-format (current-error-port)
                                     "debug: guix-store-path: ~A\n"
                                     new-store-path)
                      new-store-path)))))))))

(define (nss-certs-store-path store)
  (let* ((nss-certs-package (@ (gnu packages certs)
                               nss-certs))
         (derivation (package-derivation store nss-certs-package)))
    (with-time-logging "building the nss-certs derivation"
      (build-derivations store (list derivation)))
    (derivation->output-path derivation)))

(define (channel->derivation-file-names-by-system store channel)
  (define use-container? (defined?
                           'open-inferior/container
                           (resolve-module '(guix inferior))))

  (define (inferior-code channel-instance systems)
    `(lambda (store)
       (let* ((instances
               (list
                (channel-instance
                 (channel (name ',(channel-name channel))
                          (url    ,(channel-url channel))
                          (branch ,(channel-branch channel))
                          (commit ,(channel-commit channel)))
                 ,(channel-instance-commit channel-instance)
                 ,(channel-instance-checkout channel-instance)))))
         (map
          (lambda (system)
            (simple-format
             (current-error-port)
             "guix-data-service: computing the derivation-file-name for ~A\n"
             system)

            (parameterize ((%current-system system))
              (let ((manifest
                     (catch #t
                       (lambda ()
                         ((channel-instances->manifest instances) store))
                       (lambda (key . args)
                         (simple-format
                          (current-error-port)
                          "error: while computing manifest entry derivation for ~A\n"
                          system)
                         (simple-format
                          (current-error-port)
                          "error ~A: ~A\n" key args)
                         #f))))
                (define (add-tmp-root-and-return-drv drv)
                  (add-temp-root store drv)
                  drv)

                `(,system
                  .
                  ((manifest-entry-item
                    . ,(and manifest
                            (add-tmp-root-and-return-drv
                             (derivation-file-name
                              (manifest-entry-item
                               (first
                                (manifest-entries manifest)))))))
                   (profile
                    . ,(catch #t
                         (lambda ()
                           (and manifest
                                (add-tmp-root-and-return-drv
                                 (derivation-file-name
                                  (run-with-store store
                                    (profile-derivation
                                     manifest
                                     #:hooks %channel-profile-hooks))))))
                         (lambda (key . args)
                           (simple-format
                            (current-error-port)
                            "error: while computing profile derivation for ~A\n"
                            system)
                           (simple-format
                            (current-error-port)
                            "error ~A: ~A\n" key args)
                           #f))))))))
          (list ,@systems)))))

    (let ((inferior
           (if use-container?
               (open-inferior/container
                store
                (guix-store-path store)
                #:extra-shared-directories
                '("/gnu/store")
                #:extra-environment-variables
                (list (string-append
                       "SSL_CERT_DIR=" (nss-certs-store-path store))))
               (begin
                 (simple-format #t "debug: using open-inferior\n")
                 (open-inferior (guix-store-path store)
                                #:error-port (inferior-error-port))))))

      (define (start-inferior-and-return-derivation-file-names)
        ;; /etc is only missing if open-inferior/container has been used
        (when use-container?
          (inferior-eval
           '(begin
              ;; Create /etc/pass, as %known-shorthand-profiles in (guix
              ;; profiles) tries to read from this file. Because the environment
              ;; is cleaned in build-self.scm, xdg-directory in (guix utils)
              ;; falls back to accessing /etc/passwd.
              (mkdir "/etc")
              (call-with-output-file "/etc/passwd"
                (lambda (port)
                  (display "root:x:0:0::/root:/bin/bash" port))))
           inferior))

        (let ((channel-instance
               (first
                (latest-channel-instances store
                                          (list channel)))))
          (inferior-eval '(use-modules (srfi srfi-1)
                                       (guix channels)
                                       (guix grafts)
                                       (guix profiles))
                         inferior)
          (inferior-eval '(when (defined? '%graft?) (%graft? #f))
                         inferior)
          (inferior-eval '(define channel-instance
                            (@@ (guix channels) channel-instance))
                         inferior)

          (let* ((systems
                  (inferior-eval '(@ (guix packages) %supported-systems)
                                 inferior))
                 (result
                  (inferior-eval-with-store
                   inferior
                   store
                   (inferior-code channel-instance systems))))

            (close-inferior inferior)

            result)))

      (catch
        #t
        (lambda ()
          (with-throw-handler #t
            start-inferior-and-return-derivation-file-names
            (lambda (key . parameters)
              (display (backtrace) (current-error-port))
              (display "\n" (current-error-port))
              (simple-format (current-error-port)
                             "error: channel->derivation-file-names-by-system: ~A: ~A\n"
                             key parameters))))
        (lambda args
          (close-inferior inferior)
          #f))))

(define (channel->derivations-by-system conn store channel)
  (let* ((derivation-file-names-by-system
          (with-time-logging "computing the channel derivation"
            ;; Obtain a session level lock here, to avoid conflicts with
            ;; other jobs over the Git repository.
            (with-advisory-session-lock/log-time
             conn
             'channel->manifest-store-item
             (lambda ()
               (channel->derivation-file-names-by-system store channel))))))
    (for-each
     (match-lambda
       ((system . derivation-file-name)
        (simple-format (current-error-port)
                       "debug: ~A: channel dervation: ~A\n"
                       system
                       derivation-file-name)))
     derivation-file-names-by-system)

    derivation-file-names-by-system))

(define (channel-derivations-by-system->guix-store-item
         store
         channel-derivations-by-system)

  (define (store-item->guix-store-item filename)
    (dirname
     (readlink
      (string-append filename "/bin"))))

  (let ((derivation-file-name-for-current-system
         (assoc-ref
          (assoc-ref channel-derivations-by-system
                     (%current-system))
          'profile)))
    (if derivation-file-name-for-current-system
        (let ((derivation-for-current-system
               (read-derivation-from-file derivation-file-name-for-current-system)))
          (with-time-logging "building the channel derivation"
            (build-derivations store (list derivation-for-current-system)))

          (store-item->guix-store-item
           (derivation->output-path derivation-for-current-system)))
        #f)))

(define (glibc-locales-for-guix-store-path store store-path)
  (let ((inf (if (defined?
                   'open-inferior/container
                   (resolve-module '(guix inferior)))
                 (open-inferior/container store store-path
                                          #:extra-shared-directories
                                          '("/gnu/store"))
                 (begin
                   (simple-format #t "debug: using open-inferior\n")
                   (open-inferior store-path
                                  #:error-port (inferior-error-port))))))
    (inferior-eval '(use-modules (srfi srfi-1)
                                 (srfi srfi-34)
                                 (guix grafts)
                                 (guix derivations))
                   inf)
    (inferior-eval '(when (defined? '%graft?) (%graft? #f)) inf)

    (let* ((inferior-glibc-locales
            (first
             (lookup-inferior-packages inf "glibc-locales")))
           (derivation (inferior-package-derivation store
                                                    inferior-glibc-locales))
           (output (derivation->output-path derivation)))
      (close-inferior inf)
      (with-time-logging "building the glibc-locales derivation"
        (build-derivations store (list derivation)))

      output)))

(define (extract-information-from conn store guix-revision-id commit store-path)
  (simple-format #t "debug: extract-information-from: ~A\n" store-path)
  (let* ((guix-locpath (getenv "GUIX_LOCPATH"))
         (inf (let ((guix-locpath
                     ;; Augment the GUIX_LOCPATH to include glibc-locales from
                     ;; the Guix at store-path, this should mean that the
                     ;; inferior Guix works, even if it's build using a different
                     ;; glibc version
                     (string-append
                      (glibc-locales-for-guix-store-path store store-path)
                      "/lib/locale"
                      ":" guix-locpath)))
                ;; Unset the GUILE_LOAD_PATH and GUILE_LOAD_COMPILED_PATH to
                ;; avoid the values for these being used in the
                ;; inferior. Even though the inferior %load-path and
                ;; %load-compiled-path has the inferior modules first, this
                ;; can cause issues when there are modules present outside
                ;; of the inferior Guix which aren't present in the inferior
                ;; Guix (like the new (guix lint) module
                (unsetenv "GUILE_LOAD_PATH")
                (unsetenv "GUILE_LOAD_COMPILED_PATH")
                (simple-format (current-error-port) "debug: set GUIX_LOCPATH to ~A\n"
                               guix-locpath)
                (if (defined?
                      'open-inferior/container
                      (resolve-module '(guix inferior)))
                    (open-inferior/container store store-path
                                             #:extra-shared-directories
                                             '("/gnu/store")
                                             #:extra-environment-variables
                                             (list (string-append
                                                    "GUIX_LOCPATH="
                                                    guix-locpath)))
                    (begin
                      (setenv "GUIX_LOCPATH" guix-locpath)
                      (simple-format #t "debug: using open-inferior\n")
                      (open-inferior store-path
                                     #:error-port (inferior-error-port)))))))
    (setenv "GUIX_LOCPATH" guix-locpath) ; restore GUIX_LOCPATH

    (when (eq? inf #f)
      (error "error: inferior is #f"))

    ;; Normalise the locale for the inferior process
    (catch
      'system-error
      (lambda ()
        (inferior-eval '(setlocale LC_ALL "en_US.utf8") inf))
      (lambda (key . args)
        (simple-format (current-error-port)
                       "warning: failed to set locale to en_US.utf8: ~A ~A\n"
                       key args)
        (display "trying to setlocale to en_US.UTF-8 instead\n"
                 (current-error-port))
        (with-exception-handler
            (lambda (key . args)
              (simple-format
               (current-error-port)
               "warning: failed to set locale to en_US.UTF-8: ~A ~A\n"
               key args))
          (lambda ()
            (inferior-eval '(setlocale LC_ALL "en_US.UTF-8") inf)))))

    (inferior-eval '(use-modules (srfi srfi-1)
                                 (srfi srfi-34)
                                 (guix grafts)
                                 (guix derivations)
                                 (gnu tests))
                   inf)
    (inferior-eval '(when (defined? '%graft?) (%graft? #f)) inf)

    (catch
      #t
      (lambda ()
        (let* ((packages
                (with-time-logging "fetching inferior packages"
                  (deduplicate-inferior-packages
                   (inferior-packages inf))))
               (inferior-lint-warnings
                (with-time-logging "fetching inferior lint warnings"
                  (all-inferior-lint-warnings inf store)))
               (inferior-data-4-tuples
                (with-time-logging "getting inferior derivations"
                  (all-inferior-package-derivations store inf packages)))
               (inferior-system-tests
                (with-time-logging "getting inferior system tests"
                  (all-inferior-system-tests inf store))))

          (with-time-logging
              "acquiring advisory transaction lock: load-new-guix-revision-inserts"
            ;; Wait until this is the only transaction inserting data, to
            ;; avoid any concurrency issues
            (obtain-advisory-transaction-lock conn
                                              'load-new-guix-revision-inserts))
          (let* ((package-ids
                  (insert-packages conn inf packages))
                 (inferior-package-id->package-database-id
                  (let ((lookup-table
                         (alist->hashq-table
                          (map (lambda (package package-id)
                                 (cons (inferior-package-id package)
                                       package-id))
                               packages
                               package-ids))))
                    (lambda (inferior-id)
                      (or
                       (hashq-ref lookup-table inferior-id)
                       (error
                        (simple-format
                         #f
                         "error: inferior-package-id->package-database-id: ~A missing\n"
                         inferior-id)))))))

            (simple-format
             #t "debug: finished loading information from inferior\n")
            (close-inferior inf)

            (when inferior-lint-warnings
              (let* ((lint-checker-ids
                      (lint-checkers->lint-checker-ids
                       conn
                       (map car inferior-lint-warnings)))
                     (lint-warning-ids
                      (insert-lint-warnings
                       conn
                       inferior-package-id->package-database-id
                       lint-checker-ids
                       inferior-lint-warnings)))
                (insert-guix-revision-lint-checkers conn
                                                    guix-revision-id
                                                    lint-checker-ids)

                (insert-guix-revision-lint-warnings conn
                                                    guix-revision-id
                                                    lint-warning-ids)))

            (insert-system-tests-for-guix-revision conn
                                                   guix-revision-id
                                                   inferior-system-tests)

            (let ((package-derivation-ids
                   (with-time-logging "inferior-data->package-derivation-ids"
                     (inferior-data->package-derivation-ids
                      conn inf inferior-package-id->package-database-id
                      inferior-data-4-tuples))))
              (update-builds-derivation-output-details-set-id
               conn
               (map fourth inferior-data-4-tuples))

              (insert-guix-revision-package-derivations conn
                                                        guix-revision-id
                                                        package-derivation-ids)
              (simple-format
               #t "Successfully loaded ~A package/derivation pairs\n"
               (length package-derivation-ids)))))
        #t)
      (lambda (key . args)
        (simple-format (current-error-port)
                       "Failed extracting information from commit: ~A\n\n" commit)
        (simple-format (current-error-port)
                       "  ~A ~A\n\n" key args)
        #f)
      (lambda (key . args)
        (display-backtrace (make-stack #t) (current-error-port))))))

(define (update-package-versions-table conn git-repository-id commit)
  (with-time-logging "lock table: package_versions_by_guix_revision_range"
    ;; Lock the table to wait for other transactions to commit before updating
    ;; the table
    (exec-query
     conn
     "
LOCK TABLE ONLY package_versions_by_guix_revision_range
  IN SHARE ROW EXCLUSIVE MODE"))

  (for-each
   (match-lambda
     ((branch-name)
      (with-time-logging
          (simple-format #f "deleting package version entries for ~A" branch-name)
        (exec-query
         conn
         "
DELETE FROM package_versions_by_guix_revision_range
WHERE git_repository_id = $1 AND branch_name = $2"
         (list git-repository-id
               branch-name)))
      (with-time-logging
          (simple-format #f "inserting package version entries for ~A" branch-name)
        (exec-query
         conn
         "
INSERT INTO package_versions_by_guix_revision_range
SELECT DISTINCT
       $1::integer AS git_repository_id,
       $2 AS branch_name,
       packages.name AS package_name,
       packages.version AS package_version,
       first_value(guix_revisions.id)
         OVER package_version AS first_guix_revision_id,
       last_value(guix_revisions.id)
         OVER package_version AS last_guix_revision_id
FROM packages
INNER JOIN (
  SELECT DISTINCT package_derivations.package_id,
                  guix_revision_package_derivations.revision_id
  FROM package_derivations
  INNER JOIN guix_revision_package_derivations
    ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
) AS revision_packages ON packages.id = revision_packages.package_id
INNER JOIN guix_revisions ON revision_packages.revision_id = guix_revisions.id
INNER JOIN git_branches ON guix_revisions.commit = git_branches.commit
WHERE git_branches.name = $2
WINDOW package_version AS (
  PARTITION BY packages.name, packages.version
  ORDER BY git_branches.datetime
  RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
)
ORDER BY packages.name, packages.version"
         (list git-repository-id branch-name)))))
   (exec-query
    conn
    "SELECT name FROM git_branches WHERE commit = $1 AND git_repository_id = $2"
    (list commit git-repository-id)))

  #t)

(define (load-new-guix-revision conn store git-repository-id commit)
  (let* ((channel-for-commit
          (channel (name 'guix)
                   (url (git-repository-id->url
                         conn
                         git-repository-id))
                   (commit commit)))
         (channel-derivations-by-system
          (channel->derivations-by-system conn
                                          store
                                          channel-for-commit))
         (store-item
          (channel-derivations-by-system->guix-store-item
           store
           channel-derivations-by-system)))
    (if store-item
        (let ((guix-revision-id
               (insert-guix-revision conn git-repository-id
                                     commit store-item)))
          (and
           guix-revision-id
           (extract-information-from conn store
                                     guix-revision-id
                                     commit store-item)
           (insert-channel-instances conn
                                     guix-revision-id
                                     (filter-map
                                      (match-lambda
                                        ((system . derivations)
                                         (and=>
                                          (assoc-ref derivations
                                                     'manifest-entry-item)
                                          (lambda (drv)
                                            (cons system drv)))))
                                      channel-derivations-by-system))
           (if (defined? 'channel-news-for-commit
                 (resolve-module '(guix channels)))
               (with-time-logging "inserting channel news entries"
                 (insert-channel-news-entries-for-guix-revision
                  conn
                  guix-revision-id
                  (channel-news-for-commit channel-for-commit commit)))
               (begin
                 (simple-format #t "debug: importing channel news not supported\n")
                 #t))

           (update-package-versions-table conn git-repository-id commit)
           (update-package-derivations-table conn
                                             git-repository-id
                                             guix-revision-id
                                             commit)))
        (begin
          (simple-format #t "Failed to generate store item for ~A\n"
                         commit)
          #f))))

(define (enqueue-load-new-guix-revision-job conn git-repository-id commit source)
  (define query
    "
INSERT INTO load_new_guix_revision_jobs (git_repository_id, commit, source)
VALUES ($1, $2, $3)
ON CONFLICT DO NOTHING
RETURNING id;")

  (match (exec-query conn
                     query
                     (list (number->string git-repository-id)
                           commit
                           source))
    ((result)
     result)
    (() #f)))

(define (select-job-for-commit conn commit)
  (let ((result
         (exec-query
          conn
          (string-append
           "SELECT id, commit, source, git_repository_id "
           "FROM load_new_guix_revision_jobs WHERE commit = $1")
          (list commit))))
    result))

(define* (select-recent-job-events conn
                                   #:key (limit 8))
  (define query
    (string-append
     "
SELECT
  load_new_guix_revision_jobs.id,
  load_new_guix_revision_jobs.commit,
  load_new_guix_revision_jobs.source,
  load_new_guix_revision_jobs.git_repository_id,
  load_new_guix_revision_job_events.event,
  load_new_guix_revision_job_events.occurred_at
FROM load_new_guix_revision_jobs
INNER JOIN load_new_guix_revision_job_events
  ON load_new_guix_revision_job_events.job_id = load_new_guix_revision_jobs.id
ORDER BY load_new_guix_revision_job_events.occurred_at DESC
LIMIT " (number->string limit)))

  (exec-query conn query))

(define (select-jobs-and-events conn before-id limit)
  (define query
    (string-append
     "
SELECT
  load_new_guix_revision_jobs.id,
  load_new_guix_revision_jobs.commit,
  load_new_guix_revision_jobs.source,
  load_new_guix_revision_jobs.git_repository_id,
  load_new_guix_revision_jobs.created_at,
  load_new_guix_revision_jobs.succeeded_at,
  (
    SELECT JSON_AGG(
      json_build_object('event', event, 'occurred_at', occurred_at) ORDER BY occurred_at ASC
    )
    FROM load_new_guix_revision_job_events
    WHERE job_id = load_new_guix_revision_jobs.id
  ),
  EXISTS (
    SELECT 1 FROM load_new_guix_revision_job_logs WHERE job_id = load_new_guix_revision_jobs.id
  ) AS log_exists
FROM load_new_guix_revision_jobs
"
     (if before-id
         (string-append
          "WHERE load_new_guix_revision_jobs.id < "
          (number->string before-id))
         "")
     "
ORDER BY load_new_guix_revision_jobs.id DESC
"
     (if limit
         (string-append
          "LIMIT " (number->string limit))
         "")))

  (map
   (match-lambda
     ((id commit source git-repository-id created-at succeeded-at
          events-json log-exists?)
      (list id commit source git-repository-id created-at succeeded-at
            (if (string-null? events-json)
                #()
                (json-string->scm events-json))
            (string=? log-exists? "t"))))
   (exec-query conn query)))

(define (select-unprocessed-jobs-and-events conn)
  (define query
    "
SELECT
  load_new_guix_revision_jobs.id,
  load_new_guix_revision_jobs.commit,
  load_new_guix_revision_jobs.source,
  load_new_guix_revision_jobs.git_repository_id,
  load_new_guix_revision_jobs.created_at,
  (
    SELECT JSON_AGG(
      json_build_object('event', event, 'occurred_at', occurred_at) ORDER BY occurred_at ASC
    )
    FROM load_new_guix_revision_job_events
    WHERE job_id = load_new_guix_revision_jobs.id
  ),
  EXISTS (
    SELECT 1 FROM load_new_guix_revision_job_logs WHERE job_id = load_new_guix_revision_jobs.id
  ) AS log_exists,
  commit IN (
    SELECT commit FROM (
      SELECT DISTINCT ON (name)
        name, git_branches.commit
      FROM git_branches
      WHERE
        git_branches.git_repository_id = load_new_guix_revision_jobs.git_repository_id AND
        git_branches.commit IS NOT NULL
      ORDER BY name, datetime DESC
    ) branches_and_latest_commits
  ) AS latest_branch_commit
FROM load_new_guix_revision_jobs
WHERE
  succeeded_at IS NULL AND
  (
    SELECT COUNT(*)
    FROM load_new_guix_revision_job_events
    WHERE job_id = load_new_guix_revision_jobs.id AND event = 'retry'
  ) >= (
    SELECT COUNT(*)
    FROM load_new_guix_revision_job_events
    WHERE job_id = load_new_guix_revision_jobs.id AND event = 'failure'
  )
ORDER BY latest_branch_commit DESC, id DESC")

  (map
   (match-lambda
     ((id commit source git-repository-id created-at
          events-json log-exists? latest-branch-commit)
      (list id commit source git-repository-id created-at
            (if (string-null? events-json)
                #()
                (json-string->scm events-json))
            (string=? log-exists? "t")
            (string=? latest-branch-commit "t"))))
   (exec-query conn query)))

(define (select-jobs-and-events-for-commit conn commit)
  (define query
    "
SELECT
  load_new_guix_revision_jobs.id,
  load_new_guix_revision_jobs.source,
  load_new_guix_revision_jobs.git_repository_id,
  load_new_guix_revision_jobs.created_at,
  load_new_guix_revision_jobs.succeeded_at,
  (
    SELECT JSON_AGG(
      json_build_object('event', event, 'occurred_at', occurred_at) ORDER BY occurred_at ASC
    )
    FROM load_new_guix_revision_job_events
    WHERE job_id = load_new_guix_revision_jobs.id
  ),
  EXISTS (
    SELECT 1 FROM load_new_guix_revision_job_logs WHERE job_id = load_new_guix_revision_jobs.id
  ) AS log_exists
FROM load_new_guix_revision_jobs
WHERE commit = $1
ORDER BY load_new_guix_revision_jobs.id DESC")

  (map
   (match-lambda
     ((id source git-repository-id created-at succeeded-at
          events-json log-exists?)
      (list id commit source git-repository-id created-at succeeded-at
            (if (string-null? events-json)
                #()
                (json-string->scm events-json))
            (string=? log-exists? "t"))))
   (exec-query conn query (list commit))))

(define (most-recent-n-load-new-guix-revision-jobs conn n)
  (let ((result
         (exec-query
          conn
          (string-append
           "SELECT id, commit, source, git_repository_id "
           "FROM load_new_guix_revision_jobs ORDER BY id ASC LIMIT $1")
          (list (number->string n)))))
    result))

(define (select-job-for-update conn id)
  (exec-query
   conn
   (string-append
    "SELECT id, commit, source, git_repository_id "
    "FROM load_new_guix_revision_jobs "
    "WHERE id = $1 AND succeeded_at IS NULL "
    "FOR NO KEY UPDATE SKIP LOCKED")
   (list id)))

(define (record-job-event conn job-id event)
  (exec-query
   conn
   (string-append
    "INSERT INTO load_new_guix_revision_job_events (job_id, event) "
    "VALUES ($1, $2)")
   (list job-id event)))

(define (record-job-succeeded conn id)
  (exec-query
   conn
   (string-append
    "UPDATE load_new_guix_revision_jobs "
    "SET succeeded_at = clock_timestamp() "
    "WHERE id = $1 ")
   (list id)))

(define (fetch-unlocked-jobs conn)
  (define query "
SELECT
  id,
  commit IN (
    SELECT commit FROM (
      SELECT DISTINCT ON (name)
        name, git_branches.commit
      FROM git_branches
      WHERE
        git_branches.git_repository_id = load_new_guix_revision_jobs.git_repository_id AND
        git_branches.commit IS NOT NULL
      ORDER BY name, datetime DESC
    ) branches_and_latest_commits
  ) AS latest_branch_commit
FROM load_new_guix_revision_jobs
WHERE
  succeeded_at IS NULL AND
  (
    SELECT COUNT(*)
    FROM load_new_guix_revision_job_events
    WHERE job_id = load_new_guix_revision_jobs.id AND event = 'retry'
  ) >= (
    SELECT COUNT(*)
    FROM load_new_guix_revision_job_events
    WHERE job_id = load_new_guix_revision_jobs.id AND event = 'failure'
  )
ORDER BY latest_branch_commit DESC, id DESC
FOR NO KEY UPDATE OF load_new_guix_revision_jobs
SKIP LOCKED")

  (map
   (match-lambda
     ((id priority)
      (list id
            (string=? priority "t"))))
   (exec-query conn query)))

(define (with-store-connection f)
  (with-store store
    (set-build-options store #:fallback? #t)

    (f store)))

(define (setup-logging id thunk)
  (let* ((previous-output-port (current-output-port))
         (previous-error-port (current-error-port))
         (result
          (with-postgresql-connection
           (simple-format #f "load-new-guix-revision ~A logging" id)
           (lambda (logging-conn)
             (insert-empty-log-entry logging-conn id)
             (let ((logging-port
                    (log-port id logging-conn
                              #:delete-existing-log-parts? #t)))
               (set-current-output-port logging-port)
               (set-current-error-port logging-port)
               (let ((result
                      (parameterize ((current-build-output-port logging-port)
                                     (real-error-port previous-error-port)
                                     (inferior-error-port
                                      (setup-port-for-inferior-error-output
                                       id previous-error-port)))
                        (thunk))))
                 (combine-log-parts! logging-conn id)
                 (drop-log-parts-sequence logging-conn id)

                 ;; This can happen with GC, so do it explicitly
                 (close-port logging-port)

                 result))))))
    (set-current-output-port previous-output-port)
    (set-current-error-port previous-error-port)
    result))

(define (process-load-new-guix-revision-job id)
  (with-postgresql-connection
   (simple-format #f "load-new-guix-revision ~A" id)
   (lambda (conn)
     ;; Fix the hash encoding of derivation_output_details. This'll only run
     ;; once on any given database, but is kept here just to make sure any
     ;; instances have the data updated.
     (fix-derivation-output-details-hash-encoding conn)

     (exec-query conn "BEGIN")

     (match (select-job-for-update conn id)
       (((id commit source git-repository-id))

        ;; With a separate connection, outside of the transaction so the event
        ;; gets persisted regardless.
        (with-postgresql-connection
         (simple-format #f "load-new-guix-revision ~A start-event" id)
         (lambda (start-event-conn)
           (record-job-event start-event-conn id "start")))

        (simple-format #t "Processing job ~A (commit: ~A, source: ~A)\n\n"
                       id commit source)

        (if (or
             (guix-revision-exists? conn git-repository-id commit)
             (eq?
              (with-time-logging (string-append "loading revision " commit)
                (setup-logging
                 id
                 (lambda ()
                   (catch #t
                     (lambda ()
                       (with-store-connection
                        (lambda (store)
                          (load-new-guix-revision conn
                                                  store
                                                  git-repository-id
                                                  commit))))
                     (lambda (key . args)
                       (simple-format
                        (current-error-port)
                        "error: load-new-guix-revision: ~A ~A\n"
                        key args)
                       #f)))))
              #t))
            (begin
              (record-job-succeeded conn id)
              (record-job-event conn id "success")
              (exec-query conn "COMMIT")
              #t)
            (begin
              (exec-query conn "ROLLBACK")
              (record-job-event conn id "failure")
              #f)))
       (()
        (exec-query conn "ROLLBACK")
        (simple-format #t "job ~A not found to be processed\n"
                       id))))))
