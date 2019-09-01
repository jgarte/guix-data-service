(define-module (guix-data-service jobs load-new-guix-revision)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
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
  #:use-module (guix-data-service model package)
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
  #:export (log-for-job
            count-log-parts
            combine-log-parts!
            fetch-unlocked-jobs
            process-load-new-guix-revision-job
            select-job-for-commit
            select-jobs-and-events
            select-jobs-and-events-for-commit
            record-job-event
            enqueue-load-new-guix-revision-job
            most-recent-n-load-new-guix-revision-jobs))

(define (log-port job-id conn)
  (define output-port
    (current-output-port))

  (define id 0)
  (define buffer "")

  (define (insert job_id s)
    (exec-query
     conn
     (string-append
      "INSERT INTO load_new_guix_revision_job_log_parts (id, job_id, contents) "
      "VALUES ($1, $2, $3)")
     (list (number->string id) job_id s)))

  (define (log-string s)
    (if (string-contains s "\n")
        (let ((output (string-append buffer s)))
          (set! id (+ 1 id)) ; increment id
          (set! buffer "") ; clear the buffer
          (insert job-id output)
          (display output output-port))
        (set! buffer (string-append buffer s))))

  ;; TODO, this is useful when re-running jobs, but I'm not sure that should
  ;; be a thing, jobs should probably be only attempted once.
  (exec-query
   conn
   "DELETE FROM load_new_guix_revision_job_log_parts WHERE job_id = $1"
   (list job-id))

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

(define inferior-package-id
  (@@ (guix inferior) inferior-package-id))

(define (log-time action f)
  (simple-format #t "debug: Starting ~A\n" action)
  (let* ((start-time (current-time))
         (result (f))
         (time-taken (- (current-time) start-time)))
    (simple-format #t "debug: Finished ~A, took ~A seconds\n"
                   action time-taken)
    result))

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

(define (all-inferior-lint-warnings inf store)
  (define checkers
    (inferior-eval
     '(begin
        (use-modules (guix lint))
        (map (lambda (checker)
               (list (lint-checker-name checker)
                     (lint-checker-description checker)
                     (if (memq checker %network-dependent-checkers)
                         #t
                         #f)))
             %all-checkers))
     inf))

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
         (filter
          (match-lambda
            ((package-id . warnings)
             (not (null? warnings))))
          (hash-map->list
           (lambda (package-id package)
             (cons
              package-id
              (map
               (lambda (lint-warning)
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
               (check package))))
           %package-table)))))

  (map
   (match-lambda
     ((name description network-dependent?)
      (cons
       (list name description network-dependent?)
       (if network-dependent?
           '()
           (log-time
            (simple-format #f "getting ~A lint warnings" name)
            (lambda ()
              (inferior-eval-with-store inf store (lint-warnings-for-checker
                                                   name))))))))
   checkers))

(define (all-inferior-package-derivations store inf packages)
  (define inferior-%supported-systems
    (inferior-eval '(@ (guix packages) %supported-systems) inf))

  (define supported-system-pairs
    (map (lambda (system)
           (cons system system))
         inferior-%supported-systems))

  (define supported-system-cross-build-pairs
    (map (lambda (system)
           (filter-map (lambda (target)
                         (and (not (string=? system target))
                              (cons system target)))
                       inferior-%supported-systems))
         inferior-%supported-systems))

  (define (proc packages system-target-pairs)
    `(lambda (store)
       (append-map
        (lambda (inferior-package-id)
          (let ((package (hashv-ref %package-table inferior-package-id)))
            (catch
              #t
              (lambda ()
                (let ((supported-systems
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
                  (if supported-systems
                      (append-map
                       (lambda (system)
                         (filter-map
                          (lambda (target)
                            (catch
                              'misc-error
                              (lambda ()
                                (guard (c ((package-cross-build-system-error? c)
                                           #f))
                                  (list inferior-package-id
                                        system
                                        target
                                        (derivation-file-name
                                         (if (string=? system target)
                                             (package-derivation store package system)
                                             (package-cross-derivation store package
                                                                       target
                                                                       system))))))
                              (lambda args
                                ;; misc-error #f ~A ~S (No
                                ;; cross-compilation for
                                ;; clojure-build-system yet:
                                #f)))
                          (lset-intersection
                           string=?
                           supported-systems
                           (list ,@(map cdr system-target-pairs)))))
                       (lset-intersection
                        string=?
                        supported-systems
                        (list ,@(map car system-target-pairs))))
                      '())))
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
     (log-time
      (simple-format #f "getting derivations for ~A" system-target-pairs)
      (lambda ()
        (inferior-eval '(invalidate-derivation-caches!) inf)
        (inferior-eval-with-store inf store (proc packages system-target-pairs)))))
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
                 (string<? (inferior-package-version a)
                           (inferior-package-version b))
                 (string<? a-name
                           b-name)))))))

(define (insert-packages conn inf packages)
  (let* ((package-license-set-ids
          (log-time "fetching inferior package license metadata"
                    (lambda ()
                      (inferior-packages->license-set-ids conn inf
                                                          packages))))
         (packages-metadata-ids
          (log-time "fetching inferior package metadata"
                    (lambda ()
                      (inferior-packages->package-metadata-ids
                       conn packages package-license-set-ids)))))

    (log-time "getting package-ids"
              (lambda ()
                (inferior-packages->package-ids
                 conn packages packages-metadata-ids)))))

(define (insert-lint-warnings conn inferior-package-id->package-database-id
                              lint-warnings-data)
  (let ((lint-checker-ids
         (lint-checkers->lint-checker-ids
          conn
          (map car lint-warnings-data))))

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
                            "warning: skipping duplicate lint warning ~A ~A"
                            location-and-messages)
                           result)
                         (append result
                                 (list location-and-messages))))
                   '()
                   warnings))))
         warnings-by-package-id))
      lint-checker-ids
      (map cdr lint-warnings-data)))))

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
                   target)))
          inferior-data-4-tuples)))


    (insert-package-derivations conn
                                flat-package-ids-systems-and-targets
                                derivation-ids)))

(define (inferior-package-transitive-supported-systems package)
  ((@@ (guix inferior) inferior-package-field)
   package
   'package-transitive-supported-systems))

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
                    (log-time
                     "building the guix derivation"
                     (lambda ()
                       (build-derivations store (list derivation))))

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
    (log-time
     "building the nss-certs derivation"
     (lambda ()
       (build-derivations store (list derivation))))
    (derivation->output-path derivation)))

(define (channel->derivation-file-name store channel)
  (define use-container? (defined?
                           'open-inferior/container
                           (resolve-module '(guix inferior))))

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
               (open-inferior (guix-store-path store))))))

    (catch
      #t
      (lambda ()
        (with-throw-handler #t
          (lambda ()
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
              (inferior-eval '(use-modules (guix channels)
                                           (guix profiles))
                             inferior)
              (inferior-eval '(define channel-instance
                                (@@ (guix channels) channel-instance))
                             inferior)

              (let ((file-name
                     (inferior-eval-with-store
                      inferior
                      store
                      `(lambda (store)
                         (let ((instances
                                (list
                                 (channel-instance
                                  (channel (name ',(channel-name channel))
                                           (url    ,(channel-url channel))
                                           (branch ,(channel-branch channel))
                                           (commit ,(channel-commit channel)))
                                  ,(channel-instance-commit channel-instance)
                                  ,(channel-instance-checkout channel-instance)))))
                           (run-with-store store
                             (mlet* %store-monad ((manifest (channel-instances->manifest instances))
                                                  (derv (profile-derivation manifest)))
                               (mbegin %store-monad
                                 (return (derivation-file-name derv))))))))))

                (close-inferior inferior)

                file-name)))
          (lambda (key . parameters)
            (display (backtrace) (current-error-port))
            (display "\n" (current-error-port))
            (simple-format (current-error-port)
                           "error: channel->derivation-file-name: ~A: ~A\n"
                           key parameters))))
      (lambda args
        (close-inferior inferior)
        #f))))

(define (channel->manifest-store-item conn store channel)
  (let* ((manifest-store-item-derivation-file-name
          (log-time
           "computing the channel derivation"
           (lambda ()
             ;; Obtain a session level lock here, to avoid conflicts with
             ;; other jobs over the Git repository.
             (with-advisory-session-lock
              conn
              'channel->manifest-store-item
              (lambda ()
                (channel->derivation-file-name store channel))))))
         (derivation
          (read-derivation-from-file manifest-store-item-derivation-file-name)))
    (log-time
     "building the channel derivation"
     (lambda ()
       (build-derivations store (list derivation))))
    (derivation->output-path derivation)))

(define (channel->guix-store-item conn store channel)
  (catch
    #t
    (lambda ()
      (dirname
       (readlink
        (string-append (channel->manifest-store-item conn
                                                     store
                                                     channel)
                       "/bin"))))
    (lambda args
      (simple-format #t "guix-data-service: load-new-guix-revision: error: ~A\n" args)
      #f)))

(define (extract-information-from conn git-repository-id commit store-path)
  (simple-format
   #t "debug: extract-information-from: ~A\n" store-path)
  (with-store store
    (set-build-options store
                       #:fallback? #t)
    (let ((inf (if (defined?
                     'open-inferior/container
                     (resolve-module '(guix inferior)))
                   (open-inferior/container store store-path
                                            #:extra-shared-directories
                                            '("/gnu/store"))
                   (begin
                     (simple-format #t "debug: using open-inferior\n")
                     (open-inferior store-path)))))
      (inferior-eval '(use-modules (srfi srfi-1)
                                   (srfi srfi-34)
                                   (guix grafts)
                                   (guix derivations))
                     inf)
      (inferior-eval '(%graft? #f) inf)

      (catch
        #t
        (lambda ()
          (let* ((packages
                  (log-time
                   "fetching inferior packages"
                   (lambda ()
                     (deduplicate-inferior-packages
                      (inferior-packages inf)))))
                 (inferior-lint-warnings
                  (log-time
                   "fetching inferior lint warnings"
                   (lambda ()
                     (all-inferior-lint-warnings inf store))))
                 (inferior-data-4-tuples
                  (log-time
                   "getting inferior derivations"
                   (lambda ()
                     (all-inferior-package-derivations store inf packages)))))

            ;; Wait until this is the only transaction inserting data, to
            ;; avoid any concurrency issues
            (obtain-advisory-transaction-lock conn
                                              'load-new-guix-revision-inserts)

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
                        (hashq-ref lookup-table inferior-id)))))

              (simple-format
               #t "debug: finished loading information from inferior\n")
              (close-inferior inf)

              (let* ((lint-warning-ids
                      (insert-lint-warnings
                       conn
                       inferior-package-id->package-database-id
                       inferior-lint-warnings))
                     (package-derivation-ids
                      (inferior-data->package-derivation-ids
                       conn inf inferior-package-id->package-database-id
                       inferior-data-4-tuples))
                     (guix-revision-id
                      (insert-guix-revision conn git-repository-id
                                            commit store-path)))

                (insert-guix-revision-lint-warnings conn
                                                    guix-revision-id
                                                    lint-warning-ids)

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
          (display-backtrace (make-stack #t) (current-error-port)))))))


(define (store-item-for-git-repository-id-and-commit
         conn git-repository-id commit)
  (with-store store
    (set-build-options store
                       #:fallback? #t)
    (channel->guix-store-item
     conn
     store
     (channel (name 'guix)
              (url (git-repository-id->url
                    conn
                    git-repository-id))
              (commit commit)))))

(define (load-new-guix-revision conn git-repository-id commit)
  (let ((store-item
         (store-item-for-git-repository-id-and-commit
          conn git-repository-id commit)))
    (if store-item
        (extract-information-from conn git-repository-id
                                  commit store-item)
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
                     (list git-repository-id commit source))
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

(define (select-jobs-and-events conn)
  (define query
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
ORDER BY load_new_guix_revision_jobs.id DESC")

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
  NOT EXISTS (
    SELECT 1
    FROM load_new_guix_revision_job_events
    -- Skip jobs that have failed, to avoid trying them over and over again
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

(define (process-load-new-guix-revision-job id)
  (with-postgresql-connection
   (simple-format #f "load-new-guix-revision ~A" id)
   (lambda (conn)
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

        (if (or (guix-revision-exists? conn git-repository-id commit)
                (eq? (log-time
                      (string-append "loading revision " commit)
                      (lambda ()
                        (let* ((previous-output-port (current-output-port))
                               (previous-error-port (current-error-port))
                               (result
                                (with-postgresql-connection
                                 (simple-format #f "load-new-guix-revision ~A logging" id)
                                 (lambda (logging-conn)
                                   (insert-empty-log-entry logging-conn id)
                                   (let ((logging-port (log-port id logging-conn)))
                                     (set-current-output-port logging-port)
                                     (set-current-error-port logging-port)
                                     (let ((result
                                            (parameterize ((current-build-output-port logging-port))
                                              (load-new-guix-revision conn git-repository-id commit))))
                                       (combine-log-parts! logging-conn id)

                                       ;; This can happen with GC, so do it explicitly
                                       (close-port logging-port)

                                       result))))))
                          (set-current-output-port previous-output-port)
                          (set-current-error-port previous-error-port)
                          result)))
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
        (simple-format #t "job ~A not found to be processed\n"
                       id))))))
