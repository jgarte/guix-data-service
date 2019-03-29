(define-module (guix-data-service jobs load-new-guix-revision)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:use-module (squee)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix progress)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix build utils)
  #:use-module (guix-data-service model package)
  #:use-module (guix-data-service model guix-revision)
  #:use-module (guix-data-service model package-derivation)
  #:use-module (guix-data-service model guix-revision-package-derivation)
  #:use-module (guix-data-service model package-metadata)
  #:use-module (guix-data-service model derivation)
  #:export (process-next-load-new-guix-revision-job
            select-job-for-commit
            enqueue-load-new-guix-revision-job
            most-recent-n-load-new-guix-revision-jobs))

(define inferior-package-id
  (@@ (guix inferior) inferior-package-id))

(define (log-time action f)
  (simple-format #t "debug: Starting ~A\n" action)
  (force-output)
  (let* ((start-time (current-time))
         (result (f))
         (time-taken (- (current-time) start-time)))
    (simple-format #t "debug: Finished ~A, took ~A seconds\n"
                   action time-taken)
    (force-output)
    result))

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
                       (package-transitive-supported-systems package)))
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
                    (list ,@(map car system-target-pairs))))))
              (lambda args
                (simple-format (current-error-port)
                               "error: while processing ~A ignoring error: ~A\n"
                               (package-name package)
                               args)
                '()))))
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

(define (inferior-guix->package-derivation-ids store conn inf)
  (let* ((packages (log-time "fetching inferior packages"
                             (lambda ()
                               (deduplicate-inferior-packages
                                (inferior-packages inf)))))
         (packages-metadata-ids
          (log-time "fetching inferior package metadata"
                    (lambda ()
                      (inferior-packages->package-metadata-ids conn packages))))
         (package-ids
          (log-time "getting package-ids"
                    (lambda ()
                      (inferior-packages->package-ids
                       conn packages packages-metadata-ids))))
         (inferior-package-id->package-id-hash-table
          (alist->hashq-table
           (map (lambda (package package-id)
                  (cons (inferior-package-id package)
                        package-id))
                packages
                package-ids)))
         (inferior-data-4-tuples
          (log-time "getting inferior derivations"
                    (lambda ()
                      (all-inferior-package-derivations store inf packages)))))

    (simple-format
     #t "debug: finished loading information from inferior\n")
    (close-inferior inf)

    (let ((derivation-ids
           (derivation-file-names->derivation-ids
            conn
            (map fourth inferior-data-4-tuples)))
          (flat-package-ids-systems-and-targets
           (map
            (match-lambda
              ((inferior-package-id system target derivation-file-name)
               (list (hashq-ref inferior-package-id->package-id-hash-table
                                inferior-package-id)
                     system
                     target)))
            inferior-data-4-tuples)))

      (insert-package-derivations conn
                                  flat-package-ids-systems-and-targets
                                  derivation-ids))))

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
          (begin
            (invalidate-derivation-caches!)
            (let* ((guix-package (@ (gnu packages package-management)
                                    guix))
                   (derivation (package-derivation store guix-package)))
              (build-derivations store (list derivation))

              (let ((new-store-path
                     (derivation->output-path derivation)))
                (set! store-path new-store-path)
                new-store-path)))))))

(define (nss-certs-store-path store)
  (let* ((nss-certs-package (@ (gnu packages certs)
                               nss-certs))
         (derivation (package-derivation store nss-certs-package)))
    (build-derivations store (list derivation))
    (derivation->output-path derivation)))

(define (channel->derivation-file-name store channel)
  (let ((inferior
         (open-inferior/container
          store
          (guix-store-path store)
          #:extra-shared-directories
          '("/gnu/store")
          #:extra-environment-variables
          (list (string-append
                 "SSL_CERT_DIR=" (nss-certs-store-path store))))))

    (catch
      #t
      (lambda ()
        ;; Create /etc/pass, as %known-shorthand-profiles in (guix
        ;; profiles) tries to read from this file. Because the environment
        ;; is cleaned in build-self.scm, xdg-directory in (guix utils)
        ;; falls back to accessing /etc/passwd.
        (inferior-eval
         '(begin
            (mkdir "/etc")
            (call-with-output-file "/etc/passwd"
              (lambda (port)
                (display "root:x:0:0::/root:/bin/bash" port))))
         inferior)

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
      (lambda args
        (simple-format (current-error-port)
                       "error: channel->derivation-file-name: ~A\n"
                       args)

        (close-inferior inferior)

        #f))))

(define (channel->manifest-store-item store channel)
  (let* ((manifest-store-item-derivation-file-name
          (channel->derivation-file-name store channel))
         (derivation
          (read-derivation-from-file manifest-store-item-derivation-file-name)))
    (build-derivations store (list derivation))
    (derivation->output-path derivation)))

(define (channel->guix-store-item store channel)
  (catch
    #t
    (lambda ()
      (dirname
       (readlink
        (string-append (channel->manifest-store-item store
                                                     channel)
                       "/bin"))))
    (lambda args
      (simple-format #t "guix-data-service: load-new-guix-revision: error: ~A\n" args)
      #f)))

(define (extract-information-from store conn url commit store-path)
  (simple-format
   #t "debug: extract-information-from: ~A\n" store-path)
  (let ((inf (open-inferior/container store store-path
                                      #:extra-shared-directories
                                      '("/gnu/store"))))
    (inferior-eval '(use-modules (srfi srfi-1)
                                 (srfi srfi-34)
                                 (guix grafts)
                                 (guix derivations))
                   inf)
    (inferior-eval '(%graft? #f) inf)

    (exec-query conn "BEGIN")
    (let ((package-derivation-ids
           (inferior-guix->package-derivation-ids store conn inf))
          (guix-revision-id
           (insert-guix-revision conn url commit store-path)))

      (insert-guix-revision-package-derivations conn
                                                guix-revision-id
                                                package-derivation-ids)

      (exec-query conn "COMMIT")

      (simple-format
       #t "Successfully loaded ~A package/derivation pairs\n"
       (length package-derivation-ids)))))

(define (load-new-guix-revision conn url commit)
  (if (guix-revision-exists? conn url commit)
      #t
      (with-store store
        (let ((store-item (channel->guix-store-item
                           store
                           (channel (name 'guix)
                                    (url url)
                                    (commit commit)))))
          (and store-item
               (extract-information-from store conn url commit store-item))))))

(define (enqueue-load-new-guix-revision-job conn url commit source)
  (define query
    "
INSERT INTO load_new_guix_revision_jobs (url, commit, source)
VALUES ($1, $2, $3)
RETURNING id;")

  (first
   (exec-query conn
               query
               (list url commit source))))

(define (select-job-for-commit conn commit)
  (let ((result
         (exec-query
          conn
          "SELECT * FROM load_new_guix_revision_jobs WHERE commit = $1"
          (list commit))))
    result))

(define (most-recent-n-load-new-guix-revision-jobs conn n)
  (let ((result
         (exec-query
          conn
          "SELECT * FROM load_new_guix_revision_jobs ORDER BY id ASC LIMIT $1"
          (list (number->string n)))))
    result))

(define (process-next-load-new-guix-revision-job conn)
  (let ((next
         (exec-query
          conn
          "SELECT * FROM load_new_guix_revision_jobs ORDER BY id ASC LIMIT 1")))
    (match next
      (((id url commit source))
       (begin
         (simple-format #t "Processing job ~A (url: ~A, commit: ~A, source: ~A)\n\n"
                        id url commit source)
         (load-new-guix-revision conn url commit)
         (exec-query
          conn
          (string-append "DELETE FROM load_new_guix_revision_jobs WHERE id = '"
                         id
                         "'"))))
      (_ #f))))
