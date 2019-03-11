(define-module (guix-data-service jobs load-new-guix-revision)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
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
            most-recent-n-load-new-guix-revision-jobs))

(define (inferior-guix->package-derivation-ids store conn inf)
  (define (inferior-package->systems-targets-and-derivations package)
    (let ((supported-systems
           (inferior-package-transitive-supported-systems package)))
      (append-map
       (lambda (system)
         (filter-map
          (lambda (target)
            (catch
              #t
              (lambda ()
                (list
                 system
                 target
                 (inferior-package-derivation store package system
                                              #:target
                                              (if (string=? system target)
                                                  #f
                                                  target))))
              (lambda args
                (cond
                 ((string-contains (simple-format #f "~A" (second args))
                                   "&package-cross-build-system-error")
                  #f)
                 ((string-contains (simple-format #f "~A" (fourth args))
                                   "(No cross-compilation for ")
                  #f)
                 (else
                  (simple-format
                   #t "guix-data-service: inferior-guix->package-ids: error processing derivation\n  ~A for system ~A and target ~A\n"
                   package system target)
                  (for-each (lambda (arg)
                              (simple-format #t "arg: ~A\n" arg))
                            args)
                  #f)))))
          supported-systems))
       supported-systems)))

  (let* ((packages (inferior-packages inf))
         (packages-metadata-ids
          (inferior-packages->package-metadata-ids conn packages))
         (packages-count (length packages))
         (progress-reporter (progress-reporter/bar
                             packages-count
                             (format #f "processing ~a packages"
                                     packages-count)))
         (systems-targets-and-derivations-by-package
          (call-with-progress-reporter progress-reporter
            (lambda (report)
              (map
               (lambda (package)
                 (report)
                 (inferior-package->systems-targets-and-derivations package))
               packages))))
         (package-ids
          (inferior-packages->package-ids
           conn packages packages-metadata-ids))
         (derivation-ids
          (derivations->derivation-ids
           conn
           (append-map
            (lambda (system-targets-and-derivations)
              (map third system-targets-and-derivations))
            systems-targets-and-derivations-by-package)))
         (flat-package-ids-systems-and-targets
          (append-map
           (lambda (package-id system-targets-and-derivations)
             (map (match-lambda
                    ((system target derivation)
                     (list package-id
                           system
                           target)))
                  system-targets-and-derivations))
           package-ids
           systems-targets-and-derivations-by-package)))

    (insert-package-derivations conn
                                flat-package-ids-systems-and-targets
                                derivation-ids)))

(define (inferior-package-transitive-supported-systems package)
  ((@@ (guix inferior) inferior-package-field)
   package
   'package-transitive-supported-systems))

(define (guix-store-path store)
  (let* ((guix-package (@ (gnu packages package-management)
                          guix))
         (derivation (package-derivation store guix-package)))
    (build-derivations store (list derivation))
    (derivation->output-path derivation)))

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
                  (return (derivation-file-name derv)))))))))))

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

(define (extract-information-from store conn url commit store_path)
  (let ((inf (open-inferior/container store store_path
                                      #:extra-shared-directories
                                      '("/gnu/store"))))
    (inferior-eval '(use-modules (guix grafts)) inf)
    (inferior-eval '(%graft? #f) inf)

    (exec-query conn "BEGIN")
    (let ((package-derivation-ids
           (inferior-guix->package-derivation-ids store conn inf))
          (guix-revision-id
           (insert-guix-revision conn url commit store_path)))

      (insert-guix-revision-package-derivations conn
                                                guix-revision-id
                                                package-derivation-ids)

      (exec-query conn "COMMIT")

      (simple-format
       #t "Successfully loaded ~A package/derivation pairs\n"
       (length package-derivation-ids)))

    (close-inferior inf)))

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
          "SELECT * FROM load_new_guix_revision_jobs LIMIT $1"
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

