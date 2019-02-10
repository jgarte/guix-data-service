(define-module (guix-data-service jobs load-new-guix-revision)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix derivations)
  #:use-module (guix-data-service model package)
  #:use-module (guix-data-service model guix-revision)
  #:use-module (guix-data-service model guix-revision-package)
  #:use-module (guix-data-service model package-metadata)
  #:use-module (guix-data-service model derivation)
  #:export (process-next-load-new-guix-revision-job))

(define (inferior-guix->package-ids store conn inf)
  (let* ((packages (inferior-packages inf))
         (packages-metadata-ids
          (inferior-packages->package-metadata-ids conn packages))
         (packages-derivation-ids
          (derivations->derivation-ids conn
                                       (map (lambda (package)
                                              (inferior-package-derivation
                                               store package))
                                            packages))))

    (inferior-packages->package-ids
     conn packages packages-metadata-ids packages-derivation-ids)))

(define (channel->manifest-store-item store channel)
  (define (build-and-get-output-path store profile-derv)
    (run-with-store store
      (mbegin %store-monad
        (built-derivations (list profile-derv))
        (return (derivation->output-path profile-derv)))))

  (let ((instances (latest-channel-instances store (list channel))))
    (run-with-store store
      (mlet* %store-monad ((manifest (channel-instances->manifest instances))
                           (derv (profile-derivation manifest)))
        ((store-lift build-and-get-output-path) derv)))))

(define (channel->guix-store-item store channel)
  (dirname
   (readlink
    (string-append (channel->manifest-store-item store channel)
                   "/bin"))))

(define (extract-information-from store conn url commit store_path)
  (let ((inf (open-inferior store_path)))
    (inferior-eval '(use-modules (guix grafts)) inf)
    (inferior-eval '(%graft? #f) inf)

    (let ((package-ids (inferior-guix->package-ids store conn inf)))
      (exec-query conn "BEGIN")

      (let ((guix-revision-id
             (insert-guix-revision conn url commit store_path)))
        (insert-guix-revision-packages conn guix-revision-id package-ids)))

    (exec-query conn "COMMIT")

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
          (extract-information-from store conn url commit store-item)))))

(define (process-next-load-new-guix-revision-job conn)
  (let ((next
         (exec-query
          conn
          "SELECT * FROM load_new_guix_revision_jobs ORDER BY id ASC LIMIT 1")))
    (match next
      (((id url commit))
       (begin
         (simple-format #t "Processing job ~A (url: ~A, commit: ~A)\n\n"
                        id url commit)
         (load-new-guix-revision conn url commit)
         (exec-query
          conn
          (string-append "DELETE FROM load_new_guix_revision_jobs WHERE id = '"
                         id
                         "'"))))
      (_ #f))))

