(define-module (guix-data-service model guix-revision-package)
  #:use-module (squee)
  #:export (insert-guix-revision-packages))

(define (insert-guix-revision-packages conn guix-revision-id package-ids)
  (define insert
    (string-append "INSERT INTO guix_revision_packages "
                   "(revision_id, package_id) "
                   "VALUES "
                   (string-join (map (lambda (package-id)
                                       (simple-format
                                        #f "(~A, ~A)"
                                        guix-revision-id
                                        package-id))
                                     package-ids)
                                ", ")
                   ";"))

  (exec-query conn insert))
