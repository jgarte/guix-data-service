(define-module (guix-data-service model guix-revision-package-derivation)
  #:use-module (squee)
  #:export (insert-guix-revision-package-derivations))

(define (insert-guix-revision-package-derivations
         conn guix-revision-id package-derivation-ids)
  (define insert
    (string-append "INSERT INTO guix_revision_package_derivations "
                   "(revision_id, package_derivation_id) "
                   "VALUES "
                   (string-join (map (lambda (package-derivation-id)
                                       (simple-format
                                        #f "(~A, ~A)"
                                        guix-revision-id
                                        package-derivation-id))
                                     package-derivation-ids)
                                ", ")
                   ";"))

  (exec-query conn insert))
