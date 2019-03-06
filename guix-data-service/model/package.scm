(define-module (guix-data-service model package)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix inferior)
  #:use-module (guix-data-service model utils)
  #:export (select-existing-package-entries
            select-packages-in-revision
            insert-into-package-entries
            inferior-packages->package-ids))

(define (select-existing-package-entries package-entries)
  (string-append "SELECT id, packages.name, packages.version, "
                 "packages.package_metadata_id, packages.derivation_id "
                 "FROM packages "
                 "JOIN (VALUES "
                 (string-join (map (lambda (package-entry)
                                     (apply
                                      simple-format
                                      #f "('~A', '~A', ~A, ~A)"
                                      package-entry))
                                   package-entries)
                              ", ")
                 ") AS vals (name, version, package_metadata_id, derivation_id) "
                 "ON packages.name = vals.name AND "
                 "packages.version = vals.version AND "
                 "packages.package_metadata_id = vals.package_metadata_id AND "
                 "packages.derivation_id = vals.derivation_id"
                 ";"))

(define (select-packages-in-revision conn commit-hash)
  (define query
    (string-append
     "SELECT packages.name, packages.version, packages.derivation_id "
     "FROM packages "
     "INNER JOIN guix_revision_packages"
     " ON packages.id = guix_revision_packages.package_id "
     "INNER JOIN guix_revisions"
     " ON guix_revision_packages.revision_id = guix_revisions.id "
     "WHERE guix_revisions.commit = $1 "
     "ORDER BY packages.name, packages.version"))

  (exec-query conn query (list commit-hash)))

(define (insert-into-package-entries package-entries)
  (string-append "INSERT INTO packages "
                 "(name, version, package_metadata_id, derivation_id) VALUES "
                 (string-join
                  (map
                   (match-lambda
                     ((name version package_metadata_id derivation_id)
                      (simple-format #f "('~A', '~A', ~A, ~A)"
                                     name
                                     version
                                     package_metadata_id
                                     derivation_id)))
                   package-entries)
                  ",")
                 " RETURNING id"
                 ";"))

(define (inferior-packages->package-ids conn packages metadata-ids derivation-ids)
  (define package-entries
    (map (lambda (package metadata-id derivation-id)
           (list (inferior-package-name package)
                 (inferior-package-version package)
                 metadata-id
                 derivation-id))
         packages
         metadata-ids
         derivation-ids))

  (let* ((existing-package-entry-ids
          (exec-query->vhash conn
                             (select-existing-package-entries package-entries)
                             ;; name, version, package_metadata_id and
                             ;; derivation_id
                             cdr
                             first)) ;;id
         (missing-package-entries
          (filter (lambda (package-entry)
                    (not (vhash-assoc package-entry
                                      existing-package-entry-ids)))
                  package-entries))
         (new-package-entry-ids
          (if (null? missing-package-entries)
              '()
              (map car
                   (exec-query
                    conn
                    (insert-into-package-entries
                     missing-package-entries)))))
         (new-entries-id-lookup-vhash
          (two-lists->vhash missing-package-entries
                            new-package-entry-ids)))

    (map (lambda (package-entry)
           (cdr
            (or (vhash-assoc package-entry
                             existing-package-entry-ids)
                (vhash-assoc package-entry
                             new-entries-id-lookup-vhash)
                (error "missing package entry"))))
         package-entries)))
