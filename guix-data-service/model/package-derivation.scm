(define-module (guix-data-service model package-derivation)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix-data-service model utils)
  #:export (insert-package-derivations
            count-packages-derivations-in-revision))

(define (insert-missing-package-derivations conn entries)
  (define query
    (string-append
     "INSERT INTO package_derivations "
     "(package_id, derivation_id, system, target) VALUES "
     (string-join
      (map
       (lambda (entry)
         (apply simple-format
                #f "(~A, ~A, '~A', '~A')"
                entry))
       entries)
      ", ")
     " RETURNING id"))

  (exec-query conn query))

(define (insert-package-derivations conn
                                    package-ids-systems-and-targets
                                    derivation-ids)
  (define select-existing-package-derivation-entries
    (string-append
     "SELECT id, package_derivations.package_id,"
     " package_derivations.derivation_id, package_derivations.system,"
     " package_derivations.target "
     "FROM package_derivations "
     "JOIN (VALUES "
     (string-join (map (match-lambda*
                         (((package-id system target) derivation-id)
                          (simple-format
                           #f "(~A, ~A, '~A', '~A')"
                           package-id
                           derivation-id
                           system
                           target)))
                       package-ids-systems-and-targets
                       derivation-ids)
                  ", ")
     ") AS vals (package_id, derivation_id, system, target) "
     "ON package_derivations.package_id = vals.package_id "
     "AND package_derivations.derivation_id = vals.derivation_id "
     "AND package_derivations.system = vals.system "
     "AND package_derivations.target = vals.target"))

  (define data-4-tuples
    (map (match-lambda*
           (((package-id system target) derivation-id)
            (list package-id
                  derivation-id
                  system
                  target)))
         package-ids-systems-and-targets
         derivation-ids))

  (if (null? data-4-tuples)
      '()
      (begin
        (let* ((existing-entries
                (exec-query->vhash
                 conn
                 select-existing-package-derivation-entries
                 cdr
                 first)) ;; id

               (missing-entries
                (filter (lambda (4-tuple)
                          (not (vhash-assoc 4-tuple existing-entries)))
                        data-4-tuples))

               (new-entry-ids
                (if (null? missing-entries)
                    '()
                    (begin
                      (vlist->list existing-entries)
                      (insert-missing-package-derivations conn missing-entries))))

               (new-entries-id-lookup-vhash
                (two-lists->vhash missing-entries
                                  new-entry-ids)))
          (map (lambda (4-tuple)
                 (cdr
                  (or (vhash-assoc 4-tuple existing-entries)
                      (vhash-assoc 4-tuple new-entries-id-lookup-vhash)
                      (error "Missing entry"))))
               data-4-tuples)))))

(define (count-packages-derivations-in-revision conn commit-hash)
  (define query
    "
SELECT package_derivations.system, package_derivations.target,
COUNT(DISTINCT package_derivations.derivation_id)
FROM package_derivations
WHERE package_derivations.id IN (
 SELECT guix_revision_package_derivations.package_derivation_id
 FROM guix_revision_package_derivations
 INNER JOIN guix_revisions
   ON guix_revision_package_derivations.revision_id = guix_revisions.id
 WHERE guix_revisions.commit = $1
)
GROUP BY package_derivations.system, package_derivations.target
ORDER BY package_derivations.system DESC, package_derivations.target DESC")

  (exec-query conn query (list commit-hash)))
