(define-module (guix-data-service model derivation)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (squee)
  #:use-module (guix inferior)
  #:use-module (guix derivations)
  #:use-module (guix-data-service model utils)
  #:export (select-existing-derivations
            insert-into-derivations
            inferior-packages->derivation-ids))

(define (select-existing-derivations file-names)
  (string-append "SELECT id, file_name "
                 "FROM derivations "
                 "WHERE file_name IN "
                 "(" (string-join (map (lambda (file-name)
                                         (simple-format #f "'~A'" file-name))
                                       file-names)
                                  ",")
                 ");"))

(define (insert-into-derivations file-names)
  (string-append "INSERT INTO derivations (file_name) VALUES "
                 (string-join
                  (map
                   (lambda (file-name)
                     (simple-format #f "('~A')" file-name))
                   file-names)
                  ",")
                 " RETURNING id"
                 ";"))

(define (inferior-packages->derivation-ids store conn inferior-packages)
  (let* ((package-derivation-file-names (map (lambda (package)
                                               (derivation-file-name
                                                (inferior-package-derivation
                                                 store package)))
                                             inferior-packages))

         (existing-derivation-entries (exec-query->vhash
                                       conn
                                       (select-existing-derivations
                                        package-derivation-file-names)
                                       second ;; file_name
                                       first)) ;; id

         (missing-derivation-file-names
          (filter (lambda (file-name)
                    (not (vhash-assoc file-name
                                      existing-derivation-entries)))
                  package-derivation-file-names))
         (new-derivation-entries
          (if (null? missing-derivation-file-names)
              '()
              (map car
                   (exec-query
                    conn
                    (insert-into-derivations
                     missing-derivation-file-names)))))
         (new-entries-id-lookup-vhash
          (two-lists->vhash missing-derivation-file-names
                            new-derivation-entries)))
    (map (lambda (derivation-file-name)
           (cdr
            (or (vhash-assoc derivation-file-name
                             existing-derivation-entries)
                (vhash-assoc derivation-file-name
                             new-entries-id-lookup-vhash)
                (error "missing derivation id"))))
         package-derivation-file-names)))
