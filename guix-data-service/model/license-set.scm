(define-module (guix-data-service model license-set)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (squee)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service model license)
  #:export (inferior-packages->license-set-ids))

(define select-license-sets
  "
SELECT id, license_ids
FROM license_sets")

(define (insert-license-sets license-id-lists)
  (string-append
   "INSERT INTO license_sets (license_ids) VALUES "
   (string-join
    (map (lambda (license-ids)
           (string-append
            "('{"
            (string-join
             (map number->string
                  (sort license-ids <))
             ", ")
            "}')"))
         license-id-lists)
    ", ")
   " RETURNING id"))

(define (inferior-packages->license-set-ids conn inf packages)
  (define license-id-lists
    (inferior-packages->license-id-lists conn inf packages))

  (let* ((unique-license-id-lists (delete-duplicates
                                   license-id-lists))
         (existing-license-sets
          (exec-query->vhash conn
                             select-license-sets
                             (lambda (results)
                               (if (string=? (second results) "{}")
                                   '()
                                   (map
                                    string->number
                                    (string-split
                                     (string-drop-right
                                      (string-drop (second results) 1)
                                      1)
                                     #\,))))
                             (lambda (result)
                               (string->number (first result))))) ;; id
         (missing-license-sets
          (delete-duplicates
           (filter (lambda (license-set-license-ids)
                     (not (vhash-assoc license-set-license-ids
                                       existing-license-sets)))
                   unique-license-id-lists)))
         (new-license-set-entries
          (if (null? missing-license-sets)
              '()
              (map (lambda (result)
                     (string->number (first result)))
                   (exec-query conn
                               (insert-license-sets missing-license-sets)))))
         (new-entries-id-lookup-vhash
          (two-lists->vhash missing-license-sets
                            new-license-set-entries)))

    (map (lambda (license-id-list)
           (cdr
            (or (vhash-assoc license-id-list
                             existing-license-sets)
                (vhash-assoc license-id-list
                             new-entries-id-lookup-vhash)
                (begin
                  (error "missing license set entry"
                         license-id-list)))))
         license-id-lists)))
