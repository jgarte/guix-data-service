(define-module (guix-data-service model derivation)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix base32)
  #:use-module (guix inferior)
  #:use-module (guix memoization)
  #:use-module (guix derivations)
  #:use-module (guix-data-service model utils)
  #:export (valid-systems
            count-derivations
            select-derivation-by-file-name
            select-derivation-outputs-by-derivation-id
            select-derivation-by-output-filename
            select-derivations-using-output
            select-derivations-by-revision-name-and-version
            select-derivation-inputs-by-derivation-id
            select-existing-derivations
            select-derivations-by-id
            select-derivations-and-build-status
            insert-into-derivations
            derivation-file-names->derivation-ids))

(define (valid-systems conn)
  (map car
       (exec-query
        conn
        "SELECT DISTINCT system FROM derivations ORDER BY 1")))

(define (count-derivations conn)
  (first
   (exec-query
    conn
    "SELECT COUNT(*) FROM derivations")))

(define (select-existing-derivations file-names)
  (string-append "SELECT id, file_name "
                 "FROM derivations "
                 "WHERE file_name IN "
                 "(" (string-join (map (lambda (file-name)
                                         (simple-format #f "'~A'" file-name))
                                       file-names)
                                  ",")
                 ");"))

(define (select-from-derivation-output-details paths)
  (string-append
   "SELECT id, path FROM derivation_output_details "
   "WHERE path IN ("
   (string-join (map quote-string paths)
                ",")
   ")"))

(define (select-derivation-by-output-filename conn filename)
  (define query
    (string-append
     "SELECT derivations.file_name, derivation_outputs.id "
     "FROM derivation_output_details "
     "INNER JOIN derivation_outputs"
     " ON derivation_output_details.id = derivation_outputs.derivation_output_details_id "
     "INNER JOIN derivations"
     " ON derivation_outputs.derivation_id = derivations.id "
     "WHERE derivation_output_details.path = $1"))

  (exec-query conn query (list filename)))

(define (select-derivations-using-output conn output-id)
  (define query
    (string-append
     "SELECT derivations.file_name "
     "FROM derivations "
     "INNER JOIN derivation_inputs"
     " ON derivation_inputs.derivation_id = derivations.id "
     "WHERE derivation_output_id = $1 "
     "ORDER BY derivations.file_name "
     "LIMIT 100 "))

  (exec-query conn query (list output-id)))

(define (select-derivations-by-revision-name-and-version
         conn revision-commit-hash name version)
  (define query "
SELECT derivations.system, package_derivations.target, derivations.file_name,
  latest_build_status.status
FROM derivations
INNER JOIN package_derivations
  ON derivations.id = package_derivations.derivation_id
INNER JOIN packages
  ON package_derivations.package_id = packages.id
INNER JOIN guix_revision_package_derivations
  ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
INNER JOIN guix_revisions
  ON guix_revision_package_derivations.revision_id = guix_revisions.id
LEFT OUTER JOIN builds ON derivations.id = builds.derivation_id
LEFT OUTER JOIN (
  SELECT DISTINCT ON (internal_build_id) *
  FROM build_status
  ORDER BY internal_build_id, status_fetched_at DESC
) AS latest_build_status
  ON builds.internal_id = latest_build_status.internal_build_id
WHERE guix_revisions.commit = $1
  AND packages.name = $2
  AND packages.version = $3
ORDER BY derivations.system DESC,
         package_derivations.target DESC,
         derivations.file_name")

  (exec-query conn query (list revision-commit-hash name version)))

(define (insert-derivation-outputs conn
                                   derivation-id
                                   names-and-derivation-outputs)
  (define (insert-into-derivation-output-details derivation-outputs)
    (string-append
     "INSERT INTO derivation_output_details "
     "(path, hash_algorithm, hash, recursive) VALUES "
     (string-join
      (map
       (match-lambda
         (($ <derivation-output> path hash-algo hash recursive?)
          (string-append
           "("
           (string-join
            (list (quote-string path)
                  (value->quoted-string-or-null
                   (and=> hash-algo symbol->string))
                  (value->quoted-string-or-null
                   (and=> hash bytevector->nix-base32-string))
                  (if recursive? "TRUE" "FALSE"))
            ",")
           ")")))
       derivation-outputs)
      ",")
     " RETURNING id"
     ";"))

  (define (insert-into-derivation-outputs output-names
                                          derivation-output-ids)
    (string-append "INSERT INTO derivation_outputs "
                   "(derivation_id, name, derivation_output_details_id) VALUES "
                   (string-join
                    (map (lambda (output-name derivation-output-id)
                           (simple-format
                            #f "(~A, '~A', ~A)"
                            derivation-id output-name derivation-output-id))
                         output-names
                         derivation-output-ids)
                    ",")
                   ";"))

  (let* ((derivation-outputs (map cdr names-and-derivation-outputs))
         (derivation-output-paths (map derivation-output-path
                                       derivation-outputs))

         (existing-derivation-output-details-entries
          (exec-query->vhash
           conn
           (select-from-derivation-output-details
            derivation-output-paths)
           second ;; path
           first)) ;; id

         (missing-entries (filter
                           (lambda (derivation-output)
                             (not (vhash-assoc
                                   (derivation-output-path derivation-output)
                                   existing-derivation-output-details-entries)))
                           derivation-outputs))

         (new-derivation-output-details-ids
          (if (null? missing-entries)
              '()
              (map car
                   (exec-query
                    conn
                    (insert-into-derivation-output-details missing-entries)))))

         (new-entries-id-lookup-vhash
          (two-lists->vhash (map derivation-output-path missing-entries)
                            new-derivation-output-details-ids))

         (derivation-output-ids
          (map (lambda (path)
                 (cdr
                  (or (vhash-assoc path
                                   existing-derivation-output-details-entries)
                      (vhash-assoc path
                                   new-entries-id-lookup-vhash)
                      (error "missing derivation output details entry"))))
               derivation-output-paths))

         (derivation-output-names
          (map car names-and-derivation-outputs)))

    (exec-query conn
                (insert-into-derivation-outputs derivation-output-names
                                                derivation-output-ids))

    derivation-output-ids))

(define (select-derivation-by-file-name conn file-name)
  (define query
    (string-append
     "SELECT id, file_name, builder, args, env_vars, system "
     "FROM derivations "
     "WHERE file_name = $1"))

  (match (exec-query conn query (list file-name))
    (()
     #f)
    ((result)
     result)))

(define select-derivation-output-id
  (mlambda (conn name path)
    (match (exec-query
            conn
            (string-append
             "SELECT derivation_outputs.id FROM derivation_outputs "
             "INNER JOIN derivations ON "
             "derivation_outputs.derivation_id = derivations.id "
             "WHERE derivations.file_name = '" path "' "
             "AND derivation_outputs.name = '" name "';"))
      (((id))
       id)
      (()
       (error (simple-format
               #f "cannot find derivation-output with name ~A and path ~A"
               name path))))))

(define (select-derivation-outputs-by-derivation-id conn id)
  (define query
    (string-append
     "SELECT derivation_outputs.name, derivation_output_details.path, "
     "derivation_output_details.hash_algorithm, derivation_output_details.hash, "
     "derivation_output_details.recursive "
     "FROM derivation_outputs "
     "INNER JOIN derivation_output_details ON "
     "derivation_outputs.derivation_output_details_id = derivation_output_details.id "
     "WHERE derivation_id = $1"))

  (exec-query conn query (list id)))

(define (select-derivation-inputs-by-derivation-id conn id)
  (define query
    (string-append
     "SELECT derivations.file_name, derivation_outputs.name, "
     "derivation_output_details.path "
     "FROM derivation_inputs "
     "INNER JOIN derivation_outputs"
     " ON derivation_outputs.id = derivation_inputs.derivation_output_id "
     "INNER JOIN derivation_output_details"
     " ON derivation_outputs.derivation_output_details_id = derivation_output_details.id "
     "INNER JOIN derivations"
     " ON derivation_outputs.derivation_id = derivations.id "
     "WHERE derivation_inputs.derivation_id = $1"))

  (exec-query conn query (list id)))

(define (insert-derivation-inputs conn derivation-id derivation-inputs)
  (define (insert-into-derivation-inputs output-ids)
    (string-append "INSERT INTO derivation_inputs "
                   "(derivation_id, derivation_output_id) VALUES "
                   (string-join
                    (map (lambda (output-id)
                           (simple-format
                            #f "(~A, ~A)"
                            derivation-id output-id))
                         output-ids)
                    ",")
                   ";"))

  (unless (null? derivation-inputs)
    (exec-query
     conn
     (insert-into-derivation-inputs
      (append-map
       (match-lambda
         (($ <derivation-input> path sub-derivations)
          (map (lambda (sub-derivation)
                 (select-derivation-output-id conn
                                              sub-derivation
                                              path))
               sub-derivations)))
       derivation-inputs)))))

(define (select-from-derivation-source-files store-paths)
  (string-append
   "SELECT id, store_path FROM derivation_source_files "
   "WHERE store_path IN ("
   (string-join (map quote-string store-paths)
                ",")
   ");"))

(define (insert-derivation-sources conn derivation-id sources)
  (define (insert-into-derivation-source-files store-paths)
    (string-append
     "INSERT INTO derivation_source_files (store_path) VALUES "
     (string-join
      (map (lambda (store-path)
             (simple-format
              #f "('~A')" store-path))
           store-paths)
      ",")
     " RETURNING id"
     ";"))

  (define (insert-into-derivation-sources derivation-source-file-ids)
    (string-append
     "INSERT INTO derivation_sources "
     "(derivation_id, derivation_source_file_id) VALUES "
     (string-join
      (map (lambda (derivation-source-file-id)
             (simple-format
              #f "(~A, ~A)" derivation-id derivation-source-file-id))
           derivation-source-file-ids)
      ",")
     ";"))

  (let* ((existing-derivation-store-paths
          (exec-query->vhash
           conn
           (select-from-derivation-source-files sources)
           second ;; store_path
           first)) ;; id

         (missing-entries (filter
                           (lambda (store-path)
                             (not (vhash-assoc store-path
                                               existing-derivation-store-paths)))
                           sources))

         (new-derivation-source-file-entries
          (if (null? missing-entries)
              '()
              (exec-query conn
                          (insert-into-derivation-source-files missing-entries))))

         (new-entries-id-lookup-vhash
          (two-lists->vhash missing-entries
                            new-derivation-source-file-entries))

         (sources-ids
          (map (lambda (store-path)
                 (cdr
                  (or (vhash-assoc store-path
                                   existing-derivation-store-paths)
                      (vhash-assoc store-path
                                   new-entries-id-lookup-vhash)
                      (error "missing derivation source files entry"))))
               sources)))

    (exec-query conn
                (insert-into-derivation-sources sources-ids))))

(define (insert-missing-derivations conn
                                    derivation-ids-hash-table
                                    derivations)
  (define (ensure-input-derivations-exist input-derivation-file-names)
    (unless (null? input-derivation-file-names)
      (simple-format
       #t "debug: ensure-input-derivations-exist: processing ~A derivations\n"
       (length input-derivation-file-names))
      (force-output)
      (let* ((existing-derivation-entries
              (derivation-file-names->vhash conn
                                            derivation-ids-hash-table
                                            input-derivation-file-names))

             (missing-derivations-filenames
              (filter (lambda (derivation-file-name)
                        (not (vhash-assoc derivation-file-name
                                          existing-derivation-entries)))
                      input-derivation-file-names)))

        (unless (null? missing-derivations-filenames)
          ;; Ensure all the input derivations exist
          (insert-missing-derivations
           conn
           derivation-ids-hash-table
           (map read-derivation-from-file
                missing-derivations-filenames))))))

  (define (insert-into-derivations)
    (string-append
     "INSERT INTO derivations "
     "(file_name, builder, args, env_vars, system) VALUES "
     (string-join
      (map (match-lambda
             (($ <derivation> outputs inputs sources
                              system builder args env-vars file-name)
              (simple-format
               #f "('~A', '~A', ARRAY[~A]::varchar[], ARRAY[~A], '~A')"
               file-name
               builder
               (string-join (map quote-string args) ",")
               (string-join (map (match-lambda
                                   ((key . value)
                                    (string-append
                                     "['" key '"', $$"
                                     value "$$ ]")))
                                 env-vars)
                            ",")
               system)))
           derivations)
      ",")
     " RETURNING id"
     ";"))

  (simple-format
   #t "debug: insert-missing-derivations: inserting ~A derivations\n"
   (length derivations))
  (let ((derivation-ids
         (map car (exec-query conn (insert-into-derivations)))))

    (simple-format
     #t "debug: insert-missing-derivations: updating hash table\n")
    (for-each (lambda (derivation derivation-id)
                (hash-set! derivation-ids-hash-table
                           (derivation-file-name derivation)
                           derivation-id))
              derivations
              derivation-ids)

    (simple-format
     #t "debug: insert-missing-derivations: inserting outputs\n")
    (for-each (lambda (derivation-id derivation)
                (insert-derivation-outputs conn
                                           derivation-id
                                           (derivation-outputs derivation)))
              derivation-ids
              derivations)

    (simple-format
     #t "debug: insert-missing-derivations: inserting sources\n")
    (for-each (lambda (derivation-id derivation)
                (insert-derivation-sources conn
                                           derivation-id
                                           (derivation-sources derivation)))
              derivation-ids
              derivations)

    (simple-format
     #t "debug: insert-missing-derivations: ensure-input-derivations-exist\n")
    (force-output)

    (ensure-input-derivations-exist (deduplicate-strings
                                     (map derivation-input-path
                                          (append-map
                                           derivation-inputs
                                           derivations))))

    (simple-format
     #t "debug: insert-missing-derivations: inserting inputs\n")
    (for-each (lambda (derivation-id derivation)
                (insert-derivation-inputs conn
                                          derivation-id
                                          (derivation-inputs derivation)))

              derivation-ids
              derivations)

    derivation-ids))

(define (select-derivations-by-id conn ids)
  (define query
    (string-append "SELECT id, file_name "
                   "FROM derivations "
                   "WHERE id IN "
                   "(" (string-join (map (lambda (id)
                                           (simple-format #f "'~A'" id))
                                         ids)
                                    ",")
                   ");"))

  (exec-query conn query))

(define* (select-derivations-and-build-status conn #:key
                                              file-names
                                              systems
                                              targets
                                              build-statuses)
  (define criteria
    (string-join
     (filter-map
      (lambda (field values)
        (if values
            (string-append
             field " IN ("
             (string-join (map (lambda (value)
                                 (simple-format #f "'~A'" value))
                               values)
                          ",")
             ")")
            #f))
      '("derivations.file_name"
        "derivations.system"
        "target"
        "latest_build_status.status")
      (list (deduplicate-strings file-names)
            systems
            targets
            build-statuses))
     " AND "))

  (define query
    (string-append
     "
SELECT
  derivations.file_name,
  derivations.system,
  package_derivations.target,
  latest_build_status.status
FROM derivations
INNER JOIN package_derivations
  ON derivations.id = package_derivations.derivation_id
LEFT OUTER JOIN builds
  ON derivations.id = builds.derivation_id
LEFT OUTER JOIN (
  SELECT DISTINCT ON (internal_build_id) *
  FROM build_status
  ORDER BY internal_build_id, status_fetched_at DESC
) AS latest_build_status
ON builds.internal_id = latest_build_status.internal_build_id
WHERE " criteria ";"))

  (exec-query conn query))

(define (deduplicate-derivations derivations)
  (define sorted-derivations
    (sort derivations
          (lambda (a b)
            (string<? (derivation-file-name a)
                      (derivation-file-name b)))))

  (pair-fold
   (match-lambda*
     (((x) result)
      (cons x result))
     (((x y rest ...) result)
      (if (string=? (derivation-file-name x)
                    (derivation-file-name y))
          result
          (cons x result))))
   '()
   sorted-derivations))

(define (derivation-file-names->vhash conn derivation-ids-hash-table file-names)
  (simple-format #t "debug: derivation-file-names->vhash: ~A file-names\n"
                 (length file-names))
  (match (fold (match-lambda*
                 ((file-name (result . missing-file-names))
                  (let ((cached-id (hash-ref derivation-ids-hash-table
                                             file-name)))
                    (if cached-id
                        (cons (vhash-cons file-name cached-id result)
                              missing-file-names)
                        (cons result
                              (cons file-name missing-file-names))))))
               (cons vlist-null '())
               file-names)
    ((result)
     (simple-format
      #t "debug: derivation-file-names->vhash: lookup ~A file-names, all found\n"
      (length file-names))
     result)
    ((result . missing-file-names)
     (simple-format
      #t "debug: derivation-file-names->vhash: lookup ~A file-names, ~A not cached\n"
      (length file-names) (length missing-file-names))
     (let ((result-for-missing-file-names
            (exec-query->vhash
             conn
             (select-existing-derivations missing-file-names)
             second ;; file_name
             first))) ;; id
       (simple-format
        #t "debug: derivation-file-names->vhash: adding ~A entries to the cache\n"
        (vlist-length result-for-missing-file-names))
       (vhash-fold (lambda (key value _)
                     (hash-set! derivation-ids-hash-table key value))
                   '()
                   result-for-missing-file-names)

       (vhash-fold
        (lambda (key value combined)
          (vhash-cons key value combined))
        result
        result-for-missing-file-names)))))

(define (derivation-file-names->derivation-ids conn derivation-file-names)
  (if (null? derivation-file-names)
      '()
      (let* ((derivations-count (length derivation-file-names))
             (derivation-ids-hash-table (make-hash-table derivations-count)))
        (simple-format
         #t "debug: derivation-file-names->derivation-ids: processing ~A derivations\n"
         derivations-count)
        (let* ((existing-derivation-entries
                (derivation-file-names->vhash conn
                                              derivation-ids-hash-table
                                              derivation-file-names))

               (missing-derivations
                (map read-derivation-from-file
                     (deduplicate-strings
                      (filter (lambda (derivation-file-name)
                                (not (vhash-assoc derivation-file-name
                                                  existing-derivation-entries)))
                              derivation-file-names))))

               (new-derivation-entries
                (if (null? missing-derivations)
                    '()
                    (insert-missing-derivations conn
                                                derivation-ids-hash-table
                                                missing-derivations)))

               (new-entries-id-lookup-vhash
                (two-lists->vhash (map derivation-file-name missing-derivations)
                                  new-derivation-entries)))

          (map (lambda (derivation-file-name)
                 (cdr
                  (or (vhash-assoc derivation-file-name
                                   existing-derivation-entries)
                      (vhash-assoc derivation-file-name
                                   new-entries-id-lookup-vhash)
                      (error "missing derivation id"))))
               derivation-file-names)))))
