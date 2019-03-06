(define-module (guix-data-service model derivation)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix base32)
  #:use-module (guix inferior)
  #:use-module (guix derivations)
  #:use-module (guix-data-service model utils)
  #:export (select-existing-derivations
            select-derivations-by-id
            select-derivations-and-build-status-by-id
            insert-into-derivations
            derivations->derivation-ids))

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

(define (select-derivation-output-id conn name path)
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
             name path)))))

(define (insert-derivation-input conn derivation-id derivation-input)
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

  (match derivation-input
    (($ <derivation-input> path sub-derivations)
     (exec-query
      conn
      (insert-into-derivation-inputs
       (map (lambda (sub-derivation)
              (select-derivation-output-id conn
                                           sub-derivation
                                           path))
            sub-derivations))))))

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

(define (insert-missing-derivations conn derivations)
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

  (map (lambda (derivation-id derivation)
         (insert-derivation-outputs conn
                                    derivation-id
                                    (derivation-outputs derivation))

         (insert-derivation-sources conn
                                    derivation-id
                                    (derivation-sources derivation))

         (for-each (lambda (derivation-input)
                     (insert-derivation-input conn
                                              derivation-id
                                              derivation-input))
                   (derivation-inputs derivation))

         derivation-id)
       (map car (exec-query conn (insert-into-derivations)))
       derivations))

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

(define (select-derivations-and-build-status-by-id conn ids)
  (define query
    (string-append
     "SELECT derivations.id, derivations.file_name, latest_build_status.status "
     "FROM derivations "
     "LEFT OUTER JOIN builds ON derivations.id = builds.derivation_id "
     "LEFT OUTER JOIN "
     "(SELECT DISTINCT ON (internal_build_id) * "
     "FROM build_status "
     "ORDER BY internal_build_id, status_fetched_at DESC"
     ") AS latest_build_status "
     "ON builds.internal_id = latest_build_status.internal_build_id "
     "WHERE derivations.id IN "
     "(" (string-join (map (lambda (id)
                             (simple-format #f "'~A'" id))
                           ids)
                      ",")
     ");"))

  (exec-query conn query))

(define (derivations->derivation-ids conn derivations)
  (define (ensure-input-derivations-exist)
    (let* ((missing-derivation-file-names (map derivation-file-name
                                               derivations))

           (input-derivation-file-names (delete-duplicates
                                         (map derivation-input-path
                                              (append-map
                                               derivation-inputs
                                               derivations)))))

      ;; Ensure all the input derivations exist
      (derivations->derivation-ids
       conn
       (map read-derivation-from-file
            input-derivation-file-names))))

  (if (null? derivations)
      '()
      (begin
        (ensure-input-derivations-exist)
        (let* ((derivation-file-names (map derivation-file-name
                                           derivations))

               (existing-derivation-entries (exec-query->vhash
                                             conn
                                             (select-existing-derivations
                                              derivation-file-names)
                                             second ;; file_name
                                             first)) ;; id

               (missing-derivations
                (filter (lambda (derivation)
                          (not (vhash-assoc (derivation-file-name derivation)
                                            existing-derivation-entries)))
                        derivations))

               (new-derivation-entries
                (if (null? missing-derivations)
                    '()
                    (insert-missing-derivations conn missing-derivations)))

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
