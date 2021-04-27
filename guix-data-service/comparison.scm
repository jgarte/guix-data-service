;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (guix-data-service comparison)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (json)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model utils)
  #:use-module (guix-data-service model derivation)
  #:export (derivation-differences-data

            package-differences-data
            package-data->package-data-vhashes

            package-data-vhashes->new-packages
            package-data-vhashes->removed-packages
            package-data-version-changes

            package-derivation-differences-data
            package-derivation-data->package-derivation-data-vhashes

            package-derivation-data->names-and-versions
            package-derivation-data-vhash->derivations
            package-derivation-data-vhash->derivations-and-build-status
            package-derivation-data-changes

            lint-warning-differences-data

            system-test-derivations-differences-data

            channel-news-differences-data))

(define (derivation-differences-data conn
                                     base-derivation-file-name
                                     target-derivation-file-name)
  (define base-derivation
    (select-derivation-by-file-name conn base-derivation-file-name))

  (define target-derivation
    (select-derivation-by-file-name conn target-derivation-file-name))

  (define group-by-last-element
    (lambda (vals)
      (let ((groups (last vals)))
        (cons (if (eq? (length groups) 2)
                  'common
                  (first groups))
              (drop-right vals 1)))))

  (define (fetch-value alist key)
    (assq-ref (find (lambda (env-var)
                      (if (string=? key (assq-ref env-var 'key))
                          (assq-ref env-var 'value)
                          #f))
                    alist)
              'value))

  `((outputs
     . ,(group-to-alist/vector
         group-by-last-element
         (derivation-outputs-differences-data conn
                                              (first base-derivation)
                                              (first target-derivation))))
    (inputs
     . ,(group-to-alist/vector
         group-by-last-element
         (derivation-inputs-differences-data conn
                                             (first base-derivation)
                                             (first target-derivation))))

    (sources
     . ,(group-to-alist/vector
         group-by-last-element
         (derivation-sources-differences-data conn
                                              (first base-derivation)
                                              (first target-derivation))))
    ,@(match base-derivation
        ((_ _ base-builder base-args base-env-vars base-system)
         (match target-derivation
           ((_ _ target-builder target-args target-env-vars target-system)
            `((system
               . ,(if (string=? base-system target-system)
                      `((common . ,base-system))
                      `((base . ,base-system)
                        (target . ,target-system))))
              (builder
               . ,(if (string=? base-builder target-builder)
                      `((common . ,base-builder))
                      `((base . ,base-builder)
                        (target . ,target-builder))))
              (arguments
               . ,(if (eq? base-args target-args)
                      `((common . ,(list->vector base-args)))
                      `((base . ,(list->vector base-args))
                        (target . ,(list->vector target-args)))))
              (environment-variables
               . ,(map (lambda (key)
                         (let ((base-value (fetch-value base-env-vars key))
                               (target-value (fetch-value target-env-vars key)))
                           (if (and base-value target-value)
                               `(,key
                                 . ,(if (string=? base-value target-value)
                                        `((common . ,base-value))
                                        `((base . ,base-value)
                                          (target . ,target-value))))
                               (if base-value
                                   `(,key . ((base . ,base-value)))
                                   `(,key . ((target . ,target-value)))))))
                       (delete-duplicates
                        (map (lambda (env-var)
                               (assq-ref env-var 'key))
                             (append base-env-vars
                                     target-env-vars))
                        string=?))))))))))

(define (derivation-outputs-differences-data conn
                                             base-derivation-id
                                             target-derivation-id)
  (define query
    (string-append
     "
SELECT derivation_outputs.name,
       derivation_output_details.path,
       derivation_output_details.hash_algorithm,
       derivation_output_details.hash,
       derivation_output_details.recursive,
       ARRAY_AGG(derivation_outputs.derivation_id) AS derivation_ids
FROM derivation_outputs
INNER JOIN derivation_output_details
  ON derivation_output_details_id = derivation_output_details.id
WHERE derivation_outputs.derivation_id IN ("
     (simple-format #f "~A,~A"
                    base-derivation-id
                    target-derivation-id) "
)
GROUP BY 1, 2, 3, 4, 5"))

  (map (match-lambda
         ((output-name path hash-algorithm hash recursive
                       derivation_ids)
          (let ((parsed-derivation-ids
                 (map string->number
                      (parse-postgresql-array-string derivation_ids))))
            `((output-name . ,output-name)
              (path . ,path)
              ,@(if (string? hash-algorithm)
                    `((hash-algorithm . ,hash-algorithm))
                    `((hash-algorithm . null)))
              ,@(if (string? hash)
                    `((hash . ,hash))
                    `((hash . null)))
              (recursive . ,(string=? recursive "t"))
              ,(append (if (memq base-derivation-id
                                 parsed-derivation-ids)
                           '(base)
                           '())
                       (if (memq target-derivation-id
                                 parsed-derivation-ids)
                           '(target)
                           '()))))))
       (exec-query conn query)))

(define (derivation-inputs-differences-data conn
                                            base-derivation-id
                                            target-derivation-id)
  (define query
    (string-append
     "
SELECT derivations.file_name,
       derivation_outputs.name,
       relevant_derivation_inputs.derivation_ids
FROM derivation_outputs
INNER JOIN (
  SELECT derivation_output_id,
         ARRAY_AGG(derivation_id) AS derivation_ids
  FROM derivation_inputs
  WHERE derivation_id IN (" (simple-format #f "~A,~A"
                                           base-derivation-id
                                           target-derivation-id)
  ") GROUP BY derivation_output_id
) AS relevant_derivation_inputs
  ON derivation_outputs.id = relevant_derivation_inputs.derivation_output_id
INNER JOIN derivations ON derivation_outputs.derivation_id = derivations.id
"))

  (map (match-lambda
         ((derivation_file_name derivation_output_name
                                derivation_ids)
          (let ((parsed-derivation-ids
                 (map string->number
                      (parse-postgresql-array-string derivation_ids))))
            `((derivation_file_name . ,derivation_file_name)
              (derivation_output_name . ,derivation_output_name)
              ,(append (if (memq base-derivation-id
                                 parsed-derivation-ids)
                           '(base)
                           '())
                       (if (memq target-derivation-id
                                 parsed-derivation-ids)
                           '(target)
                           '()))))))
       (exec-query conn query)))

(define (derivation-sources-differences-data conn
                                             base-derivation-id
                                             target-derivation-id)
  (define query
    (string-append
     "
SELECT derivation_source_files.store_path, ARRAY_AGG(derivation_sources.derivation_id)
FROM derivation_source_files
INNER JOIN derivation_sources
  ON derivation_source_files.id = derivation_sources.derivation_source_file_id
WHERE derivation_sources.derivation_id IN (" (simple-format #f "~A,~A"
                                                            base-derivation-id
                                                            target-derivation-id)
")
GROUP BY derivation_source_files.store_path"))

  (map (match-lambda
         ((store_path derivation_ids)
          (let ((parsed-derivation-ids
                 (map string->number
                      (parse-postgresql-array-string derivation_ids))))
            `((store_path . ,store_path)
              ,(append (if (memq base-derivation-id
                                 parsed-derivation-ids)
                           '(base)
                           '())
                       (if (memq target-derivation-id
                                 parsed-derivation-ids)
                           '(target)
                           '()))))))
       (exec-query conn query)))

(define* (package-derivation-differences-data conn
                                              base_guix_revision_id
                                              target_guix_revision_id
                                              #:key
                                              (systems #f)
                                              (targets #f)
                                              (include-builds? #t)
                                              (exclude-unchanged-outputs? #t)
                                              ;; Build changes are (symbols):
                                              ;; broken, fixed, still-failing,
                                              ;; still-working, unknown
                                              (build-change 'unknown)
                                              limit-results
                                              after-name)
  (define extra-constraints
    (string-append
     (if systems
         (string-append
          " AND systems.system IN ("
          (string-join (map
                        (lambda (s)
                          (string-append "'" s "'"))
                        systems)
                       ", ")
          ")")
         "")
     (if targets
         (string-append
          " AND package_derivations.target IN ("
          (string-join (map
                        (lambda (s)
                          (string-append "'" s "'"))
                        targets)
                       ", ")
          ")")
         "")))

  (define query
    (string-append "
WITH base_packages AS (
  SELECT packages.*, derivations.id AS derivation_id, derivations.file_name,
    systems.system, package_derivations.target,
    derivations_by_output_details_set.derivation_output_details_set_id
  FROM packages
  INNER JOIN package_derivations
    ON packages.id = package_derivations.package_id
  INNER JOIN systems
    ON package_derivations.system_id = systems.id
  INNER JOIN derivations
    ON package_derivations.derivation_id = derivations.id
  INNER JOIN derivations_by_output_details_set
    ON derivations.id = derivations_by_output_details_set.derivation_id
  WHERE package_derivations.id IN (
    SELECT guix_revision_package_derivations.package_derivation_id
    FROM guix_revision_package_derivations
    WHERE revision_id = $1
  )" extra-constraints "
), target_packages AS (
  SELECT packages.*, derivations.id AS derivation_id, derivations.file_name,
    systems.system, package_derivations.target,
    derivations_by_output_details_set.derivation_output_details_set_id
  FROM packages
  INNER JOIN package_derivations
    ON packages.id = package_derivations.package_id
  INNER JOIN systems
    ON package_derivations.system_id = systems.id
  INNER JOIN derivations
    ON package_derivations.derivation_id = derivations.id
  INNER JOIN derivations_by_output_details_set
    ON derivations.id = derivations_by_output_details_set.derivation_id
  WHERE package_derivations.id IN (
    SELECT guix_revision_package_derivations.package_derivation_id
    FROM guix_revision_package_derivations
    WHERE revision_id = $2
  )" extra-constraints "
)
SELECT base_packages.name, base_packages.version,
  base_packages.package_metadata_id, base_packages.file_name,
  base_packages.system, base_packages.target,"
   (if include-builds?
       "
  (
    SELECT JSON_AGG(
             json_build_object(
               'build_server_id', builds.build_server_id,
               'build_server_build_id', builds.build_server_build_id,
               'status',  latest_build_status.status,
               'timestamp',  latest_build_status.timestamp,
               'build_for_equivalent_derivation',
               builds.derivation_file_name != base_packages.file_name
             )
             ORDER BY latest_build_status.timestamp
           )
    FROM builds
    INNER JOIN latest_build_status
      ON builds.id = latest_build_status.build_id
    WHERE builds.derivation_output_details_set_id =
          base_packages.derivation_output_details_set_id
  ) AS base_builds,"
       "")
   "
  target_packages.name, target_packages.version,
  target_packages.package_metadata_id, target_packages.file_name,
  target_packages.system, target_packages.target"
   (if include-builds?
       ",
  (
    SELECT JSON_AGG(
             json_build_object(
               'build_server_id', builds.build_server_id,
               'build_server_build_id', builds.build_server_build_id,
               'status',  latest_build_status.status,
               'timestamp',  latest_build_status.timestamp,
               'build_for_equivalent_derivation',
               builds.derivation_file_name != target_packages.file_name
             )
             ORDER BY latest_build_status.timestamp
           )
    FROM builds
    INNER JOIN latest_build_status
      ON builds.id = latest_build_status.build_id
    WHERE builds.derivation_output_details_set_id =
          target_packages.derivation_output_details_set_id
  ) AS target_builds"
       "")
   "
FROM base_packages
FULL OUTER JOIN target_packages
  ON base_packages.name = target_packages.name
  AND base_packages.version = target_packages.version
  AND base_packages.system = target_packages.system
  AND base_packages.target = target_packages.target
WHERE
  (
    base_packages.id IS NULL OR
    target_packages.id IS NULL OR
    base_packages.file_name != target_packages.file_name
  )"
   (if after-name
       "
  AND coalesce(base_packages.name, target_packages.name) > $3"
       "")
   (if exclude-unchanged-outputs?
       "
  AND
  (
    (
      base_packages.derivation_output_details_set_id IS NULL OR
      target_packages.derivation_output_details_set_id IS NULL
    ) OR (
      base_packages.derivation_output_details_set_id <>
      target_packages.derivation_output_details_set_id
    )
  )
  "
       "")
   (cond
    ((eq? build-change #f) "")
    ((eq? build-change 'unknown)
     "
  AND (
    (
      base_packages.id IS NULL OR
      target_packages.id IS NULL
    )
    OR
    (
      NOT EXISTS (
        SELECT 1
        FROM builds
        INNER JOIN latest_build_status
          ON builds.id = latest_build_status.build_id
        WHERE builds.derivation_output_details_set_id =
              base_packages.derivation_output_details_set_id
          AND (
            latest_build_status.status = 'succeeded'
            OR
            latest_build_status.status = 'failed'
          )
      )
      AND NOT EXISTS (
        SELECT 1
        FROM builds
        INNER JOIN latest_build_status
          ON builds.id = latest_build_status.build_id
        WHERE builds.derivation_output_details_set_id =
              target_packages.derivation_output_details_set_id
          AND (
            latest_build_status.status = 'succeeded'
            OR
            latest_build_status.status = 'failed'
          )
      )
    )
  )")
    (else
     (let ((exists-build-with-status
            (lambda (base-or-target status)
              (simple-format
               #f
               "EXISTS (
        SELECT 1
        FROM builds
        INNER JOIN latest_build_status
          ON builds.id = latest_build_status.build_id
        WHERE builds.derivation_output_details_set_id =
              ~A_packages.derivation_output_details_set_id
          AND latest_build_status.status = '~A'
      )
"
               base-or-target
               status)))
           (not-exists-build-with-status
            (lambda (base-or-target status)
              (simple-format
               #f
               "NOT EXISTS (
        SELECT 1
        FROM builds
        INNER JOIN latest_build_status
          ON builds.id = latest_build_status.build_id
        WHERE builds.derivation_output_details_set_id =
              ~A_packages.derivation_output_details_set_id
          AND latest_build_status.status = '~A'
      )
"
               base-or-target
               status)))
           (criteria
            (lambda args
              (string-append
               "\n  AND "
               (string-join
                args
                "  \nAND\n  ")))))
       (string-append
        "
  AND base_packages.id IS NOT NULL
  AND target_packages.id IS NOT NULL"
        (cond
         ((eq? build-change 'broken)
          (criteria
           (exists-build-with-status "base" "succeeded")
           (exists-build-with-status "target" "failed")
           (not-exists-build-with-status "target" "succeeded")))
         ((eq? build-change 'fixed)
          (criteria
           (exists-build-with-status "base" "failed")
           (not-exists-build-with-status "base" "succeeded")
           (exists-build-with-status "target" "succeeded")))
         ((eq? build-change 'still-failing)
          (criteria
           (not-exists-build-with-status "base" "succeeded")
           (exists-build-with-status "base" "failed")
           (not-exists-build-with-status "target" "succeeded")
           (exists-build-with-status "target" "failed")))
         ((eq? build-change 'still-working)
          (criteria
           (exists-build-with-status "base" "succeeded")
           (exists-build-with-status "target" "succeeded")))
         (else
          (error "unknown build-change-value")))))))
   "
ORDER BY coalesce(base_packages.name, target_packages.name) ASC, base_packages.version, target_packages.version"
   (if limit-results
       (simple-format
        #f
        "
LIMIT ~A"
        (number->string limit-results))
       "")))

  (exec-query conn query `(,base_guix_revision_id
                           ,target_guix_revision_id
                           ,@(if after-name
                                 (list after-name)
                                 '()))))

(define* (package-differences-data conn
                                   base_guix_revision_id
                                   target_guix_revision_id)
  (define query
    (string-append "
WITH base_packages AS (
  SELECT *
  FROM packages
  WHERE id IN (
    SELECT package_id
    FROM package_derivations
    INNER JOIN guix_revision_package_derivations
      ON package_derivations.id =
         guix_revision_package_derivations.package_derivation_id
    WHERE guix_revision_package_derivations.revision_id = $1
  )
), target_packages AS (
  SELECT *
  FROM packages
  WHERE id IN (
    SELECT package_id
    FROM package_derivations
    INNER JOIN guix_revision_package_derivations
      ON package_derivations.id =
         guix_revision_package_derivations.package_derivation_id
    WHERE guix_revision_package_derivations.revision_id = $2
  )
)
SELECT base_packages.name, base_packages.version,
  base_packages.package_metadata_id,
  target_packages.name, target_packages.version,
  target_packages.package_metadata_id
FROM base_packages
FULL OUTER JOIN target_packages
  ON base_packages.name = target_packages.name
  AND base_packages.version = target_packages.version
WHERE
  base_packages.id IS NULL OR
  target_packages.id IS NULL OR
  base_packages.id != target_packages.id
ORDER BY coalesce(base_packages.name, target_packages.name) ASC, base_packages.version, target_packages.version"))

  (exec-query conn query (list base_guix_revision_id target_guix_revision_id)))

(define (package-data->package-data-vhashes package-data)
  (define (add-data-to-vhash data vhash)
    (let ((key (first data)))
      (if (or (eq? #f key)
              (string-null? key))
          vhash
          (vhash-cons key
                      (drop data 1)
                      vhash))))

  (apply values
         (fold (lambda (row result)
                 (let-values (((base-row-part target-row-part) (split-at row 3)))
                   (match result
                     ((base-package-data target-package-data)
                      (list (add-data-to-vhash base-row-part base-package-data)
                            (add-data-to-vhash target-row-part target-package-data))))))
               (list vlist-null vlist-null)
               package-data)))

(define (package-derivation-data->package-derivation-data-vhashes package-data)
  (define (add-data-to-vhash data vhash)
    (let ((key (first data)))
      (if (or (eq? key #f)
              (string-null? key))
          vhash
          (vhash-cons key
                      (drop data 1)
                      vhash))))

  (apply values
         (fold (lambda (row result)
                 (let-values (((base-row-part target-row-part) (split-at row 7)))
                   (match result
                     ((base-package-data target-package-data)
                      (list (add-data-to-vhash base-row-part base-package-data)
                            (add-data-to-vhash target-row-part target-package-data))))))
               (list vlist-null vlist-null)
               package-data)))

(define (package-derivation-data->names-and-versions package-data)
  (reverse
   (pair-fold
    (lambda (pair result)
      (match pair
        (((name . version))
         (cons (cons name version)
               result))
        (((name1 . version1) (name2 . version2) rest ...)
         (if (and (string=? name1 name2)
                  (string=? version1 version2))
             result
             (cons (cons name1 version1)
                   result)))))
    '()
    (map (match-lambda
           ((base-name base-version _ _ _ _ _ target-name target-version _ _ _ _ _)
            (if (or (and (string? base-name) (string-null? base-name))
                    (eq? base-name #f))
                (cons target-name target-version)
                (cons base-name base-version))))
         package-data))))

(define (package-derivation-data-vhash->derivations conn packages-vhash)
  (define (vhash->derivation-ids vhash)
    (vhash-fold (lambda (key value result)
                  (cons (third value)
                        result))
                '()
                vhash))

  (let* ((derivation-ids
          (vhash->derivation-ids packages-vhash))
         (derivation-data
          (select-derivations-by-id conn derivation-ids)))
    derivation-data))

(define (package-derivation-data-vhash->derivations-and-build-status
         conn
         package-derivation-data-vhash
         systems
         targets
         build-statuses)

  (define (vhash->derivation-file-names vhash)
    (vhash-fold (lambda (key value result)
                  (cons (third value)
                        result))
                '()
                vhash))

  (let* ((derivation-file-names
          (vhash->derivation-file-names package-derivation-data-vhash)))
    (if (null? derivation-file-names)
        '()
        (select-derivations-and-build-status
         conn
         #:file-names derivation-file-names
         #:systems (if (null? systems) #f systems)
         #:targets (if (null? targets) #f targets)
         #:build-statuses (if (null? build-statuses) #f build-statuses)))))

(define (package-data-vhash->package-name-and-version-hash-table vhash)
  (vhash-fold (lambda (name details result)
                (let ((key (cons name (first details))))
                  (hash-set! result
                             key
                             (cons (cdr details)
                                   (or (hash-ref result key)
                                       '())))
                  result))
              (make-hash-table)
              vhash))

(define (package-data-vhashes->new-packages base-packages-vhash target-packages-vhash)
  (hash-map->list
   (match-lambda*
     (((name . version) metadata ...)
      `((name . ,name)
        (version . ,version))))
   (package-data-vhash->package-name-and-version-hash-table
    (vlist-filter (match-lambda
                    ((name . details)
                     (not (vhash-assoc name base-packages-vhash))))
                  target-packages-vhash))))

(define (package-data-vhashes->removed-packages base-packages-vhash target-packages-vhash)
  (hash-map->list
   (match-lambda*
     (((name . version) metadata ...)
      `((name . ,name)
        (version . ,version))))
   (package-data-vhash->package-name-and-version-hash-table
    (vlist-filter (match-lambda
                    ((name . details)
                     (not (vhash-assoc name target-packages-vhash))))
                  base-packages-vhash))))

(define (package-data-vhash->package-versions-hash-table package-data-vhash)
  (vhash-fold (lambda (name details result)
                (let ((version (first details))
                      (known-versions (or (hash-ref result name)
                                          '())))
                  (hash-set! result
                             name
                             (cons version known-versions))
                  result))
              (make-hash-table)
              package-data-vhash))

(define (package-data-version-changes base-packages-vhash target-packages-vhash)
  (let ((base-versions
         (package-data-vhash->package-versions-hash-table
          base-packages-vhash))
        (target-versions
         (package-data-vhash->package-versions-hash-table
          target-packages-vhash)))

    (hash-fold (lambda (name target-versions result)
                 (let ((base-versions (hash-ref base-versions name)))
                   (if base-versions
                       (let ((base-version-numbers base-versions)
                             (target-version-numbers target-versions))
                         (if (equal? base-version-numbers target-version-numbers)
                             result
                             (cons
                              `(,name . ((base . ,(list->vector base-version-numbers))
                                         (target . ,(list->vector target-version-numbers))))
                              result)))
                       result)))
               '()
               target-versions)))

(define (package-derivation-data-changes names-and-versions
                                         base-packages-vhash
                                         target-packages-vhash)

  (define base-package-details-by-name-and-version
    (package-data-vhash->package-name-and-version-hash-table base-packages-vhash))

  (define target-package-details-by-name-and-version
    (package-data-vhash->package-name-and-version-hash-table target-packages-vhash))

  (define (derivation-system-and-target-list->alist lst)
    (if (null? lst)
        '()
        `(,(match (first lst)
             ((derivation-file-name system target builds)
              `((system . ,system)
                (target . ,target)
                (derivation-file-name . ,derivation-file-name)
                (builds               . ,(if (or (and (string? builds)
                                                      (string-null? builds))
                                                 (eq? #f builds))
                                             #()
                                             (json-string->scm builds))))))
          ,@(derivation-system-and-target-list->alist (cdr lst)))))

  (list->vector
   (filter-map
    (lambda (name-and-version)
      (let ((base-packages-entry
             (hash-ref base-package-details-by-name-and-version
                       name-and-version))
            (target-packages-entry
             (hash-ref target-package-details-by-name-and-version
                       name-and-version)))
        (cond
         ((and base-packages-entry target-packages-entry)
          (let ((base-derivations (map cdr base-packages-entry))
                (target-derivations (map cdr target-packages-entry)))
            (if (equal? base-derivations target-derivations)
                #f
                `((name . ,(car name-and-version))
                  (version . ,(cdr name-and-version))
                  (base . ,(list->vector
                            (derivation-system-and-target-list->alist
                             base-derivations)))
                  (target . ,(list->vector
                              (derivation-system-and-target-list->alist
                               target-derivations)))))))
         (base-packages-entry
          (let ((base-derivations (map cdr base-packages-entry)))
            `((name . ,(car name-and-version))
              (version . ,(cdr name-and-version))
              (base . ,(list->vector
                        (derivation-system-and-target-list->alist
                         base-derivations)))
              (target . ,(list->vector '())))))
         (else
          (let ((target-derivations (map cdr target-packages-entry)))
            `((name . ,(car name-and-version))
              (version . ,(cdr name-and-version))
              (base . ,(list->vector '()))
              (target . ,(list->vector
                          (derivation-system-and-target-list->alist
                           target-derivations)))))))))
    names-and-versions)))

(define* (lint-warning-differences-data conn
                                        base-guix-revision-id
                                        target-guix-revision-id
                                        locale)
  (define query
    (string-append "
WITH base_lint_warnings AS (
  SELECT DISTINCT ON (lint_warnings.id) lint_warnings.id,
         packages.name, packages.version,
         lint_checkers.name AS lint_checker_name,
         translated_lint_checker_descriptions.description AS lint_checker_description,
         lint_checkers.network_dependent AS lint_checker_network_dependent,
         locations.file, locations.line, locations.column_number,
         lint_warning_messages.message,
         lint_warning_messages.locale AS lint_warning_messages_locale,
         translated_lint_checker_descriptions.locale AS lint_checker_descriptions_locale
  FROM lint_warnings
  INNER JOIN packages
    ON lint_warnings.package_id = packages.id
  INNER JOIN lint_checkers
    ON lint_warnings.lint_checker_id = lint_checkers.id
  INNER JOIN (
    SELECT DISTINCT ON(lint_checkers.id) lint_checkers.id AS lint_checker_id,
             lint_checker_descriptions.description,
             lint_checker_descriptions.locale
    FROM lint_checkers
    INNER JOIN lint_checker_description_sets
      ON lint_checker_description_sets.id = lint_checkers.lint_checker_description_set_id
    INNER JOIN lint_checker_descriptions
      ON lint_checker_descriptions.id = ANY (lint_checker_description_sets.description_ids)
    INNER JOIN guix_revision_lint_checkers
      ON lint_checkers.id = guix_revision_lint_checkers.lint_checker_id
      AND guix_revision_lint_checkers.guix_revision_id = $1
    ORDER BY lint_checkers.id,
             CASE
               WHEN lint_checker_descriptions.locale = $3 THEN 2
               WHEN lint_checker_descriptions.locale = 'en_US.UTF-8' THEN 1
               ELSE 0
             END DESC
  ) AS translated_lint_checker_descriptions
    ON translated_lint_checker_descriptions.lint_checker_id = lint_checkers.id
  INNER JOIN locations
    ON lint_warnings.location_id = locations.id
  INNER JOIN lint_warning_message_sets
    ON lint_warnings.lint_warning_message_set_id = lint_warning_message_sets.id
  INNER JOIN lint_warning_messages
    ON lint_warning_messages.id = ANY (lint_warning_message_sets.message_ids)
  WHERE lint_warnings.id IN (
     SELECT lint_warning_id
     FROM guix_revision_lint_warnings
     WHERE guix_revision_id = $1
  )
  ORDER BY lint_warnings.id,
           CASE
             WHEN lint_warning_messages.locale = $3 THEN 2
             WHEN lint_warning_messages.locale = 'en_US.UTF-8' THEN 1
           ELSE 0 END DESC
), target_lint_warnings AS (
  SELECT DISTINCT ON (lint_warnings.id) lint_warnings.id,
         packages.name, packages.version,
         lint_checkers.name AS lint_checker_name,
         translated_lint_checker_descriptions.description AS lint_checker_description,
         lint_checkers.network_dependent AS lint_checker_network_dependent,
         locations.file, locations.line, locations.column_number,
         lint_warning_messages.message,
         translated_lint_checker_descriptions.locale AS lint_checker_descriptions_locale
  FROM lint_warnings
  INNER JOIN packages
    ON lint_warnings.package_id = packages.id
  INNER JOIN lint_checkers
    ON lint_warnings.lint_checker_id = lint_checkers.id
  INNER JOIN (
    SELECT DISTINCT ON(lint_checkers.id) lint_checkers.id AS lint_checker_id,
             lint_checker_descriptions.description description,
             lint_checker_descriptions.locale
    FROM lint_checkers
    INNER JOIN lint_checker_description_sets
      ON lint_checker_description_sets.id = lint_checkers.lint_checker_description_set_id
    INNER JOIN lint_checker_descriptions
      ON lint_checker_descriptions.id = ANY (lint_checker_description_sets.description_ids)
    INNER JOIN guix_revision_lint_checkers
      ON lint_checkers.id = guix_revision_lint_checkers.lint_checker_id
      AND guix_revision_lint_checkers.guix_revision_id = $2
    ORDER BY lint_checkers.id,
             CASE
               WHEN lint_checker_descriptions.locale = $3 THEN 2
               WHEN lint_checker_descriptions.locale = 'en_US.UTF-8' THEN 1
               ELSE 0
             END DESC
  ) AS translated_lint_checker_descriptions
    ON translated_lint_checker_descriptions.lint_checker_id = lint_checkers.id
  INNER JOIN locations
    ON lint_warnings.location_id = locations.id
  INNER JOIN lint_warning_message_sets
    ON lint_warnings.lint_warning_message_set_id = lint_warning_message_sets.id
  INNER JOIN lint_warning_messages
    ON lint_warning_messages.id = ANY (lint_warning_message_sets.message_ids)
  WHERE lint_warnings.id IN (
     SELECT lint_warning_id
     FROM guix_revision_lint_warnings
     WHERE guix_revision_id = $2
  )
  ORDER BY lint_warnings.id,
          CASE
            WHEN lint_warning_messages.locale = $3 THEN 2
            WHEN lint_warning_messages.locale = 'en_US.UTF-8' THEN 1
          ELSE 0 END DESC
)
SELECT coalesce(
         base_lint_warnings.name,
         target_lint_warnings.name
       ) AS package_name,
       coalesce(
         base_lint_warnings.version,
         target_lint_warnings.version
       ) AS package_version,
       coalesce(
         base_lint_warnings.lint_checker_name,
         target_lint_warnings.lint_checker_name
       ) AS lint_checker_name,
       coalesce(
         base_lint_warnings.message,
         target_lint_warnings.message
       ) AS message,
       coalesce(
         base_lint_warnings.lint_checker_description,
         target_lint_warnings.lint_checker_description
       ) AS lint_checker_description,
       coalesce(
         base_lint_warnings.lint_checker_network_dependent,
         target_lint_warnings.lint_checker_network_dependent
       ) AS lint_checker_network_dependent,
       coalesce(
         base_lint_warnings.file,
         target_lint_warnings.file
       ) AS file,
       coalesce(
         base_lint_warnings.line,
         target_lint_warnings.line
       ) AS line,
       coalesce(
         base_lint_warnings.column_number,
         target_lint_warnings.column_number
       ) AS column_number,
       CASE
         WHEN base_lint_warnings.name IS NULL THEN 'new'
         WHEN target_lint_warnings.name IS NULL THEN 'removed'
         ELSE 'moved'
       END AS change
FROM base_lint_warnings
FULL OUTER JOIN target_lint_warnings
  ON base_lint_warnings.name = target_lint_warnings.name
  AND base_lint_warnings.lint_checker_name = target_lint_warnings.lint_checker_name
  AND (
    base_lint_warnings.message = target_lint_warnings.message OR
    -- TODO Some lint warnings include the line number in the message, so
    -- they'll appear to be altered if the package definition moves within the
    -- file, therefore try replacing the line number to see if the message matches
    -- that way as well
    replace(base_lint_warnings.message,base_lint_warnings.line::varchar,target_lint_warnings.line::varchar) = target_lint_warnings.message
  )
WHERE
  (
    base_lint_warnings.id IS NULL OR
    target_lint_warnings.id IS NULL OR
    base_lint_warnings.id != target_lint_warnings.id
  ) AND (
    base_lint_warnings.name IS NULL OR
    target_lint_warnings.name IS NULL
  )
ORDER BY coalesce(base_lint_warnings.name, target_lint_warnings.name) ASC, base_lint_warnings.version, target_lint_warnings.version, change"))

  (exec-query conn query
              (list base-guix-revision-id
                    target-guix-revision-id
                    locale)))

(define* (system-test-derivations-differences-data conn
                                                   base_guix_revision_id
                                                   target_guix_revision_id
                                                   system)
  (define query
    (string-append "
WITH base_system_tests AS (
  SELECT name, description,
         derivations.file_name AS derivation_file_name, derivation_output_details_set_id,
         locations.file, locations.line, locations.column_number
  FROM guix_revision_system_test_derivations
  INNER JOIN system_tests
    ON guix_revision_system_test_derivations.system_test_id = system_tests.id
  INNER JOIN locations
    ON system_tests.location_id = locations.id
  INNER JOIN derivations
    ON guix_revision_system_test_derivations.derivation_id = derivations.id
  INNER JOIN derivations_by_output_details_set
    ON guix_revision_system_test_derivations.derivation_id = derivations_by_output_details_set.derivation_id
  WHERE guix_revision_id = $1
    AND guix_revision_system_test_derivations.system = $3
), target_system_tests AS (
  SELECT name, description,
         derivations.file_name AS derivation_file_name, derivation_output_details_set_id,
         locations.file, locations.line, locations.column_number
  FROM guix_revision_system_test_derivations
  INNER JOIN system_tests
    ON guix_revision_system_test_derivations.system_test_id = system_tests.id
  INNER JOIN locations
    ON system_tests.location_id = locations.id
  INNER JOIN derivations
    ON guix_revision_system_test_derivations.derivation_id = derivations.id
  INNER JOIN derivations_by_output_details_set
    ON guix_revision_system_test_derivations.derivation_id = derivations_by_output_details_set.derivation_id
  WHERE guix_revision_id = $2
    AND guix_revision_system_test_derivations.system = $3
)
SELECT base_system_tests.name, base_system_tests.description, base_system_tests.derivation_file_name,
       base_system_tests.file, base_system_tests.line, base_system_tests.column_number,
       (
         SELECT JSON_AGG(
           json_build_object(
             'build_server_id', builds.build_server_id,
             'build_server_build_id', builds.build_server_build_id,
             'status',  latest_build_status.status,
             'timestamp',  latest_build_status.timestamp,
             'build_for_equivalent_derivation',
             builds.derivation_file_name != base_system_tests.derivation_file_name
           )
           ORDER BY latest_build_status.timestamp
         )
         FROM builds
         INNER JOIN latest_build_status
           ON builds.id = latest_build_status.build_id
         WHERE builds.derivation_output_details_set_id =
               base_system_tests.derivation_output_details_set_id
       ) AS base_builds,
       target_system_tests.name, target_system_tests.description, target_system_tests.derivation_file_name,
       target_system_tests.file, target_system_tests.line, target_system_tests.column_number,
       (
         SELECT JSON_AGG(
           json_build_object(
             'build_server_id', builds.build_server_id,
             'build_server_build_id', builds.build_server_build_id,
             'status',  latest_build_status.status,
             'timestamp',  latest_build_status.timestamp,
             'build_for_equivalent_derivation',
             builds.derivation_file_name != target_system_tests.derivation_file_name
           )
           ORDER BY latest_build_status.timestamp
         )
         FROM builds
         INNER JOIN latest_build_status
           ON builds.id = latest_build_status.build_id
         WHERE builds.derivation_output_details_set_id =
               target_system_tests.derivation_output_details_set_id
       ) AS target_builds
FROM base_system_tests
FULL OUTER JOIN target_system_tests
  ON base_system_tests.name = target_system_tests.name
WHERE
  base_system_tests.name IS NULL OR
  target_system_tests.name IS NULL OR
  base_system_tests.derivation_file_name != target_system_tests.derivation_file_name
ORDER BY coalesce(base_system_tests.name, target_system_tests.name) ASC"))

  (map
   (match-lambda
     ((base_name base_description base_derivation_file_name
                 base_file base_line base_column_number
                 base_builds
                 target_name target_description target_derivation_file_name
                 target_file target_line target_column_number
                 target_builds)
      (define (location->alist file line column-number)
        `((file          . ,file)
          (line          . ,(string->number line))
          (column_number . ,(string->number column-number))))

      `((name        . ,(or base_name target_name))
        (description . ,(if (and (string? base_description)
                                 (string? target_description)
                                 (string=? base_description target_description))
                            base_description
                            `((base   . ,(if (null? base_description)
                                             'null
                                             base_description))
                              (target . ,(if (null? target_description)
                                             'null
                                             target_description)))))
        (derivation  . ,(if (and (string? base_derivation_file_name)
                                 (string? target_derivation_file_name)
                                 (string=? base_derivation_file_name
                                           target_derivation_file_name))
                            base_derivation_file_name
                            `((base   . ,(if (null? base_derivation_file_name)
                                             'null
                                             base_derivation_file_name))
                              (target . ,(if (null? target_derivation_file_name)
                                             'null
                                             target_derivation_file_name)))))
        (location    . ,(if
                         (and (string? base_file)
                              (string? target_file)
                              (string=? base_file target_file)
                              (string=? base_line target_line)
                              (string=? base_column_number target_column_number))
                         (location->alist base_file base_line base_column_number)
                         `((base . ,(if (null? base_file)
                                        'null
                                        (location->alist
                                         base_file
                                         base_line
                                         base_column_number)))
                           (target . ,(if (null? target_file)
                                          'null
                                          (location->alist
                                           target_file
                                           target_line
                                           target_column_number))))))
        (builds      . ,(if (and (string? base_derivation_file_name)
                                 (string? target_derivation_file_name)
                                 (string=? base_derivation_file_name
                                           target_derivation_file_name))
                            (json-string->scm base_builds)
                            `((base   . ,(if (null? base_builds)
                                             #()
                                             (json-string->scm base_builds)))
                              (target . ,(if (null? target_builds)
                                             #()
                                             (json-string->scm target_builds)))))))))
   (exec-query-with-null-handling
    conn
    query
    (list base_guix_revision_id
          target_guix_revision_id
          system))))

(define (channel-news-differences-data conn
                                       base-guix-revision-id
                                       target-guix-revision-id)
  (define query
    "
WITH base_news_entries AS (
  SELECT channel_news_entries.id,
         channel_news_entries.commit,
         channel_news_entries.tag,
         (
           SELECT JSON_AGG(ARRAY[lang,text])
           FROM channel_news_entry_text
           INNER JOIN channel_news_entry_titles
             ON channel_news_entry_text.id = channel_news_entry_titles.channel_news_entry_text_id
             WHERE channel_news_entry_titles.channel_news_entry_id = channel_news_entries.id
         ) AS title_text,
         (
           SELECT JSON_AGG(ARRAY[lang,text])
           FROM channel_news_entry_text
           INNER JOIN channel_news_entry_bodies
             ON channel_news_entry_text.id = channel_news_entry_bodies.channel_news_entry_text_id
           WHERE channel_news_entry_bodies.channel_news_entry_id = channel_news_entries.id
         ) AS body_text
  FROM channel_news_entries
  WHERE id IN (
    SELECT channel_news_entry_id
    FROM guix_revision_channel_news_entries
    WHERE guix_revision_channel_news_entries.guix_revision_id = $1
  )
), target_news_entries AS (
  SELECT channel_news_entries.id,
         channel_news_entries.commit,
         channel_news_entries.tag,
         (
           SELECT JSON_AGG(ARRAY[lang,text])
           FROM channel_news_entry_text
           INNER JOIN channel_news_entry_titles
             ON channel_news_entry_text.id = channel_news_entry_titles.channel_news_entry_text_id
             WHERE channel_news_entry_titles.channel_news_entry_id = channel_news_entries.id
         ) AS title_text,
         (
           SELECT JSON_AGG(ARRAY[lang,text])
           FROM channel_news_entry_text
           INNER JOIN channel_news_entry_bodies
             ON channel_news_entry_text.id = channel_news_entry_bodies.channel_news_entry_text_id
           WHERE channel_news_entry_bodies.channel_news_entry_id = channel_news_entries.id
         ) AS body_text
  FROM channel_news_entries
  WHERE id IN (
    SELECT channel_news_entry_id
    FROM guix_revision_channel_news_entries
    WHERE guix_revision_channel_news_entries.guix_revision_id = $2
  )
)
SELECT coalesce(
         base_news_entries.commit,
         target_news_entries.commit
       ) AS commit,
       coalesce(
         base_news_entries.tag,
         target_news_entries.tag
       ) AS tag,
       coalesce(
         base_news_entries.title_text,
         target_news_entries.title_text
       ) AS title_text,
       coalesce(
         base_news_entries.body_text,
         target_news_entries.body_text
       ) AS body_text,
       CASE
         WHEN base_news_entries.id IS NULL THEN 'new'
         WHEN target_news_entries.id IS NULL THEN 'removed'
         ELSE 'changed'
       END AS change
FROM base_news_entries
FULL OUTER JOIN target_news_entries
  ON base_news_entries.commit = target_news_entries.commit
WHERE (
  base_news_entries.id IS NULL OR
  target_news_entries.id IS NULL OR
  base_news_entries.id != target_news_entries.id
)")

  (map
   (match-lambda
     ((commit tag title_text body_text change)
      (list commit
            tag
            (map (match-lambda
                   (#(lang text)
                    (cons lang text)))
                 (vector->list
                  (json-string->scm title_text)))
            (map (match-lambda
                   (#(lang text)
                    (cons lang text)))
                 (vector->list
                  (json-string->scm body_text)))
            (string->symbol change))))
   (exec-query-with-null-handling conn query
                                  (list base-guix-revision-id
                                        target-guix-revision-id))))

