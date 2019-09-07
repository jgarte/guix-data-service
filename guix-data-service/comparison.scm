(define-module (guix-data-service comparison)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix-data-service model derivation)
  #:export (package-data->package-data-vhashes
            package-differences-data
            package-data-vhash->derivations
            package-data->names-and-versions
            package-data-vhash->derivations-and-build-status
            package-data-vhashes->new-packages
            package-data-vhashes->removed-packages
            package-data-version-changes
            package-data-derivation-changes

            lint-warning-differences-data))

(define* (package-differences-data conn
                                   base_guix_revision_id
                                   target_guix_revision_id
                                   #:key
                                   (systems #f)
                                   (targets #f))
  (define extra-constraints
    (string-append
     (if systems
         (string-append
          " AND package_derivations.system IN ("
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
  SELECT packages.*, derivations.file_name,
    package_derivations.system, package_derivations.target
  FROM packages
  INNER JOIN package_derivations
    ON packages.id = package_derivations.package_id
  INNER JOIN derivations
    ON package_derivations.derivation_id = derivations.id
  WHERE package_derivations.id IN (
    SELECT guix_revision_package_derivations.package_derivation_id
    FROM guix_revision_package_derivations
    WHERE revision_id = $1
  )" extra-constraints
"), target_packages AS (
  SELECT packages.*, derivations.file_name,
    package_derivations.system, package_derivations.target
  FROM packages
  INNER JOIN package_derivations
    ON packages.id = package_derivations.package_id
  INNER JOIN derivations
    ON package_derivations.derivation_id = derivations.id
  WHERE package_derivations.id IN (
    SELECT guix_revision_package_derivations.package_derivation_id
    FROM guix_revision_package_derivations
    WHERE revision_id = $2
  )" extra-constraints
")
SELECT base_packages.name, base_packages.version,
  base_packages.package_metadata_id, base_packages.file_name,
  base_packages.system, base_packages.target,
  target_packages.name, target_packages.version,
  target_packages.package_metadata_id, target_packages.file_name,
  target_packages.system, target_packages.target
FROM base_packages
FULL OUTER JOIN target_packages
  ON base_packages.name = target_packages.name
  AND base_packages.version = target_packages.version
  AND base_packages.system = target_packages.system
  AND base_packages.target = target_packages.target
WHERE
  base_packages.id IS NULL OR
  target_packages.id IS NULL OR
  base_packages.id != target_packages.id OR
  base_packages.file_name != target_packages.file_name
ORDER BY coalesce(base_packages.name, target_packages.name) ASC, base_packages.version, target_packages.version"))

  (exec-query conn query (list base_guix_revision_id target_guix_revision_id)))

(define (package-data->package-data-vhashes package-data)
  (define (add-data-to-vhash data vhash)
    (let ((key (first data)))
      (if (string-null? key)
          vhash
          (vhash-cons key
                      (drop data 1)
                      vhash))))

  (apply values
         (fold (lambda (row result)
                 (let-values (((base-row-part target-row-part) (split-at row 6)))
                   (match result
                     ((base-package-data target-package-data)
                      (list (add-data-to-vhash base-row-part base-package-data)
                            (add-data-to-vhash target-row-part target-package-data))))))
               (list vlist-null vlist-null)
               package-data)))

(define (package-data->names-and-versions package-data)
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
           ((base-name base-version _ _ _ _ target-name target-version _ _ _ _)
            (if (string-null? base-name)
                (cons target-name target-version)
                (cons base-name base-version))))
         package-data))))

(define (package-data-vhash->derivations conn packages-vhash)
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

(define (package-data-vhash->derivations-and-build-status conn packages-vhash
                                                          systems targets
                                                          build-statuses)
  (define (vhash->derivation-file-names vhash)
    (vhash-fold (lambda (key value result)
                  (cons (third value)
                        result))
                '()
                vhash))

  (let* ((derivation-file-names
          (vhash->derivation-file-names packages-vhash)))
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
  (define (system-and-target<? a b)
    (if (string=? (car a) (car b))
        (string<? (cdr a) (cdr b))
        (string<? (car a) (car b))))

  (define (add-version-system-and-target-to-alist alist data)
    (match data
      ((version package-metadata-id derivation-id system target)
       (let ((systems-for-version (or (and=> (assoc version alist) cdr)
                                      '())))
         `((,version . ,(sort (cons (cons system target)
                                    systems-for-version)
                              system-and-target<?))
           ,@(alist-delete version alist))))))

  (vhash-fold (lambda (name details result)
                (let ((version (first details))
                      (known-versions (or (hash-ref result name)
                                          '())))
                  (hash-set! result
                             name
                             (add-version-system-and-target-to-alist known-versions
                                                                     details))
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
                       (let ((base-version-numbers (map car base-versions))
                             (target-version-numbers (map car target-versions)))
                         (if (equal? base-version-numbers target-version-numbers)
                             result
                             (cons
                              `(,name . ((base . ,(list->vector base-version-numbers))
                                         (target . ,(list->vector target-version-numbers))))
                              result)))
                       result)))
               '()
               target-versions)))

(define (package-data-derivation-changes names-and-versions
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
             ((derivation-file-name system target)
              `((system . ,system)
                (target . ,target)
                (derivation-file-name . ,derivation-file-name))))
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

(define (lint-warning-differences-data conn
                                       base-guix-revision-id
                                       target-guix-revision-id)
  (define query
    (string-append "
WITH base_lint_warnings AS (
  SELECT lint_warnings.id,
         packages.name, packages.version,
         lint_checkers.name AS lint_checker_name,
         lint_checkers.description AS lint_checker_description,
         lint_checkers.network_dependent AS lint_checker_network_dependent,
         locations.file, locations.line, locations.column_number,
         lint_warning_messages.message
  FROM lint_warnings
  INNER JOIN packages
    ON lint_warnings.package_id = packages.id
  INNER JOIN lint_checkers
    ON lint_warnings.lint_checker_id = lint_checkers.id
  INNER JOIN locations
    ON lint_warnings.location_id = locations.id
  INNER JOIN lint_warning_message_sets
    ON lint_warnings.lint_warning_message_set_id = lint_warning_message_sets.id
  INNER JOIN lint_warning_messages
    ON lint_warning_messages.id = ANY (lint_warning_message_sets.message_ids) AND
       lint_warning_messages.locale = 'en_US.utf8'
  WHERE lint_warnings.id IN (
    SELECT lint_warning_id
    FROM guix_revision_lint_warnings
    WHERE guix_revision_id = $1
  )
), target_lint_warnings AS (
  SELECT lint_warnings.id,
         packages.name, packages.version,
         lint_checkers.name AS lint_checker_name,
         lint_checkers.description AS lint_checker_description,
         lint_checkers.network_dependent AS lint_checker_network_dependent,
         locations.file, locations.line, locations.column_number,
         lint_warning_messages.message
  FROM lint_warnings
  INNER JOIN packages
    ON lint_warnings.package_id = packages.id
  INNER JOIN lint_checkers
    ON lint_warnings.lint_checker_id = lint_checkers.id
  INNER JOIN locations
    ON lint_warnings.location_id = locations.id
  INNER JOIN lint_warning_message_sets
    ON lint_warnings.lint_warning_message_set_id = lint_warning_message_sets.id
  INNER JOIN lint_warning_messages
    ON lint_warning_messages.id = ANY (lint_warning_message_sets.message_ids) AND
       lint_warning_messages.locale = 'en_US.utf8'
  WHERE lint_warnings.id IN (
    SELECT lint_warning_id
    FROM guix_revision_lint_warnings
    WHERE guix_revision_id = $2
  )
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
                    target-guix-revision-id)))
