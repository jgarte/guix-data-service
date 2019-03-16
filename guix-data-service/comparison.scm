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
            package-data-vhash->derivations-and-build-status
            package-data-vhashes->new-packages
            package-data-vhashes->removed-packages
            package-data-version-changes
            package-data-derivation-changes))

(define (package-differences-data conn base_guix_revision_id target_guix_revision_id)
  (define query
    "
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
  )
), target_packages AS (
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
  )
)
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
ORDER BY base_packages.name DESC, base_packages.version, target_packages.name, target_packages.version")

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

(define (package-data-vhash->derivations-and-build-status conn packages-vhash)
  (define (vhash->derivation-file-names vhash)
    (vhash-fold (lambda (key value result)
                  (cons (third value)
                        result))
                '()
                vhash))

  (let* ((derivation-file-names
          (vhash->derivation-file-names packages-vhash))
         (derivation-data
          (select-derivations-and-build-status-by-file-name
           conn
           derivation-file-names)))
    derivation-data))

(define (package-data-vhash->package-name-and-version-vhash vhash)
  (vhash-fold (lambda (name details result)
                (let ((key (cons name (first details))))
                  (vhash-cons key
                              (cons (cdr details)
                                    (or (and=> (vhash-assoc key result) cdr)
                                        '()))
                              (vhash-delete key result))))
              vlist-null
              vhash))

(define (package-data-vhashes->new-packages base-packages-vhash target-packages-vhash)
  (vlist->list
   (package-data-vhash->package-name-and-version-vhash
    (vlist-filter (match-lambda
                    ((name . details)
                     (not (vhash-assoc name base-packages-vhash))))
                  target-packages-vhash))))

(define (package-data-vhashes->removed-packages base-packages-vhash target-packages-vhash)
  (vlist->list
   (package-data-vhash->package-name-and-version-vhash
    (vlist-filter (match-lambda
                    ((name . details)
                     (not (vhash-assoc name target-packages-vhash))))
                  base-packages-vhash))))

(define (package-data-vhash->package-versions-vhash package-data-vhash)
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
                      (known-versions (or (and=> (vhash-assoc name result) cdr)
                                          '())))
                  (vhash-cons name
                              (add-version-system-and-target-to-alist known-versions
                                                                      details)
                              (vhash-delete name result))))
              vlist-null
              package-data-vhash))

(define (package-data-version-changes base-packages-vhash target-packages-vhash)
  (let ((base-versions (package-data-vhash->package-versions-vhash
                        base-packages-vhash))
        (target-versions (package-data-vhash->package-versions-vhash
                          target-packages-vhash)))
    (vhash-fold (lambda (name target-versions result)
                  (let ((base-versions (and=> (vhash-assoc name base-versions)
                                              cdr)))
                    (if base-versions
                        (begin
                          (if (equal? base-versions target-versions)
                              result
                              `((,name . ((base . ,(map car base-versions))
                                          (target . ,(map car target-versions))))
                                ,@result)))
                        result)))
                '()
                target-versions)))

(define (package-data-derivation-changes base-packages-vhash target-packages-vhash)
  (define base-package-details-by-name-and-version
    (package-data-vhash->package-name-and-version-vhash base-packages-vhash))

  (define target-package-details-by-name-and-version
    (package-data-vhash->package-name-and-version-vhash target-packages-vhash))

  (define (derivation-system-and-target-list->alist lst)
    (if (null? lst)
        '()
        `((,(cdr (first lst)) . ,(car (first lst)))
          ,@(derivation-system-and-target-list->alist (cdr lst)))))

  (vhash-fold
   (lambda (name-and-version target-packages-entry result)
     (let ((base-packages-entry
            (vhash-assoc name-and-version
                         base-package-details-by-name-and-version)))
       (if base-packages-entry
           (let ((base-derivations (map cdr (cdr base-packages-entry)))
                 (target-derivations (map cdr target-packages-entry)))
             (if (equal? base-derivations target-derivations)
                 result
                 `((,name-and-version
                    . ((base . ,(derivation-system-and-target-list->alist
                                 base-derivations))
                       (target . ,(derivation-system-and-target-list->alist
                                   target-derivations))))
                   ,@result)))
           result)))
   '()
   target-package-details-by-name-and-version))
