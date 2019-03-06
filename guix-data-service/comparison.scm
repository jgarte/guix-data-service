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
            package-data-other-changes))

(define (package-differences-data conn base_guix_revision_id target_guix_revision_id)
  (define query
    "WITH base_packages AS (
  SELECT packages.* FROM packages INNER JOIN guix_revision_packages ON packages.id = guix_revision_packages.package_id WHERE revision_id = $1
), target_packages AS (
  SELECT packages.* FROM packages INNER JOIN guix_revision_packages ON packages.id = guix_revision_packages.package_id WHERE revision_id = $2
)
SELECT base_packages.name, base_packages.version, base_packages.package_metadata_id, base_packages.derivation_id, target_packages.name, target_packages.version, target_packages.package_metadata_id, target_packages.derivation_id
FROM base_packages
FULL OUTER JOIN target_packages ON base_packages.name = target_packages.name AND base_packages.version = target_packages.version
WHERE (base_packages.id IS NULL OR target_packages.id IS NULL OR base_packages.id != target_packages.id)
ORDER BY base_packages.name, base_packages.version, target_packages.name, target_packages.version")

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
                 (let-values (((base-row-part target-row-part) (split-at row 4)))
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
  (define (vhash->derivation-ids vhash)
    (vhash-fold (lambda (key value result)
                  (cons (third value)
                        result))
                '()
                vhash))

  (let* ((derivation-ids
          (vhash->derivation-ids packages-vhash))
         (derivation-data
          (select-derivations-and-build-status-by-id conn derivation-ids)))
    derivation-data))

(define (package-data-vhash->package-name-and-version-vhash vhash)
  (vhash-fold (lambda (name details result)
                (vhash-cons (cons name (first details))
                            (cdr details)
                            result))
              vlist-null
              vhash))

(define (package-data-vhashes->new-packages base-packages-vhash target-packages-vhash)
  (vlist->list
   (vlist-filter (match-lambda
                   ((name . details)
                    (not (vhash-assoc name base-packages-vhash))))
                 target-packages-vhash)))

(define (package-data-vhashes->removed-packages base-packages-vhash target-packages-vhash)
  (vlist->list
   (vlist-filter (match-lambda
                   ((name . details)
                    (not (vhash-assoc name target-packages-vhash))))
                 base-packages-vhash)))

(define (package-data-vhash->package-versions-vhash package-data-vhash)
  (vhash-fold (lambda (name details result)
                (let ((version (first details))
                      (known-versions (vhash-assoc name result)))
                  (if known-versions
                      (vhash-cons name
                                  (cons version known-versions)
                                  (vhash-delete name result))
                      (vhash-cons name
                                  (list version)
                                  result))))
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
                              `((,name . ((base . ,base-versions)
                                          (target . ,target-versions)))
                                ,@result)))
                        result)))
                '()
                target-versions)))

(define (package-data-other-changes base-packages-vhash target-packages-vhash)
  (define base-package-details-by-name-and-version
    (package-data-vhash->package-name-and-version-vhash base-packages-vhash))

  (define target-package-details-by-name-and-version
    (package-data-vhash->package-name-and-version-vhash target-packages-vhash))

  (vhash-fold (lambda (name-and-version target-details result)
                (let ((base-packages-entry
                       (vhash-assoc name-and-version base-package-details-by-name-and-version)))
                  (if base-packages-entry
                      (let ((base-details (cdr base-packages-entry)))
                        (if (equal? base-details target-details)
                            result
                            `((,name-and-version . ((base . ,base-details)
                                                    (target . ,target-details)))
                              ,@result)))
                      result)))
              '()
              target-package-details-by-name-and-version))
