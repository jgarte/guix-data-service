(define-module (guix-data-service model package-metadata)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (json)
  #:use-module (gcrypt hash)
  #:use-module (rnrs bytevectors)
  #:use-module (guix base16)
  #:use-module (guix inferior)
  #:use-module (guix-data-service model location)
  #:use-module (guix-data-service model utils)
  #:export (select-package-metadata-by-revision-name-and-version
            inferior-packages->package-metadata-ids))

(define (select-package-metadata package-metadata-values)
  (define fields
    '("synopsis" "description" "home_page" "location_id" "license_set_id"))

  (string-append "SELECT id, " (string-join (map
                                             (lambda (name)
                                               (string-append
                                                "package_metadata." name))
                                             fields)
                                            ", ") " "
                 "FROM package_metadata "
                 "JOIN (VALUES "
                 (string-join (map
                               (match-lambda
                                 ((synopsis description home-page location-id
                                            license-set-id)
                                  (apply
                                   simple-format
                                   #f
                                   (string-append
                                    "("
                                    (string-join
                                     (list-tabulate
                                      (length fields)
                                      (lambda (n) "~A"))
                                     ",")
                                    ")")
                                   (list
                                    (value->quoted-string-or-null synopsis)
                                    (value->quoted-string-or-null description)
                                    (value->quoted-string-or-null home-page)
                                    location-id
                                    license-set-id))))
                               package-metadata-values)
                              ",")
                 ") AS vals (" (string-join fields ", ") ") "
                 "ON "
                 (string-join
                  (map (lambda (field)
                         (string-append
                          "package_metadata." field " = vals." field))
                       fields)
                  " AND ")))

(define (select-package-metadata-by-revision-name-and-version
         conn revision-commit-hash name version)
  (define query "
SELECT package_metadata.synopsis, package_metadata.description,
  package_metadata.home_page,
  locations.file, locations.line, locations.column_number,
  (SELECT JSON_AGG((license_data.*))
   FROM (
     SELECT licenses.name, licenses.uri, licenses.comment
     FROM licenses
     INNER JOIN license_sets ON licenses.id = ANY(license_sets.license_ids)
     WHERE license_sets.id = package_metadata.license_set_id
     ORDER BY licenses.name
   ) AS license_data
  ) AS licenses
FROM package_metadata
INNER JOIN packages
  ON package_metadata.id = packages.package_metadata_id
LEFT OUTER JOIN locations
  ON package_metadata.location_id = locations.id
WHERE packages.id IN (
  SELECT package_derivations.package_id
  FROM package_derivations
  INNER JOIN guix_revision_package_derivations
    ON package_derivations.id =
    guix_revision_package_derivations.package_derivation_id
  INNER JOIN guix_revisions
    ON guix_revision_package_derivations.revision_id = guix_revisions.id
  WHERE guix_revisions.commit = $1
)
  AND packages.name = $2
  AND packages.version = $3")

  (map
   (match-lambda
     ((synopsis description home-page file line column-number
                license-json)
      (list synopsis description home-page file line column-number
            (if (string-null? license-json)
                #()
                (json-string->scm license-json)))))
   (exec-query conn query (list revision-commit-hash name version))))

(define (insert-package-metadata metadata-rows)
  (string-append "INSERT INTO package_metadata "
                 "(synopsis, description, home_page, location_id, license_set_id) "
                 "VALUES "
                 (string-join
                  (map (match-lambda
                         ((synopsis description home_page
                                    location-id license-set-id)
                          (string-append
                           "("
                           (value->quoted-string-or-null synopsis) ","
                           (value->quoted-string-or-null description) ","
                           (value->quoted-string-or-null home_page) ","
                           location-id ","
                           license-set-id
                           ")")))
                       metadata-rows)
                  ",")
                 " RETURNING id"
                 ";"))


(define (inferior-packages->package-metadata-ids conn
                                                 packages
                                                 license-set-ids)
  (define package-metadata
    (map (lambda (package license-set-id)
           (list (inferior-package-synopsis package)
                 (inferior-package-description package)
                 (inferior-package-home-page package)
                 (location->location-id
                  conn
                  (inferior-package-location package))
                 license-set-id))
         packages
         license-set-ids))

  (let* ((existing-package-metadata-entries
          (exec-query->vhash conn
                             (select-package-metadata package-metadata)
                             cdr
                             first)) ;; id))
         (missing-package-metadata-entries
          (delete-duplicates
           (filter (lambda (metadata)
                     (not (vhash-assoc metadata
                                       existing-package-metadata-entries)))
                   package-metadata)))
         (new-package-metadata-entries
          (if (null? missing-package-metadata-entries)
              '()
              (map first
                   (exec-query conn
                               (insert-package-metadata
                                missing-package-metadata-entries)))))
         (new-entries-id-lookup-vhash
          (two-lists->vhash missing-package-metadata-entries
                            new-package-metadata-entries)))

    (map (lambda (package-metadata-values)
           (cdr
            (or (vhash-assoc package-metadata-values
                             existing-package-metadata-entries)
                (vhash-assoc package-metadata-values
                             new-entries-id-lookup-vhash)
                (begin
                  (error "missing package-metadata entry"
                         package-metadata-values)))))
         package-metadata)))
