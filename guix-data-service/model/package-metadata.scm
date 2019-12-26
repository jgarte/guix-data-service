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
                         (if (member field '("home_page" "location_id"
                                             "license_set_id"))
                             (string-append
                              "(package_metadata." field " = vals." field " OR "
                              "(package_metadata." field " IS NULL AND"
                              " vals." field " IS NULL))")
                             (string-append
                              "package_metadata." field " = vals." field)))
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
                 (non-empty-string-or-false
                  (inferior-package-home-page package))
                 (location->location-id
                  conn
                  (inferior-package-location package))
                 license-set-id))
         packages
         license-set-ids))

  (insert-missing-data-and-return-all-ids
   conn
   "package_metadata"
   '(synopsis description home_page location_id license_set_id)
   (map (match-lambda
          ((synopsis description home-page location-id license-set-id)
           (list synopsis
                 description
                 (if (string? home-page)
                     home-page
                     NULL)
                 location-id
                 license-set-id)))
        package-metadata)
   ;; There can be duplicated entires in package-metadata, for example where
   ;; you have one package definition which interits from another, and just
   ;; overrides the version and the source, the package_metadata entries for
   ;; both definitions will be the same.
   #:delete-duplicates? #t))
