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
  #:use-module (guix packages)
  #:use-module (guix i18n)
  #:use-module (guix inferior)
  #:use-module (guix-data-service utils)
  #:use-module (guix-data-service model location)
  #:use-module (guix-data-service model utils)
  #:export (select-package-metadata-by-revision-name-and-version
            inferior-packages->package-metadata-ids
            inferior-packages->translated-package-descriptions-and-synopsis

            package-description-and-synopsis-locale-options-guix-revision

            synopsis-counts-by-locale
            description-counts-by-locale))

(define locales
  '("cs_CZ.utf8"
    "da_DK.utf8"
    "de_DE.utf8"
    "eo_EO.utf8"
    "es_ES.utf8"
    "fr_FR.utf8"
    "hu_HU.utf8"
    "pl_PL.utf8"
    "pt_BR.utf8"
    ;;"sr_SR.utf8"
    "sv_SE.utf8"
    "vi_VN.utf8"
    "zh_CN.utf8"))

(define inferior-package-id
  (@@ (guix inferior) inferior-package-id))

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
         conn revision-commit-hash name version locale)
  (define query "
SELECT translated_package_synopsis.synopsis, translated_package_synopsis.locale,
  translated_package_descriptions.description, translated_package_descriptions.locale,
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
INNER JOIN (
  SELECT DISTINCT ON (package_description_sets.id) package_description_sets.id,
           package_descriptions.description, package_descriptions.locale
  FROM package_descriptions
  INNER JOIN package_description_sets
    ON package_descriptions.id = ANY (package_description_sets.description_ids)
  INNER JOIN package_metadata
    ON package_metadata.package_description_set_id = package_description_sets.id
  INNER JOIN packages
    ON packages.package_metadata_id = package_metadata.id
    AND packages.name = $2
    AND packages.version = $3
  ORDER BY package_description_sets.id,
           CASE WHEN package_descriptions.locale = $4 THEN 2
                WHEN package_descriptions.locale = 'en_US.utf8' THEN 1
                ELSE 0
          END DESC
) AS translated_package_descriptions
  ON package_metadata.package_description_set_id = translated_package_descriptions.id
INNER JOIN (
  SELECT DISTINCT ON (package_synopsis_sets.id) package_synopsis_sets.id,
           package_synopsis.synopsis, package_synopsis.locale
  FROM package_synopsis
  INNER JOIN package_synopsis_sets
    ON package_synopsis.id = ANY (package_synopsis_sets.synopsis_ids)
  INNER JOIN package_metadata
    ON package_metadata.package_synopsis_set_id = package_synopsis_sets.id
  INNER JOIN packages
    ON packages.package_metadata_id = package_metadata.id
    AND packages.name = $2
    AND packages.version = $3
  ORDER BY package_synopsis_sets.id,
           CASE WHEN package_synopsis.locale = $4 THEN 2
                WHEN package_synopsis.locale = 'en_US.utf8' THEN 1
                ELSE 0
          END DESC
) AS translated_package_synopsis
  ON package_metadata.package_synopsis_set_id = translated_package_synopsis.id
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
     ((synopsis synopsis-locale description description-locale home-page file line column-number
                license-json)
      (list synopsis synopsis-locale description description-locale home-page file line column-number
            (if (string-null? license-json)
                #()
                (json-string->scm license-json)))))
   (exec-query conn query (list revision-commit-hash name version locale))))

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

(define (inferior-packages->translated-package-descriptions-and-synopsis inferior
                                                                         inferior-package)

  (define (translate inferior-package-id)
    `(let* ((package (hashv-ref %package-table ,inferior-package-id))
            (source-locale "en_US.utf8")
            (source-synopsis
              (begin
                (setlocale LC_MESSAGES source-locale)
                (P_ (package-synopsis package))))
             (source-description
              (begin
                (setlocale LC_MESSAGES source-locale)
                (P_ (package-description package))))
             (synopsis-by-locale
              (filter-map
               (lambda (locale)
                 (catch 'system-error
                   (lambda ()
                     (setlocale LC_MESSAGES locale))
                   (lambda (key . args)
                     (error
                      (simple-format
                       #f
                       "error changing locale to ~A: ~A ~A"
                       locale key args))))
                 (let ((synopsis
                        (P_ (package-synopsis package))))
                   (setlocale LC_MESSAGES source-locale)
                   (if (string=? synopsis source-synopsis)
                       #f
                       (cons locale synopsis))))
               (list ,@locales)))
             (descriptions-by-locale
              (filter-map
               (lambda (locale)
                 (catch 'system-error
                   (lambda ()
                     (setlocale LC_MESSAGES locale))
                   (lambda (key . args)
                     (error
                      (simple-format
                       #f
                       "error changing locale to ~A: ~A ~A"
                       locale key args))))
                 (let ((description
                        (P_ (package-description package))))
                   (setlocale LC_MESSAGES source-locale)
                   (if (string=? description source-description)
                       #f
                       (cons locale description))))
               (list ,@locales))))
        (cons
         (cons (cons source-locale source-description)
               descriptions-by-locale)
         (cons (cons source-locale source-synopsis)
               synopsis-by-locale))))

  (inferior-eval (translate (inferior-package-id inferior-package)) inferior))

(prevent-inlining-for-tests inferior-packages->translated-package-descriptions-and-synopsis)

(define (package-synopsis-data->package-synopsis-ids
         conn synopsis-by-locale)
  (insert-missing-data-and-return-all-ids
   conn
   "package_synopsis"
   '(locale synopsis)
   (map (match-lambda
          ((locale . synopsis)
           (list locale synopsis)))
        synopsis-by-locale)
   #:delete-duplicates? #t))

(define (insert-package-synopsis-set conn package-synopsis-ids)
  (let ((query
         (string-append
          "INSERT INTO package_synopsis_sets (synopsis_ids) VALUES "
          (string-append
           "('{"
           (string-join
            (map number->string
                 (sort package-synopsis-ids <))
            ", ")
           "}')")
          " RETURNING id")))
    (match (exec-query conn query)
      (((id)) id))))

(define (package-synopsis-data->package-synopsis-set-id
         conn synopsis-by-locale)
  (let* ((package-synopsis-ids
          (package-synopsis-data->package-synopsis-ids
           conn
           synopsis-by-locale))
         (package-synopsis-set-id
          (exec-query
           conn
           (string-append
             "SELECT id FROM package_synopsis_sets"
            " WHERE synopsis_ids = ARRAY["
            (string-join (map number->string
                              (sort package-synopsis-ids <)) ", ")
            "]"))))
    (string->number
     (match package-synopsis-set-id
       (((id)) id)
       (()
        (insert-package-synopsis-set conn package-synopsis-ids))))))

(define (package-description-data->package-description-ids
         conn descriptions-by-locale)
  (insert-missing-data-and-return-all-ids
   conn
   "package_descriptions"
   '(locale description)
   (map (match-lambda
          ((locale . description)
           (list locale description)))
        descriptions-by-locale)
   #:delete-duplicates? #t))

(define (insert-package-description-set conn package-description-ids)
  (let ((query
         (string-append
          "INSERT INTO package_description_sets (description_ids) VALUES "
          (string-append
           "('{"
           (string-join
            (map number->string
                 (sort package-description-ids <))
            ", ")
           "}')")
          " RETURNING id")))
    (match (exec-query conn query)
      (((id)) id))))

(define (package-description-data->package-description-set-id
         conn descriptions-by-locale)
  (let* ((package-description-ids
          (package-description-data->package-description-ids
           conn
           descriptions-by-locale))
         (package-description-set-id
          (exec-query
           conn
           (string-append
             "SELECT id FROM package_description_sets"
            " WHERE description_ids = ARRAY["
            (string-join (map number->string
                              (sort package-description-ids <)) ", ")
            "]"))))
    (string->number
     (match package-description-set-id
       (((id)) id)
       (()
        (insert-package-description-set conn package-description-ids))))))

(define (inferior-packages->package-metadata-ids conn
                                                 inferior
                                                 packages
                                                 license-set-ids)
  (define package-metadata
    (map (lambda (package license-set-id)
           (let ((translated-package-descriptions-and-synopsis
                  (inferior-packages->translated-package-descriptions-and-synopsis
                   inferior package)))
               (list (non-empty-string-or-false
                      (inferior-package-home-page package))
                     (location->location-id
                      conn
                      (inferior-package-location package))
                     license-set-id
                     (package-description-data->package-description-set-id
                      conn
                      (car translated-package-descriptions-and-synopsis))
                     (package-synopsis-data->package-synopsis-set-id
                      conn
                      (cdr translated-package-descriptions-and-synopsis)))))
         packages
         license-set-ids))

  (insert-missing-data-and-return-all-ids
   conn
   "package_metadata"
   '(home_page location_id license_set_id package_description_set_id package_synopsis_set_id)
   (map (match-lambda
          ((home-page location-id license-set-id package_description_set_id package_synopsis_set_id)
           (list (if (string? home-page)
                     home-page
                     NULL)
                 location-id
                 license-set-id
                 package_description_set_id
                 package_synopsis_set_id)))
        package-metadata)
   ;; There can be duplicated entires in package-metadata, for example where
   ;; you have one package definition which interits from another, and just
   ;; overrides the version and the source, the package_metadata entries for
   ;; both definitions will be the same.
   #:delete-duplicates? #t
   ;; There is so much package metadata that it's worth creating a temporary
   ;; table
   #:use-temporary-table? #t))

(define (package-description-and-synopsis-locale-options-guix-revision conn
                                                                       revision-id)
  (exec-query
   conn
   "SELECT DISTINCT coalesce(package_descriptions.locale, package_synopsis.locale)
    FROM package_descriptions
    INNER JOIN package_description_sets
      ON package_descriptions.id = ANY (package_description_sets.description_ids)
    INNER JOIN package_metadata
      ON package_metadata.package_description_set_id = package_description_sets.id
    INNER JOIN package_synopsis_sets
      ON package_synopsis_sets.id = package_metadata.package_synopsis_set_id
    INNER JOIN package_synopsis
      ON package_synopsis.id = ANY (package_synopsis_sets.synopsis_ids)
    INNER JOIN packages
      ON packages.package_metadata_id = package_metadata.id
    INNER JOIN package_derivations
      ON package_derivations.package_id = packages.id
    INNER JOIN guix_revision_package_derivations
      ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
    WHERE guix_revision_package_derivations.revision_id = $1"
   (list revision-id)))

(define (synopsis-counts-by-locale conn revision-id)
  (define synopsis-counts
    "
SELECT package_synopsis.locale, COUNT(package_synopsis.synopsis)  AS translated_synopsis
FROM package_synopsis_sets
INNER JOIN package_synopsis
  ON package_synopsis.id = ANY (package_synopsis_sets.synopsis_ids)
WHERE package_synopsis_sets.id IN (
  SELECT package_metadata.package_synopsis_set_id
  FROM packages
  INNER JOIN package_derivations
    ON packages.id = package_derivations.package_id
  INNER JOIN guix_revision_package_derivations
    ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
  INNER JOIN guix_revisions
    ON guix_revision_package_derivations.revision_id = guix_revisions.id
  INNER JOIN package_metadata
    ON package_metadata.id = packages.package_metadata_id
   WHERE guix_revisions.id = $1)
GROUP BY package_synopsis.locale;
")
  (map
   (match-lambda
     ((locale synopsis-counts)
      `(,locale . ,(string->number synopsis-counts))))
   (exec-query conn synopsis-counts
               (list revision-id))))

(define (description-counts-by-locale conn revision-id)
  (define description-counts
    "
SELECT package_descriptions.locale, COUNT(package_descriptions.description)  AS translated_description
FROM package_description_sets
INNER JOIN package_descriptions
  ON package_descriptions.id = ANY (package_description_sets.description_ids)
 WHERE package_description_sets.id IN (
  SELECT package_metadata.package_description_set_id
  FROM packages
  INNER JOIN package_derivations
    ON packages.id = package_derivations.package_id
  INNER JOIN guix_revision_package_derivations
    ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
  INNER JOIN guix_revisions
    ON guix_revision_package_derivations.revision_id = guix_revisions.id
  INNER JOIN package_metadata
    ON package_metadata.id = packages.package_metadata_id
   WHERE guix_revisions.id = $1)
GROUP BY package_descriptions.locale;
")
  (map
   (match-lambda
     ((locale description-counts)
      `(,locale . ,(string->number description-counts))))
   (exec-query conn description-counts
               (list revision-id))))
