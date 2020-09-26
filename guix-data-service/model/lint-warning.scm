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

(define-module (guix-data-service model lint-warning)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (squee)
  #:use-module (guix-data-service model utils)
  #:export (lint-warnings-data->lint-warning-ids
            insert-guix-revision-lint-warnings
            lint-warnings-for-guix-revision
            select-lint-warnings-by-revision-package-name-and-version

            any-translated-lint-warnings?))

(define (lint-warnings-data->lint-warning-ids
         conn
         ;; (lint-checker-id package-id location-id lint-warning-message-set-id)
         lint-warnings-data)
  (insert-missing-data-and-return-all-ids
   conn
   "lint_warnings"
   '(lint_checker_id package_id location_id lint_warning_message_set_id)
   lint-warnings-data))

(define (insert-guix-revision-lint-warnings conn
                                            guix-revision-id
                                            lint-warning-ids)
  (if (null? lint-warning-ids)
      '()
      (exec-query
       conn
       (string-append
        "INSERT INTO guix_revision_lint_warnings (lint_warning_id, guix_revision_id) "
        "VALUES "
        (string-join
         (map (lambda (lint-warning-id)
                (simple-format
                 #f
                 "(~A, ~A)"
                 lint-warning-id
                 guix-revision-id))
              lint-warning-ids)
         ", ")))))

(define* (lint-warnings-for-guix-revision conn commit-hash
                                          #:key
                                          locale
                                          package-query
                                          linters
                                          message-query)
  (define query
    (string-append "
SELECT DISTINCT ON (lint_warnings.id) lint_warnings.id,
            lint_checkers.name, translated_lint_checker_descriptions.description,
            translated_lint_checker_descriptions.locale, lint_checkers.network_dependent,
            packages.name, packages.version,
            locations.file, locations.line, locations.column_number,
            lint_warning_messages.message, lint_warning_messages.locale
FROM lint_warnings
INNER JOIN lint_checkers
  ON lint_warnings.lint_checker_id = lint_checkers.id
INNER JOIN (
  SELECT DISTINCT ON (lint_checkers.id) lint_checkers.id AS lint_checker_id,
              lint_checker_descriptions.description, lint_checker_descriptions.locale
  FROM guix_revision_lint_checkers
  INNER JOIN guix_revisions
    ON guix_revision_lint_checkers.guix_revision_id = guix_revisions.id
  INNER JOIN lint_checkers
    ON guix_revision_lint_checkers.lint_checker_id = lint_checkers.id
  INNER JOIN lint_checker_description_sets
    ON lint_checkers.lint_checker_description_set_id = lint_checker_description_sets.id
  INNER JOIN lint_checker_descriptions
    ON lint_checker_descriptions.id = ANY (lint_checker_description_sets.description_ids)
  WHERE guix_revisions.commit = $1
  ORDER BY lint_checkers.id,
           CASE
             WHEN lint_checker_descriptions.locale = $2 THEN 2
             WHEN lint_checker_descriptions.locale = 'en_US.UTF-8' THEN 1
             ELSE 0
           END DESC
) AS translated_lint_checker_descriptions
  ON translated_lint_checker_descriptions.lint_checker_id = lint_checkers.id
INNER JOIN packages
  ON lint_warnings.package_id = packages.id
INNER JOIN locations
  ON locations.id = lint_warnings.location_id
INNER JOIN lint_warning_message_sets
  ON lint_warning_message_sets.id = lint_warning_message_set_id
INNER JOIN lint_warning_messages
  ON lint_warning_messages.id = ANY (lint_warning_message_sets.message_ids)"
                   (if linters
                       (string-append
                        "INNER JOIN (VALUES "
                        (string-join
                         (map (lambda (lint-checker-name)
                                (simple-format
                                 #f "($STR$~A$STR$)" lint-checker-name))
                              linters)
                         ",")
                        ") AS linters (name) ON lint_checkers.name = linters.name ")
                       "")
"WHERE lint_warnings.id IN (
  SELECT lint_warning_id
  FROM guix_revision_lint_warnings
  INNER JOIN guix_revisions ON guix_revision_id = guix_revisions.id
  WHERE commit = $1
)"
(if package-query
                       " AND to_tsvector(packages.name) @@ plainto_tsquery($3)"
                       "")
                   (if message-query
                       (simple-format
                        #f " AND to_tsvector(lint_warning_messages.message) @@ plainto_tsquery($~A)"
                        (if package-query "4" "3"))
                       "")
                   " ORDER BY lint_warnings.id,
                              CASE
                                WHEN lint_warning_messages.locale = $2 THEN 2
                                WHEN lint_warning_messages.locale = 'en_US.UTF-8' THEN 1
                                ELSE 0
                              END DESC"))

  (exec-query conn query `(,commit-hash
                           ,locale
                           ,@(if package-query
                                 (list package-query)
                                 '())
                           ,@(if message-query
                                 (list message-query)
                                 '()))))

(define* (select-lint-warnings-by-revision-package-name-and-version conn
                                                                   commit-hash
                                                                   name version
                                                                   #:key
                                                                   locale)
  (define query
"SELECT DISTINCT ON (lint_warnings.id) lint_warnings.id,
       lint_checkers.name, translated_lint_checker_descriptions.description,
       lint_checkers.network_dependent,
       locations.file, locations.line, locations.column_number,
       lint_warning_messages.message
FROM lint_warnings
INNER JOIN lint_checkers
  ON lint_checkers.id = lint_warnings.lint_checker_id
INNER JOIN (
  SELECT DISTINCT ON (lint_checkers.id) lint_checkers.id AS lint_checker_id,
              lint_checker_descriptions.description
  FROM lint_checkers
  INNER JOIN lint_checker_description_sets
    ON lint_checkers.lint_checker_description_set_id = lint_checker_description_sets.id
  INNER JOIN lint_checker_descriptions
    ON lint_checker_descriptions.id = ANY (lint_checker_description_sets.description_ids)
  INNER JOIN guix_revision_lint_checkers
    ON guix_revision_lint_checkers.lint_checker_id = lint_checkers.id
  INNER JOIN guix_revisions
    ON guix_revisions.id = guix_revision_lint_checkers.guix_revision_id
    AND guix_revisions.commit = $1
  ORDER BY lint_checkers.id,
           CASE
             WHEN lint_checker_descriptions.locale = $4 THEN 2
             WHEN lint_checker_descriptions.locale = 'en_US.UTF-8' THEN 1
             ELSE 0
           END DESC
) AS translated_lint_checker_descriptions
  ON translated_lint_checker_descriptions.lint_checker_id = lint_checkers.id
INNER JOIN packages
  ON lint_warnings.package_id = packages.id
LEFT OUTER JOIN locations
  ON lint_warnings.location_id = locations.id
INNER JOIN lint_warning_message_sets
  ON lint_warning_message_sets.id = lint_warning_message_set_id
INNER JOIN lint_warning_messages
  ON lint_warning_messages.id = ANY (lint_warning_message_sets.message_ids)
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
  AND packages.version = $3
  ORDER BY lint_warnings.id,
           CASE
             WHEN lint_warning_messages.locale = $4 THEN 2
             WHEN lint_warning_messages.locale = 'en_US.UTF-8' THEN 1
             ELSE 0
           END DESC
")

  (exec-query conn
              query
              (list commit-hash name version locale)))

(define (any-translated-lint-warnings? lint-warnings-data locale)
  (any
   (match-lambda
     ((lint-warnings-id lint-checker-name lint-checker-description
                        description-locale network-dependent package-name
                        packages-version file line column message message-locale)
      (or (string=? description-locale locale)
          (string=? message-locale locale))))
   lint-warnings-data))
