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
  #:use-module (squee)
  #:use-module (guix-data-service model utils)
  #:export (lint-warnings-data->lint-warning-ids
            insert-guix-revision-lint-warnings
            lint-warnings-for-guix-revision
            select-lint-warnings-by-revision-package-name-and-version))

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
     ", "))))

(define* (lint-warnings-for-guix-revision conn commit-hash
                                          #:key
                                          locale
                                          package-query
                                          linters
                                          message-query)
  (define query
    (string-append "
SELECT lint_warnings.id, lint_checkers.name, lint_checker_descriptions.description,
       lint_checkers.network_dependent, packages.name, packages.version,
       locations.file, locations.line, locations.column_number,
       lint_warning_messages.message
FROM lint_warnings
INNER JOIN lint_checkers
  ON lint_warnings.lint_checker_id = lint_checkers.id
INNER JOIN lint_checker_description_sets
  ON lint_checkers.lint_checker_description_set_id = lint_checker_description_sets.id
INNER JOIN lint_checker_descriptions
  ON lint_checker_descriptions.id = ANY (lint_checker_description_sets.description_ids)
INNER JOIN packages
  ON lint_warnings.package_id = packages.id
INNER JOIN locations
  ON locations.id = lint_warnings.location_id
INNER JOIN lint_warning_message_sets
  ON lint_warning_message_sets.id = lint_warning_message_set_id
INNER JOIN lint_warning_messages
  ON lint_warning_messages.locale = "
                   "'" locale "'"
" AND lint_warning_messages.id = ANY (lint_warning_message_sets.message_ids)
"
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
                       " AND to_tsvector(packages.name) @@ plainto_tsquery($2)"
                       "")
                   (if message-query
                       (simple-format
                        #f " AND to_tsvector(lint_warning_messages.message) @@ plainto_tsquery($~A)"
                        (if package-query "3" "2"))
                       "")
                   " ORDER BY packages.name, packages.version, lint_checkers.name, lint_warnings.id"))

  (exec-query conn query `(,commit-hash
                           ,@(if package-query
                                 (list package-query)
                                 '())
                           ,@(if message-query
                                 (list message-query)
                                 '()))))

(define (select-lint-warnings-by-revision-package-name-and-version conn
                                                                   commit-hash
                                                                   name version)
  (define query "
SELECT lint_warnings.id, lint_checkers.name, lint_checker_descriptions.description,
       lint_checkers.network_dependent,
       locations.file, locations.line, locations.column_number,
       lint_warning_messages.message
FROM lint_warnings
INNER JOIN lint_checkers
  ON lint_checkers.id = lint_warnings.lint_checker_id
INNER JOIN lint_checker_description_sets
  ON lint_checkers.lint_checker_description_set_id = lint_checker_description_sets.id
INNER JOIN lint_checker_descriptions
  ON lint_checker_descriptions.id = ANY (lint_checker_description_sets.description_ids)
INNER JOIN packages
  ON lint_warnings.package_id = packages.id
LEFT OUTER JOIN locations
  ON lint_warnings.location_id = locations.id
INNER JOIN lint_warning_message_sets
  ON lint_warning_message_sets.id = lint_warning_message_set_id
INNER JOIN lint_warning_messages
  ON lint_warning_messages.locale = 'en_US.utf8'
  AND lint_warning_messages.id = ANY (lint_warning_message_sets.message_ids)
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

  (exec-query conn
              query
              (list commit-hash name version)))
