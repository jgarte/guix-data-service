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

(define-module (guix-data-service model package-derivation-by-guix-revision-range)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service utils)
  #:export (delete-guix-revision-package-derivation-entries
            insert-guix-revision-package-derivation-entries
            update-package-derivations-table
            rebuild-package-derivations-table))

(define (delete-guix-revision-package-derivation-entries conn
                                                         git-repository-id
                                                         guix-revision-id
                                                         branch-name)
  (exec-query
   conn
   "
DELETE FROM package_derivations_by_guix_revision_range
WHERE git_repository_id = $1 AND
      branch_name = $2 AND
      derivation_id IN (
        SELECT package_derivations.derivation_id
        FROM package_derivations
        INNER JOIN guix_revision_package_derivations
          ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
        WHERE revision_id = $3
      )"
   (list git-repository-id
         branch-name
         guix-revision-id)))

(define* (insert-guix-revision-package-derivation-entries conn
                                                          git-repository-id
                                                          branch-name
                                                          #:key guix-revision-id)
  (define query
    (string-append
     "
INSERT INTO package_derivations_by_guix_revision_range
SELECT DISTINCT
       git_branches.git_repository_id,
       git_branches.name AS branch_name,
       packages.name AS package_name,
       packages.version AS package_version,
       revision_packages.derivation_id AS derivation_id,
       revision_packages.system AS system,
       revision_packages.target AS target,
       first_value(guix_revisions.id)
         OVER package_version AS first_guix_revision_id,
       last_value(guix_revisions.id)
         OVER package_version AS last_guix_revision_id
FROM packages
INNER JOIN (
  SELECT package_derivations.package_id,
         package_derivations.derivation_id,
         package_derivations.system,
         package_derivations.target,
         guix_revision_package_derivations.revision_id
  FROM package_derivations
  INNER JOIN guix_revision_package_derivations
    ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
) AS revision_packages ON packages.id = revision_packages.package_id
INNER JOIN guix_revisions
  ON guix_revisions.git_repository_id = $1
 AND revision_packages.revision_id = guix_revisions.id
INNER JOIN git_branches
  ON git_branches.name = $2
 AND guix_revisions.commit = git_branches.commit
"
     (if guix-revision-id
         "WHERE
      revision_packages.derivation_id IN (
        SELECT package_derivations.derivation_id
        FROM package_derivations
        INNER JOIN guix_revision_package_derivations
          ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
        WHERE revision_id = $3
      )"
         "")
     "
WINDOW package_version AS (
  PARTITION BY git_branches.git_repository_id, git_branches.name,
               packages.name, packages.version, revision_packages.derivation_id
  ORDER BY git_branches.datetime
  RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
)
ORDER BY packages.name, packages.version"))

  (exec-query
   conn
   query
   `(,git-repository-id
     ,branch-name
     ,@(if guix-revision-id
           (list guix-revision-id)
           '()))))

(define (update-package-derivations-table conn
                                          git-repository-id
                                          guix-revision-id
                                          commit)
  ;; Lock the table to wait for other transactions to commit before updating
  ;; the table
  (exec-query
   conn
   "
LOCK TABLE ONLY package_derivations_by_guix_revision_range
  IN SHARE ROW EXCLUSIVE MODE")

  (for-each
   (match-lambda
     ((branch-name)
      (with-time-logging
          (simple-format #f "deleting package derivation entries for ~A"
                         branch-name)
        (delete-guix-revision-package-derivation-entries conn
                                                         git-repository-id
                                                         guix-revision-id
                                                         branch-name))
      (with-time-logging
          (simple-format #f "inserting package derivation entries for ~A"
                         branch-name)
        (insert-guix-revision-package-derivation-entries
         conn
         git-repository-id
         branch-name
         #:guix-revision-id guix-revision-id))))
   (exec-query
    conn
    "SELECT name FROM git_branches WHERE commit = $1 AND git_repository_id = $2"
    (list commit git-repository-id)))

  #t)

(define (rebuild-package-derivations-table conn)
  (with-postgresql-transaction
   conn
   (lambda (conn)
     ;; Lock the table to wait for other transactions to commit before updating
     ;; the table
     (exec-query
      conn
      "
LOCK TABLE ONLY package_derivations_by_guix_revision_range
  IN SHARE ROW EXCLUSIVE MODE")

     (with-time-logging
         (simple-format #f "deleting all package derivation entries")
       (exec-query conn "DELETE FROM package_derivations_by_guix_revision_range"))

     (let ((git-branches-and-repository-ids
            (exec-query
             conn
             "SELECT DISTINCT name, git_repository_id FROM git_branches")))
       (for-each
        (match-lambda
          ((branch-name git-repository-id)
           (with-time-logging
               (simple-format #f "inserting package derivation entries for ~A"
                              branch-name)
             (insert-guix-revision-package-derivation-entries
              conn
              git-repository-id
              branch-name))))
        git-branches-and-repository-ids)))))
