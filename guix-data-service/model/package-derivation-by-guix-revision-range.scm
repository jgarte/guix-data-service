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
  #:export (update-package-derivations-table))

(define (log-time action f)
  (simple-format #t "debug: Starting ~A\n" action)
  (let* ((start-time (current-time))
         (result (f))
         (time-taken (- (current-time) start-time)))
    (simple-format #t "debug: Finished ~A, took ~A seconds\n"
                   action time-taken)
    result))

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
         guix-revision-id
         branch-name)))

(define (insert-guix-revision-package-derivation-entries conn
                                                         git-repository-id
                                                         guix-revision-id
                                                         branch-name)
  (exec-query
   conn
   "
INSERT INTO package_derivations_by_guix_revision_range
SELECT DISTINCT
       $1::integer AS git_repository_id,
       $2 AS branch_name,
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
  SELECT DISTINCT package_derivations.package_id,
                  package_derivations.derivation_id,
                  package_derivations.system,
                  package_derivations.target,
                  guix_revision_package_derivations.revision_id
  FROM package_derivations
  INNER JOIN guix_revision_package_derivations
    ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
) AS revision_packages ON packages.id = revision_packages.package_id
INNER JOIN guix_revisions ON revision_packages.revision_id = guix_revisions.id
INNER JOIN git_branches ON guix_revisions.commit = git_branches.commit
WHERE git_branches.name = $2 AND
      revision_packages.derivation_id IN (
        SELECT package_derivations.derivation_id
        FROM package_derivations
        INNER JOIN guix_revision_package_derivations
          ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
        WHERE revision_id = $3
      )
WINDOW package_version AS (
  PARTITION BY packages.name, packages.version, revision_packages.derivation_id
  ORDER BY git_branches.datetime
  RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
)
ORDER BY packages.name, packages.version"
   (list git-repository-id
         branch-name
         guix-revision-id)))

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
      (log-time
       (simple-format #f "deleting package derivation entries for ~A" branch-name)
       (lambda ()
         (delete-guix-revision-package-derivation-entries conn
                                                          git-repository-id
                                                          guix-revision-id
                                                          branch-name)))
      (log-time
       (simple-format #f "inserting package derivation entries for ~A" branch-name)
       (lambda ()
         (insert-guix-revision-package-derivation-entries conn
                                                          git-repository-id
                                                          guix-revision-id
                                                          branch-name)))))
   (exec-query
    conn
    "SELECT name FROM git_branches WHERE commit = $1 AND git_repository_id = $2"
    (list commit git-repository-id)))

  #t)
