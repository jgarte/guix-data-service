;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2020 Christopher Baines <mail@cbaines.net>
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

(define-module (guix-data-service data-deletion)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix-data-service database)
  #:export (delete-data-for-branch))

(define (delete-data-for-branch conn git-repository-id branch-name)
  (define commits
    (map car
         (exec-query conn
                     "
SELECT commit
FROM git_branches
WHERE git_repository_id = $1 AND name = $2"
                     (list (number->string git-repository-id)
                           branch-name))))


  (with-postgresql-transaction
   conn
   (lambda (conn)
     (exec-query
      conn
      (simple-format
       #f
       "
DELETE FROM git_branches
WHERE git_repository_id = ~A AND
  name = '~A' AND
  commit IN (~A)"
       git-repository-id
       branch-name
       (string-join
        (map (lambda (commit)
               (string-append "'" commit "'"))
             commits)
        ", ")))

     (for-each
      (lambda (table)
        (exec-query
         conn
         (simple-format
          #f
          "
DELETE FROM ~A
WHERE branch_name = $1 AND git_repository_id = $2"
          table)
         (list branch-name
               (number->string git-repository-id))))
      '("package_versions_by_guix_revision_range"
        "package_derivations_by_guix_revision_range"))

     (for-each
      (lambda (table)
        (exec-query
         conn
         (string-append
          "
DELETE FROM " table "
WHERE job_id IN (
  SELECT id
  FROM load_new_guix_revision_jobs
  WHERE git_repository_id = " (number->string git-repository-id) " AND
    commit IN ("
  (string-join
   (map (lambda (commit)
          (string-append "'" commit "'"))
        commits)
   ", ")
  ")
)")))
      '("load_new_guix_revision_job_events"
        "load_new_guix_revision_job_logs"))

     (exec-query
      conn
      (string-append
       "
DELETE FROM load_new_guix_revision_jobs
WHERE git_repository_id = " (number->string git-repository-id) " AND
  commit IN ("
      (string-join
       (map (lambda (commit)
              (string-append "'" commit "'"))
            commits)
       ", ")
      ")"))

     (let ((guix-revision-ids
            (map
             car
             (exec-query
              conn
              (string-append
               "
SELECT guix_revisions.id
FROM (VALUES "
               (string-join
                (map (lambda (commit)
                       (string-append "('" commit "')"))
                     commits)
                ", ")
               ") AS commits
INNER JOIN guix_revisions
  ON guix_revisions.commit = commits.column1
WHERE guix_revisions.git_repository_id = "
               (number->string git-repository-id) " AND
  commits.column1 NOT IN (
    SELECT commit
    FROM git_branches
)")))))

       (unless (null? guix-revision-ids)
         (for-each
          (lambda (table)
            (exec-query
             conn
             (simple-format
              #f
              "
DELETE FROM ~A WHERE ~A IN (VALUES ~A)"
              table
              (if (string=? table
                            "guix_revision_package_derivations")
                  "revision_id"
                  "guix_revision_id")
              (string-join
               (map (lambda (guix-revision-id)
                      (string-append "(" guix-revision-id ")"))
                    guix-revision-ids)
               ", "))))
          '("channel_instances"
            "guix_revision_channel_news_entries"
            "guix_revision_lint_checkers"
            "guix_revision_lint_warnings"
            "guix_revision_package_derivations"
            "guix_revision_system_test_derivations"))

         (exec-query
          conn
          (string-append
           "
DELETE FROM guix_revisions
WHERE id IN ("
           (string-join guix-revision-ids ", ")
           ")")))))))

(define (delete-data-for-all-branches-but-master)
  (with-postgresql-connection
   "data-deletion"
   (lambda (conn)
     (for-each
      (lambda (branch-name)
        (delete-data-for-branch conn 1 branch-name))
      (map
       car
       (exec-query
        conn
        "
SELECT DISTINCT name
FROM git_branches
WHERE git_repository_id = 1 AND name != 'master'"))))))

(define (delete-unreferenced-derivations)
  (define (maybe-delete-derivation conn id file-name)
    (match (map
            car
            (exec-query
             conn
             "
DELETE FROM derivation_outputs WHERE derivation_id = $1
AND NOT EXISTS (
  SELECT 1
  FROM derivation_inputs
  WHERE derivation_output_id IN (
    SELECT derivation_outputs.id
    FROM derivation_outputs
    WHERE derivation_id = $1
  )
) AND NOT EXISTS (
  SELECT 1
  FROM package_derivations
  WHERE package_derivations.derivation_id = derivation_outputs.derivation_id
) AND NOT EXISTS (
  SELECT 1 FROM channel_instances
  WHERE derivation_id = $1
) AND NOT EXISTS (
  SELECT 1 FROM guix_revision_system_test_derivations
  WHERE derivation_id = $1
)
RETURNING derivation_outputs.derivation_output_details_id"
             (list id)))
      (() 0)
      ((derivation-output-details-ids ...)

       (for-each
        (lambda (derivation-output-details-id)
          (match (exec-query
                  conn
                  "
SELECT COUNT(*) FROM derivation_outputs
WHERE derivation_output_details_id = $1"
                  (list derivation-output-details-id))
            (((count))
             (when (eq? (string->number count)
                        0)
               (exec-query
                conn
                "
DELETE FROM derivation_output_details
WHERE id = $1"
                (list derivation-output-details-id))))))
        derivation-output-details-ids)

       (exec-query
        conn
        "
DELETE FROM derivation_sources WHERE derivation_id = $1"
        (list id))

       (match (exec-query
               conn
               "
SELECT derivation_output_details_set_id
FROM derivations_by_output_details_set
WHERE derivation_id = $1"
               (list id))
         (((derivation-output-details-set-id))
          (match (exec-query
                  conn
                  "
SELECT COUNT(*) FROM derivations_by_output_details_set
WHERE derivation_output_details_set_id = $1"
                  (list derivation-output-details-set-id))
            (((count))
             (exec-query
              conn
              "
DELETE FROM derivations_by_output_details_set
WHERE derivation_id = $1"
              (list id))

             (when (<= (string->number count)
                       1)
               (exec-query
                conn
                "
DELETE FROM derivation_output_details_sets
WHERE id = $1"
                (list derivation-output-details-set-id)))))))

       (let ((input-derivations
              (exec-query
               conn
               "
SELECT DISTINCT derivations.id, derivations.file_name
FROM derivations
WHERE derivations.id IN (
  SELECT derivation_outputs.derivation_id
  FROM derivation_outputs
  INNER JOIN derivation_inputs
    ON derivation_outputs.id = derivation_inputs.derivation_output_id
  WHERE derivation_inputs.derivation_id = $1
)"
               (list id))))

         (exec-query
          conn
          "
DELETE FROM derivation_inputs WHERE derivation_id = $1"
          (list id))

         (exec-query
          conn
          "
DELETE FROM derivations WHERE id = $1"
          (list id))

         ;; Look at the inputs to see if they can be deleted too, as one of
         ;; the derivations that was using them has now been deleted.
         (fold
          (match-lambda*
            (((id file-name) result)
             (+ result
                (maybe-delete-derivation conn id file-name))))
          1
          input-derivations)))))

  (with-postgresql-connection
   "data-deletion"
   (lambda (conn)
     (define (delete-batch conn)
       (let* ((derivations
               (exec-query
                conn
                "
SELECT id, file_name
FROM derivations
LIMIT 10000000"))
              (derivations-count (length derivations)))
         (simple-format (current-error-port)
                        "Looking at ~A derivations\n"
                        derivations-count)
         (let ((deleted-count
                (fold
                 (match-lambda*
                   (((id file-name) index result)
                    (when (eq? 0 (modulo index 50000))
                      (simple-format #t "~A/~A (~A%)  (deleted ~A so far)\n"
                                     index derivations-count
                                     (exact->inexact
                                      (rationalize
                                       (* 100 (/ index derivations-count))
                                       1))
                                     result))
                    (+ result
                       (with-postgresql-transaction
                        conn
                        (lambda (conn)
                          (exec-query
                           conn
                           "
SET CONSTRAINTS derivations_by_output_details_set_derivation_id_fkey DEFERRED")

                          (maybe-delete-derivation conn id file-name))))))
                 0
                 derivations
                 (iota derivations-count))))
           (simple-format (current-error-port)
                          "Deleted ~A derivations\n"
                          deleted-count)
           deleted-count)))

     (let loop ((total-deleted 0))
       (let ((batch-deleted-count (delete-batch conn)))
         (if (eq? 0 batch-deleted-count)
             (simple-format
              (current-output-port)
              "Finished deleting derivations, deleted ~A in total\n"
              total-deleted)
             (loop (+ total-deleted batch-deleted-count))))))))
