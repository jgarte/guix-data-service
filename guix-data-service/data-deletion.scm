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
  #:use-module (guix-data-service utils)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model package-derivation-by-guix-revision-range)
  #:export (delete-data-for-branch
            delete-revisions-from-branch-except-most-recent-n
            delete-revisions-for-all-branches-except-most-recent-n
            delete-data-for-all-deleted-branches
            delete-unreferenced-derivations))

(define (delete-revisions-from-branch conn git-repository-id branch-name commits)
  (define (delete-jobs conn)
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
")")))

  (define (delete-from-git-branches conn)
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
       ", "))))

  (with-postgresql-transaction
   conn
   (lambda (conn)
     (delete-from-git-branches conn)
     (delete-jobs conn)

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

       (exec-query
        conn
        "
DELETE FROM package_derivations_by_guix_revision_range
WHERE git_repository_id = $1 AND
      branch_name = $2"
        (list (number->string git-repository-id)
              branch-name))

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
           ")
AND id NOT IN (
  SELECT id FROM guix_revisions
  INNER JOIN git_branches ON
    git_branches.commit = guix_revisions.commit AND
    git_branches.git_repository_id = guix_revisions.git_repository_id
)")))))))

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

  (delete-revisions-from-branch conn
                                git-repository-id
                                branch-name
                                commits))

(define (delete-revisions-from-branch-except-most-recent-n conn
                                                           git-repository-id
                                                           branch-name
                                                           n)
  (define commits
    (map car
         (exec-query conn
                     "
SELECT commit
FROM git_branches
WHERE git_repository_id = $1 AND name = $2
ORDER BY datetime DESC
OFFSET $3"
                     (list (number->string git-repository-id)
                           branch-name
                           (number->string n)))))

  (unless (null? commits)
    (simple-format #t "deleting ~A commits from ~A\n" (length commits) branch-name)
    (delete-revisions-from-branch conn
                                  git-repository-id
                                  branch-name
                                  commits)

    (simple-format #t "repopulating package_derivations_by_guix_revision_range\n")
    (insert-guix-revision-package-derivation-entries conn
                                                     (number->string
                                                      git-repository-id)
                                                     branch-name)))

(define (delete-revisions-for-all-branches-except-most-recent-n n)
  (with-postgresql-connection
   "data-deletion"
   (lambda (conn)
     (for-each
      (match-lambda
        ((git-repository-id branch-name)
         (delete-revisions-from-branch-except-most-recent-n
          conn
          (string->number git-repository-id)
          branch-name
          n)))
      (exec-query
       conn
       "
SELECT DISTINCT git_repository_id, name
FROM git_branches")))))

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

(define (delete-data-for-all-deleted-branches)
  (with-postgresql-connection
   "data-deletion"
   (lambda (conn)
     (for-each
      (match-lambda
        ((name git-repository-id)
         (simple-format #t "deleting data for ~A (~A)\n"
                        name git-repository-id)
         (delete-data-for-branch conn
                                 (string->number git-repository-id)
                                 name)))
      (exec-query
       conn
       "
SELECT name, git_repository_id
FROM (
  SELECT DISTINCT ON (name, git_repository_id)
    name, git_repository_id, commit
  FROM git_branches
  ORDER BY git_repository_id, name, datetime DESC
) AS git_branches_latest_revision
WHERE commit = ''")))))

(define (delete-unreferenced-derivations)
  (define (delete-builds-for-derivation-output-details-set
           conn
           derivation-output-details-set-id)
    (let ((build-ids
           (map car
                (exec-query
                 conn
                 "
SELECT id
FROM builds
WHERE derivation_output_details_set_id = $1"
                 (list derivation-output-details-set-id)))))

      (unless (null? build-ids)
        (exec-query
         conn
         (string-append
          "
DELETE FROM build_status WHERE build_id IN ("
          (string-join build-ids ",")
          ")"))

        (exec-query
         conn
         (string-append
          "
DELETE FROM builds WHERE id IN ("
          (string-join build-ids ",")
          ")")))))

  (define (maybe-delete-derivation conn id)
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
               (delete-builds-for-derivation-output-details-set
                conn
                derivation-output-details-set-id)

               (exec-query
                conn
                "
DELETE FROM derivation_output_details_sets
WHERE id = $1"
                (list derivation-output-details-set-id)))))))

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

       1)))

  (define conn-channel
    (make-postgresql-connection-channel
     "data-deletion-thread"))

  (with-postgresql-connection
   "data-deletion"
   (lambda (conn)
     (define (delete-batch conn)
       (let* ((derivations
               (map car
                    (exec-query
                     conn
                     "
SELECT DISTINCT derivation_id
FROM derivation_outputs
WHERE NOT EXISTS (
  -- This isn't a perfect check, as this will select some derivations that are
  -- used, but maybe-delete-derivation includes the proper check
  SELECT 1
  FROM derivation_inputs
  WHERE derivation_output_id = derivation_outputs.id
) AND NOT EXISTS (
  SELECT 1
  FROM package_derivations
  WHERE package_derivations.derivation_id = derivation_outputs.derivation_id
) AND NOT EXISTS (
  SELECT 1 FROM channel_instances
  WHERE derivation_id = derivation_outputs.derivation_id
) AND NOT EXISTS (
  SELECT 1 FROM guix_revision_system_test_derivations
  WHERE derivation_id = derivation_outputs.derivation_id
) LIMIT 10000000")))
              (derivations-count (length derivations)))
         (simple-format (current-error-port)
                        "Looking at ~A derivations\n"
                        derivations-count)
         (let ((deleted-count
                (fold
                 (lambda (id result)
                   (+ result
                      (with-postgresql-transaction/through-channel
                       conn-channel
                       (lambda (conn)
                         (exec-query
                          conn
                          "
SET CONSTRAINTS derivations_by_output_details_set_derivation_id_fkey DEFERRED")

                         (maybe-delete-derivation conn id)))))
                 0
                 derivations)))
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
