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

(with-postgresql-connection "foo" (lambda (conn) (for-each (lambda (branch-name) (delete-data-for-branch conn 1 branch-name)) (map car (exec-query conn "SELECT DISTINCT name FROM git_branches WHERE git_repository_id = 1 AND name != 'master'")))))
