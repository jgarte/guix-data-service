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

(define-module (guix-data-service model git-branch)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (squee)
  #:use-module (srfi srfi-19)
  #:use-module (guix-data-service model utils)
  #:export (insert-git-branch-entry
            git-branches-for-commit
            git-branches-with-repository-details-for-commit
            most-recent-commits-for-branch
            latest-processed-commit-for-branch
            all-branches-with-most-recent-commit))

(define (insert-git-branch-entry conn
                                 name commit
                                 git-repository-id datetime)
  (exec-query
   conn
   (string-append
    "INSERT INTO git_branches (name, commit, git_repository_id, datetime) "
    "VALUES ($1, $2, $3, to_timestamp($4)) "
    "ON CONFLICT DO NOTHING")
   (list name
         commit
         (number->string git-repository-id)
         (date->string datetime "~s"))))

(define (git-branches-for-commit conn commit)
  (define query
    "
SELECT name, datetime FROM git_branches WHERE commit = $1
ORDER BY datetime DESC")

  (exec-query conn query (list commit)))

(define (git-branches-with-repository-details-for-commit conn commit)
  (define query
    "
SELECT git_repositories.id, git_repositories.label,
       git_repositories.url, git_repositories.cgit_url_base,
       git_branches.name, git_branches.datetime
FROM git_branches
INNER JOIN git_repositories
  ON git_branches.git_repository_id = git_repositories.id
WHERE git_branches.commit = $1")

  (group-list-by-first-n-fields
   4
   (exec-query conn query (list commit))))

(define* (most-recent-commits-for-branch conn git-repository-id
                                         branch-name
                                         #:key
                                         (limit 100)
                                         after-date
                                         before-date)
  (define query
    (string-append
     "SELECT git_branches.commit, datetime, "
     "(guix_revisions.id IS NOT NULL) as guix_revision_exists, "
     "(
        SELECT json_agg(event)
        FROM load_new_guix_revision_job_events
        INNER JOIN load_new_guix_revision_jobs ON
          load_new_guix_revision_jobs.id = load_new_guix_revision_job_events.job_id
        WHERE load_new_guix_revision_jobs.commit = git_branches.commit AND
              git_branches.git_repository_id = load_new_guix_revision_jobs.git_repository_id
      ) AS job_events "
     "FROM git_branches "
     "LEFT OUTER JOIN guix_revisions ON git_branches.commit = guix_revisions.commit "
     "WHERE name = $1 AND git_branches.git_repository_id = $2"
     (if after-date
         (simple-format #f " AND datetime > '~A'"
                        (date->string after-date "~1 ~3"))
         "")
     (if before-date
         (simple-format #f " AND datetime < '~A'"
                        (date->string before-date "~1 ~3"))
         "")
     "ORDER BY datetime DESC"
     (if limit
         (simple-format #f " LIMIT ~A;" limit)
         "")))

  (map
   (match-lambda
     ((commit datetime guix_revision_exists job_events)
      (list commit
            datetime
            (string=? guix_revision_exists "t")
            (if (or (and (string? job_events) (string-null? job_events))
                    (eq? #f job_events))
                '()
                (vector->list (json-string->scm job_events))))))
   (exec-query
    conn
    query
    (list branch-name
          (number->string git-repository-id)))))

(define* (latest-processed-commit-for-branch conn repository-id branch-name)
  (define query
    (string-append
     "
SELECT git_branches.commit
FROM git_branches
INNER JOIN guix_revisions
  ON git_branches.commit = guix_revisions.commit
INNER JOIN load_new_guix_revision_jobs
  ON load_new_guix_revision_jobs.commit = guix_revisions.commit
INNER JOIN load_new_guix_revision_job_events
  ON job_id = load_new_guix_revision_jobs.id
WHERE guix_revisions.git_repository_id = $1
  AND git_branches.git_repository_id = $1
  AND git_branches.name = $2
  AND load_new_guix_revision_job_events.event = 'success'
ORDER BY datetime DESC
LIMIT 1"))

  (match (exec-query
          conn
          query
          (list repository-id branch-name))
    (((commit-hash))
     commit-hash)
    ('()
     #f)))

(define (all-branches-with-most-recent-commit conn git-repository-id)
  (define query
    (string-append
     "
SELECT DISTINCT ON (name)
  name, git_branches.commit,
  datetime, (guix_revisions.id IS NOT NULL) guix_revision_exists,
  (
    SELECT json_agg(event)
    FROM load_new_guix_revision_job_events
    INNER JOIN load_new_guix_revision_jobs ON
      load_new_guix_revision_jobs.id = load_new_guix_revision_job_events.job_id
    WHERE load_new_guix_revision_jobs.commit = git_branches.commit AND
          git_branches.git_repository_id = load_new_guix_revision_jobs.git_repository_id
  ) AS job_events
FROM git_branches
LEFT OUTER JOIN guix_revisions ON git_branches.commit = guix_revisions.commit
WHERE git_branches.git_repository_id = $1
ORDER BY name, datetime DESC"))

  (map
   (match-lambda
     ((name commit datetime guix_revision_exists job_events)
      (list name
            commit
            datetime
            (string=? guix_revision_exists "t")
            (if (or (and (string? job_events) (string=? job_events ""))
                    (eq? #f job_events))
                '()
                (vector->list (json-string->scm job_events))))))
   (exec-query
    conn
    query
    (list (number->string git-repository-id)))))

