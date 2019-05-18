(define-module (guix-data-service model git-branch)
  #:use-module (squee)
  #:use-module (srfi srfi-19)
  #:use-module (guix-data-service model utils)
  #:export (insert-git-branch-entry
            git-branches-for-commit
            git-branches-with-repository-details-for-commit
            most-recent-commits-for-branch
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
         git-repository-id
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
SELECT git_repositories.label, git_repositories.url,
       git_repositories.cgit_url_base,
       git_branches.name, git_branches.datetime
FROM git_branches
INNER JOIN git_repositories
  ON git_branches.git_repository_id = git_repositories.id
WHERE git_branches.commit = $1")

  (group-list-by-first-n-fields
   3
   (exec-query conn query (list commit))))

(define* (most-recent-commits-for-branch conn branch-name
                                         #:key
                                         (limit 100)
                                         after-date
                                         before-date)
  (define query
    (string-append
     "SELECT git_branches.commit, datetime, "
     "(guix_revisions.id IS NOT NULL) as guix_revision_exists "
     "FROM git_branches "
     "LEFT OUTER JOIN guix_revisions ON git_branches.commit = guix_revisions.commit "
     "WHERE name = $1 "
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

  (exec-query
   conn
   query
   (list branch-name)))

(define (all-branches-with-most-recent-commit conn)
  (define query
    (string-append
     "SELECT DISTINCT ON (name) name, git_branches.commit, "
     "datetime, (guix_revisions.id IS NOT NULL) guix_revision_exists "
     "FROM git_branches "
     "LEFT OUTER JOIN guix_revisions ON git_branches.commit = guix_revisions.commit "
     "WHERE git_branches.commit IS NOT NULL "
     "ORDER BY name, datetime DESC;"))

  (exec-query
   conn
   query))

