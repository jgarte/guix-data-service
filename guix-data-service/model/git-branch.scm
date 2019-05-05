(define-module (guix-data-service model git-branch)
  #:use-module (squee)
  #:export (insert-git-branch-entry
            git-branches-for-commit
            most-recent-100-commits-for-branch
            all-branches-with-most-recent-commit))

(define (insert-git-branch-entry conn
                                 name commit
                                 git-repository-id datetime)
  (exec-query
   conn
   (string-append
    "INSERT INTO git_branches (name, commit, git_repository_id, datetime) "
    "VALUES ($1, $2, $3, $4) "
    "ON CONFLICT DO NOTHING")
   (list name
         commit
         git-repository-id
         datetime)))

(define (git-branches-for-commit conn commit)
  (define query
    "
SELECT name, datetime FROM git_branches WHERE commit = $1
ORDER BY datetime DESC")

  (exec-query conn query (list commit)))

(define (most-recent-100-commits-for-branch conn branch-name)
  (define query
    (string-append
     "SELECT git_branches.commit, datetime, "
     "(guix_revisions.id IS NOT NULL) as guix_revision_exists "
     "FROM git_branches "
     "LEFT OUTER JOIN guix_revisions ON git_branches.commit = guix_revisions.commit "
     "WHERE name = $1 ORDER BY datetime DESC LIMIT 100;"))

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

