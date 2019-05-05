(define-module (guix-data-service model guix-revision)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:export (count-guix-revisions
            most-recent-n-guix-revisions
            commit->revision-id
            insert-guix-revision
            guix-revision-exists?))

(define (count-guix-revisions conn)
  (first
   (exec-query
    conn
    "SELECT COUNT(*) FROM guix_revisions")))

(define (most-recent-n-guix-revisions conn n)
  (exec-query conn "SELECT * FROM guix_revisions ORDER BY id DESC LIMIT 10"))

(define (commit->revision-id conn commit)
  (match (exec-query
          conn "SELECT id FROM guix_revisions WHERE commit = $1 LIMIT 1"
          (list commit))
    (((id))
     id)
    (() #f)))

(define (insert-guix-revision conn git-repository-id commit store_path)
  (define insert
    (string-append "INSERT INTO guix_revisions "
                   "(git_repository_id, commit, store_path) VALUES "
                   "(" git-repository-id ", '"
                   commit "', '"
                   store_path "') "
                   "RETURNING id;"))

  (map car (exec-query conn insert)))

(define (guix-revision-exists? conn git-repository-id commit)
  (define query
    (string-append "SELECT EXISTS("
                   "SELECT 1 FROM guix_revisions WHERE "
                   "git_repository_id = '" git-repository-id "' "
                   "AND commit = '" commit "')"
                   ";"))

  (let ((result (caar
                 (exec-query conn query))))
    (string=? result "t")))
