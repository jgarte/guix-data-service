(define-module (guix-data-service model guix-revision)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:export (count-guix-revisions
            most-recent-n-guix-revisions
            commit->revision-id
            insert-guix-revision
            guix-commit-exists?
            guix-revision-exists?
            guix-revisions-cgit-url-bases))

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

  (match (exec-query conn insert)
    (((id)) id)))

(define (guix-commit-exists? conn commit)
  (define query
    "SELECT EXISTS(SELECT 1 FROM guix_revisions WHERE commit = $1)")

  (let ((result (caar
                 (exec-query conn query (list commit)))))
    (string=? result "t")))

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

(define (guix-revisions-cgit-url-bases conn guix-revision-ids)
  (map
   car
   (exec-query
    conn
    (simple-format #f "
SELECT cgit_url_base
FROM git_repositories
WHERE cgit_url_base IS NOT NULL AND id IN (
  SELECT git_repository_id
  FROM guix_revisions
  WHERE id IN (VALUES ~A));"
                   (string-join
                    (map (lambda (id)
                           (string-append "(" id ")"))
                         guix-revision-ids)
                    ",")))))
