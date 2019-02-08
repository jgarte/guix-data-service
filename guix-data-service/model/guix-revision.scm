(define-module (guix-data-service model guix-revision)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:export (most-recent-n-guix-revisions
            commit->revision-id
            insert-guix-revision
            guix-revision-exists?))

(define (most-recent-n-guix-revisions conn n)
  (exec-query conn "SELECT * FROM guix_revisions ORDER BY id DESC LIMIT 10"))

(define (commit->revision-id conn commit)
  (match (exec-query
          conn "SELECT id FROM guix_revisions WHERE commit = $1 LIMIT 1"
          (list commit))
    (((id))
     id)
    (() #f)))

(define (insert-guix-revision conn url commit store_path)
  (define insert
    (string-append "INSERT INTO guix_revisions "
                   "(url, commit, store_path) VALUES "
                   "('" url "', '"
                   commit "', '"
                   store_path "') "
                   "RETURNING id;"))

  (map car (exec-query conn insert)))

(define (guix-revision-exists? conn url commit)
  (define query
    (string-append "SELECT EXISTS("
                   "SELECT 1 FROM guix_revisions WHERE url = '" url "' "
                   "AND commit = '" commit "')"
                   ";"))

  (let ((result (caar
                 (exec-query conn query))))
    (string=? result "t")))
