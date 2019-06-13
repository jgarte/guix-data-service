(define-module (guix-data-service model git-repository)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:export (all-git-repositories
            git-repository-id->url
            git-repository-url->git-repository-id
            git-repositories-containing-commit

            guix-revisions-and-jobs-for-git-repository))

(define (all-git-repositories conn)
  (exec-query
   conn
   (string-append
    "SELECT id, label, url FROM git_repositories ORDER BY id ASC")))

(define (git-repository-id->url conn id)
  (match
      (exec-query
       conn
       (string-append
        "SELECT url FROM git_repositories WHERE id = $1;")
       (list id))
    (((url)) url)))

(define (git-repository-url->git-repository-id conn url)
  (let ((existing-id
         (exec-query
          conn
          (string-append
           "SELECT id FROM git_repositories WHERE url = '" url "'"))))
    (match existing-id
      (((id)) id)
      (()
       (caar
        (exec-query conn
                    (string-append
                     "INSERT INTO git_repositories "
                     "(url) "
                     "VALUES "
                     "('" url "') "
                     "RETURNING id")))))))

(define (guix-revisions-and-jobs-for-git-repository conn git-repository-id)
  (define query
    "
SELECT NULL AS id, load_new_guix_revision_jobs.id AS job_id, commit, source
FROM load_new_guix_revision_jobs
WHERE git_repository_id = $1 AND succeeded_at IS NULL AND NOT EXISTS (
  SELECT 1 FROM load_new_guix_revision_job_events
  WHERE event = 'failure' AND job_id = load_new_guix_revision_jobs.id
)
UNION
SELECT id, NULL, commit, NULL
FROM guix_revisions
WHERE git_repository_id = $1
ORDER BY 1 DESC NULLS FIRST, 2 DESC LIMIT 10;")

  (exec-query
   conn
   query
   (list git-repository-id)))

(define (git-repositories-containing-commit conn commit)
  (define query
    "
SELECT id, label, url, cgit_url_base
FROM git_repositories WHERE id IN (
  SELECT git_repository_id
  FROM git_branches
  WHERE commit = $1
)")

  (exec-query conn query (list commit)))
