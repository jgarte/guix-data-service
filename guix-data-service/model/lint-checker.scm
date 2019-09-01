(define-module (guix-data-service model lint-checker)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix-data-service model utils)
  #:export (lint-checkers->lint-checker-ids
            lint-warning-count-by-lint-checker-for-revision
            insert-guix-revision-lint-checkers))

(define (lint-checkers->lint-checker-ids conn lint-checkers-data)
  (insert-missing-data-and-return-all-ids
   conn
   "lint_checkers"
   `((name              . ,(lambda (value)
                             (quote-string (symbol->string value))))
     (description       . ,quote-string)
     (network_dependent . ,value->sql-boolean))
   lint-checkers-data))

(define (lint-warning-count-by-lint-checker-for-revision conn commit-hash)
  (define query
    "
SELECT lint_checkers.name, lint_checkers.description,
       lint_checkers.network_dependent, revision_data.count
FROM lint_checkers
INNER JOIN (
  SELECT lint_checker_id, COUNT(*)
  FROM lint_warnings
  WHERE id IN (
    SELECT lint_warning_id
    FROM guix_revision_lint_warnings
    INNER JOIN guix_revisions
    ON guix_revision_lint_warnings.guix_revision_id = guix_revisions.id
    WHERE commit = $1
  )
  GROUP BY lint_checker_id
) AS revision_data ON lint_checkers.id = revision_data.lint_checker_id
ORDER BY count DESC")

  (exec-query conn query (list commit-hash)))

(define (insert-guix-revision-lint-checkers conn
                                            guix-revision-id
                                            lint-checker-ids)
  (exec-query
   conn
   (string-append
    "INSERT INTO guix_revision_lint_checkers (lint_checker_id, guix_revision_id) "
    "VALUES "
    (string-join
     (map (lambda (lint-checker-id)
            (simple-format
             #f
             "(~A, ~A)"
             lint-checker-id
             guix-revision-id))
          lint-checker-ids)
     ", "))))
