(define-module (guix-data-service model lint-warning)
  #:use-module (squee)
  #:use-module (guix-data-service model utils)
  #:export (lint-warnings-data->lint-warning-ids
            insert-guix-revision-lint-warnings))

(define (lint-warnings-data->lint-warning-ids
         conn
         ;; (lint-checker-id package-id location-id lint-warning-message-set-id)
         lint-warnings-data)
  (insert-missing-data-and-return-all-ids
   conn
   "lint_warnings"
   `((lint_checker_id             . ,identity)
     (package_id                  . ,identity)
     (location_id                 . ,identity)
     (lint_warning_message_set_id . ,identity))
   lint-warnings-data))

(define (insert-guix-revision-lint-warnings conn
                                            guix-revision-id
                                            lint-warning-ids)
  (exec-query
   conn
   (string-append
    "INSERT INTO guix_revision_lint_warnings (lint_warning_id, guix_revision_id) "
    "VALUES "
    (string-join
     (map (lambda (lint-warning-id)
            (simple-format
             #f
             "(~A, ~A)"
             lint-warning-id
             guix-revision-id))
          lint-warning-ids)
     ", "))))
