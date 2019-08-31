(define-module (guix-data-service model lint-checker)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service model utils)
  #:export (lint-checkers->lint-checker-ids))

(define (lint-checkers->lint-checker-ids conn lint-checkers-data)
  (insert-missing-data-and-return-all-ids
   conn
   "lint_checkers"
   `((name              . ,(lambda (value)
                             (quote-string (symbol->string value))))
     (description       . ,quote-string)
     (network_dependent . ,value->sql-boolean))
   lint-checkers-data))
