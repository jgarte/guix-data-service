(define-module (tests model-lint-checker)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model lint-checker))

(test-begin "test-model-lint-checker")

(with-postgresql-connection
 "test-model-lint-checker"
 (lambda (conn)
   (check-test-database! conn)

   (test-assert "single insert"
     (with-postgresql-transaction
      conn
      (lambda (conn)
        (define data
          `((name-1 #t ,(string->number (insert-lint-checker-description-set
                                         conn '(37))))
            (name-2 #f ,(string->number (insert-lint-checker-description-set
                                         conn '(38))))))

        (match (lint-checkers->lint-checker-ids conn data)
          (((? number? id1) (? number? id2))
           #t)))
      #:always-rollback? #t))

   (test-assert "double insert"
     (with-postgresql-transaction
      conn
      (lambda (conn)
        (define data
          `((name-1 #t ,(string->number (insert-lint-checker-description-set
                                         conn '(37))))
            (name-2 #f ,(string->number (insert-lint-checker-description-set
                                         conn '(38))))))

        (match (lint-checkers->lint-checker-ids conn data)
          (((? number? id1) (? number? id2))
           (match (lint-checkers->lint-checker-ids conn data)
             (((? number? second-id1) (? number? second-id2))
              (and (eq? id1 second-id1)
                   (eq? id2 second-id2)))))))
      #:always-rollback? #t))))

(test-end)
