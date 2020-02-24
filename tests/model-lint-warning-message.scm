(define-module (tests model-lint-warning-message)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model lint-warning-message))

(test-begin "test-model-lint-warning-message")

(define data
  '(("en" . "Test message")
    ("es" . "Test message in Spanish")))

(with-postgresql-connection
 "test-model-lint-checker"
 (lambda (conn)
   (check-test-database! conn)

   (test-assert "single insert"
     (with-postgresql-transaction
      conn
      (lambda (conn)
        (match (lint-warning-message-data->lint-warning-message-ids conn data)
          (((? number? id1) (? number? id2))
           #t)))
      #:always-rollback? #t))

   (test-assert "double insert"
     (with-postgresql-transaction
      conn
      (lambda (conn)
        (match (lint-warning-message-data->lint-warning-message-ids conn data)
          (((? number? id1) (? number? id2))
           (match (lint-warning-message-data->lint-warning-message-ids conn data)
             (((? number? second-id1) (? number? second-id2))
              (and (eq? id1 second-id1)
                   (eq? id2 second-id2)))))))
      #:always-rollback? #t))

   (test-assert "single set insert"
     (with-postgresql-transaction
      conn
      (lambda (conn)
        (match (lint-warning-message-data->lint-warning-message-set-id conn data)
          ((? number? id1)
           #t)))
      #:always-rollback? #t))

   (test-assert "double set insert"
     (with-postgresql-transaction
      conn
      (lambda (conn)
        (match (lint-warning-message-data->lint-warning-message-set-id conn data)
          ((? number? id)
           (match (lint-warning-message-data->lint-warning-message-set-id conn data)
             ((? number? second-id)
              (eq? id second-id))))))
      #:always-rollback? #t))))

(test-end)
