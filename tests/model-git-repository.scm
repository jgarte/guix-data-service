(define-module (test-model-git-repository)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model git-repository))

(test-begin "test-model-git-repository")

(with-postgresql-connection
 "test-model-git-repository"
 (lambda (conn)
   (check-test-database! conn)

   (test-assert "returns an id for a non existent URL"
     (with-postgresql-transaction
      conn
      (lambda (conn)
        (match (git-repository-url->git-repository-id
                conn
                "test-non-existent-url")
          ((? number? x)
           #t)))
      #:always-rollback? #t))

   (let* ((url "test-url")
          (id (git-repository-url->git-repository-id conn url)))
     (with-postgresql-transaction
      conn
      (lambda (conn)
        (test-equal "returns the right id for an existing URL"
          id
          (git-repository-url->git-repository-id conn url)))
      #:always-rollback? #t))))

(test-end)
