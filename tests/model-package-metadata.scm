(define-module (test-model-package-metadata)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64)
  #:use-module (guix utils)
  #:use-module (tests mock-inferior)
  #:use-module (guix-data-service database))

(test-begin "test-model-package-metadata")

(define mock-inferior-package-foo
  (mock-inferior-package
   (name "foo")
   (version "2")
   (synopsis "Foo")
   (description "Foo description")
   (home-page "https://example.com")
   (location (location "file.scm" 5 0))))

(with-mock-inferior-packages
 (lambda ()
   (use-modules (guix-data-service model package)
                (guix-data-service model git-repository)
                (guix-data-service model guix-revision)
                (guix-data-service model package-metadata))

   (with-postgresql-connection
    (lambda (conn)
      (test-assert "inferior-packages->package-metadata-ids"
        (with-postgresql-transaction
         conn
         (lambda (conn)
           (match
               (inferior-packages->package-metadata-ids
                conn
                (list mock-inferior-package-foo))
             ((x) (string? x))))
         #:always-rollback? #t))))))

(test-end)
