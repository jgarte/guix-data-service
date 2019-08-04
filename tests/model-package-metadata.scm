(define-module (test-model-package-metadata)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64)
  #:use-module (guix utils)
  #:use-module (guix tests)
  #:use-module (tests mock-inferior)
  #:use-module (guix-data-service model license-set)
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

(define mock-inferior-package-foo-2
  (mock-inferior-package
   (name "foo")
   (version "2")
   (synopsis "Foo")
   (description "Foo description")
   (home-page #f)
   (location #f)))

(define (test-license-set-ids conn)
  (mock
   ((guix-data-service model license)
    inferior-packages->license-data
    (lambda (inf packages)
      '((("License 1"
          "https://gnu.org/licenses/test-1.html"
          "https://example.com/why-license-1")))))

   (inferior-packages->license-set-ids conn #f #f)))

(with-mock-inferior-packages
 (lambda ()
   (use-modules (guix-data-service model package)
                (guix-data-service model git-repository)
                (guix-data-service model guix-revision)
                (guix-data-service model package-metadata))

   (with-postgresql-connection
    "test-model-package-metadata"
    (lambda (conn)
      (test-assert "inferior-packages->package-metadata-ids"
        (with-postgresql-transaction
         conn
         (lambda (conn)
           (match
               (inferior-packages->package-metadata-ids
                conn
                (list mock-inferior-package-foo
                      mock-inferior-package-foo-2)
                (test-license-set-ids conn))
             ((x) (string? x))))
         #:always-rollback? #t))

      (with-postgresql-transaction
       conn
       (lambda (conn)
         (test-equal "inferior-packages->package-metadata-ids"
           (inferior-packages->package-metadata-ids
            conn
            (list mock-inferior-package-foo
                  mock-inferior-package-foo-2)
            (test-license-set-ids conn))
           (inferior-packages->package-metadata-ids
            conn
            (list mock-inferior-package-foo
                  mock-inferior-package-foo-2)
            (test-license-set-ids conn)))
         #:always-rollback? #t))))))

(test-end)
