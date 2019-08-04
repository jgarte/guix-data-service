 (define-module (tests model-license-set)
  #:use-module (srfi srfi-64)
  #:use-module (guix utils)
  #:use-module (guix tests)
  #:use-module (guix-data-service database)
  #:use-module (tests mock-inferior)
  #:use-module (guix-data-service model license-set))

(use-modules (tests driver))

(test-begin "test-model-license-set")

(mock
 ((guix-data-service model license)
  inferior-packages->license-data
  (lambda (inf packages)
    '((("License 1"
        "https://gnu.org/licenses/test-1.html"
        "https://example.com/why-license-1"))
      (("License 1"
        "https://gnu.org/licenses/test-1.html"
        #f)
       ("License 2"
        #f
        #f)))))

 (with-postgresql-connection
  "test-model-license-set"
  (lambda (conn)
    (with-postgresql-transaction
     conn
     (lambda (conn)
       (test-assert "works"
         (inferior-packages->license-set-ids conn #f #f)))
     #:always-rollback? #t)

    (with-postgresql-transaction
     conn
     (lambda (conn)
       (test-equal "works repeatedly"
         (inferior-packages->license-set-ids conn #f #f)
         (inferior-packages->license-set-ids conn #f #f)))
     #:always-rollback? #t))))

(test-end)
