(define-module (tests model-license-set)
  #:use-module (srfi srfi-64)
  #:use-module (guix utils)
  #:use-module (guix tests)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model license)
  #:use-module (guix-data-service model license-set))

(test-begin "test-model-license-set")

(define license-data
  '((("License 1"
      "https://gnu.org/licenses/test-1.html"
      "https://example.com/why-license-1"))
    (("License 1"
      "https://gnu.org/licenses/test-1.html"
      #f)
     ("License 2"
      #f
      #f))))

(with-postgresql-connection
 "test-model-license-set"
 (lambda (conn)
   (check-test-database! conn)

   (with-postgresql-transaction
    conn
    (lambda (conn)
      (test-assert "works"
        (inferior-packages->license-set-ids
         conn
         (inferior-packages->license-id-lists conn license-data))))
    #:always-rollback? #t)

   (with-postgresql-transaction
    conn
    (lambda (conn)
      (let ((license-id-lists
             (inferior-packages->license-id-lists conn license-data)))
        (test-equal "works repeatedly"
          (inferior-packages->license-set-ids conn license-id-lists)
          (inferior-packages->license-set-ids conn license-id-lists))))
    #:always-rollback? #t)))

(test-end)
