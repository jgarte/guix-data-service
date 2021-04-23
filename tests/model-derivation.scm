(define-module (test-model-derivation)
  #:use-module (srfi srfi-64)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model derivation))

(test-begin "test-model-derivation")

(with-postgresql-connection
 "test-model-derivation"
 (lambda (conn)
   (check-test-database! conn)

   (test-equal "count-derivations"
     '("0")
     (count-derivations conn))))

(test-end)
