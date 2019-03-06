(define-module (guix-data-service model build-server)
  #:use-module (squee)
  #:export (select-build-servers))

(define (select-build-servers conn)
  (exec-query conn
              (string-append
               "SELECT id, url, lookup_all_derivations "
               "FROM build_servers")))
