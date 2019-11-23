(define-module (guix-data-service model build-server)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:export (select-build-servers))

(define (select-build-servers conn)
  (define query
    "
SELECT id, url, lookup_all_derivations
FROM build_servers
ORDER BY id")

  (map
   (match-lambda
     ((id url lookup-all-derivations)
      (list (string->number id)
            url
            (string=? lookup-all-derivations "t"))))
   (exec-query conn query)))
