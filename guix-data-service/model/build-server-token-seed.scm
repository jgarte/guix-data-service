(define-module (guix-data-service model build-server-token-seed)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (squee)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt base64)
  #:export (compute-tokens-for-build-server))

(define (compute-token secret-key-base build-server-id token-seed)
  (let ((source-string
         (simple-format #f "~A:~A:~A"
                        secret-key-base
                        build-server-id
                        token-seed)))
    (string-filter
     (base64-encode
      (bytevector-hash
       (string->utf8 source-string)
       (hash-algorithm sha1)))
     ;; Remove the + / and = to make handling the value easier
     char-set:letter+digit)))

(define (compute-tokens-for-build-server conn secret-key-base build-server-id)
  (define query
    "
SELECT token_seed
FROM build_server_token_seeds
WHERE build_server_id = $1
ORDER BY token_seed")

  (map
   (match-lambda
     ((token-seed)
      (cons token-seed
            (compute-token secret-key-base
                           build-server-id
                           token-seed))))
   (exec-query conn query (list (number->string build-server-id)))))

