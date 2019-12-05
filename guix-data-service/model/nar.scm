1(define-module (guix-data-service model nar)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:use-module (squee)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (gcrypt pk-crypto)
  #:use-module (gcrypt base16)
  #:use-module (guix scripts substitute)
  #:use-module (guix-data-service model utils)
  #:export (select-outputs-for-successful-builds-without-known-nar-entries
            select-nars-for-output
            select-signing-key

            record-narinfo-details-and-return-ids))

(define narinfo-contents
  (@@ (guix scripts substitute) narinfo-contents))

(define (record-narinfo-details-and-return-ids conn build-server-id narinfos)
  (define data
    (map (lambda (narinfo)
           (match (string-split
                   (narinfo-hash narinfo)
                   #\:)
             ((hash-algorithm hash)
              (list
               (narinfo-path narinfo)
               hash-algorithm
               hash
               (narinfo-size narinfo)
               (or (narinfo-system narinfo) NULL)
               (or (narinfo-deriver narinfo) NULL)))))
         narinfos))

  (let ((nar-ids
         (insert-missing-data-and-return-all-ids
          conn
          "nars"
          '(store_path hash_algorithm hash size system deriver)
          data)))

    (exec-query
     conn
     (string-append
      "
INSERT INTO nar_references (nar_id, reference)
VALUES "
      (string-join
       (concatenate
        (map (lambda (nar-id narinfo)
               (map (lambda (reference)
                      (simple-format
                       #f
                       "(~A, ~A)"
                       nar-id
                       (quote-string reference)))
                    (narinfo-references narinfo)))
             nar-ids
             narinfos))
       ", ")
      "
ON CONFLICT DO NOTHING"))

    (exec-query
     conn
     (string-append
      "
INSERT INTO nar_urls (nar_id, url, compression, file_size)
VALUES "
      (string-join
       (concatenate
        (map (lambda (nar-id narinfo)
               (map (lambda (uri compression file-size)
                      (simple-format
                       #f
                       "(~A, ~A, ~A, ~A)"
                       nar-id
                       (quote-string
                        (uri->string uri))
                       (quote-string compression)
                       file-size))
                    (narinfo-uris narinfo)
                    (narinfo-compressions narinfo)
                    (narinfo-file-sizes narinfo)))
             nar-ids
             narinfos))
       ", ")
      "
ON CONFLICT DO NOTHING"))

    (for-each (lambda (nar-id narinfo)
                (let ((narinfo-signature-data-id
                       (narinfo-signature->data-id conn narinfo)))

                  (exec-query
                   conn
                   (string-append
                    "
INSERT INTO narinfo_signatures (nar_id, narinfo_signature_data_id)
VALUES "
                    (simple-format
                     #f
                     "(~A,~A)"
                     nar-id
                     narinfo-signature-data-id)
                    "
ON CONFLICT DO NOTHING"))

                  (exec-query
                   conn
                   (string-append
                    "
INSERT INTO narinfo_fetch_records (narinfo_signature_data_id, build_server_id)
VALUES ($1, $2)")
                   (list (number->string narinfo-signature-data-id)
                         (number->string build-server-id)))))
              nar-ids
              narinfos)

    nar-ids))

(define (sexp->json-string sexp)
  (define (transform x)
    (if (list? x)
        (list->vector (map transform x))
        (if (bytevector? x)
            `((base16 . ,(bytevector->base16-string x)))
            x)))

  (scm->json-string (transform sexp)))

(define (narinfo-signature->data-id conn narinfo)
  (let ((public-key-id
         (narinfo-signature->public-key-id
          conn
          (narinfo-signature narinfo)))
        (contents
         (narinfo-contents narinfo)))

    (match (string-contains contents "Signature:")
      (#f #f)
      (index
       (let* ((body (string-take contents index))
              (signature-line (string-drop contents index))
              (signature-sexp
               (canonical-sexp->sexp
                (narinfo-signature narinfo))))

         (match (string-split (second (string-split signature-line
                                                    #\space))
                              #\;)
           ((version host-name signature-data)

            (first
             (insert-missing-data-and-return-all-ids
              conn
              "narinfo_signature_data"
              '(version host_name data_hash data_hash_algorithm
                        data_json sig_val_json narinfo_signature_public_key_id
                        narinfo_body narinfo_signature_line)
              (list
               (append (list (string->number version)
                             host-name)
                       (let* ((data-sexp
                               (find (match-lambda
                                       ((component data ...)
                                        (if (eq? component 'data)
                                            data
                                            #f))
                                       (_ #f))
                                     signature-sexp))
                              (hash-sexp
                               (third data-sexp))
                              (hash-algorithm
                               (second hash-sexp))
                              (hash
                               (third hash-sexp)))
                         (list
                          (bytevector->base16-string hash)
                          hash-algorithm
                          (cons "jsonb"
                                (sexp->json-string data-sexp))))
                       (let ((sig-val-sexp
                              (find (match-lambda
                                      ((component data ...)
                                       (if (eq? component 'sig-val)
                                           data
                                           #f))
                                      (_ #f))
                                    signature-sexp)))
                         (list
                          (cons "jsonb"
                                (sexp->json-string sig-val-sexp))))
                       (list public-key-id
                             body
                             signature-line))))))))))))

(define (narinfo-signature->public-key-id conn signature)
  (let* ((public-key-sexp
          (find (match-lambda
                  ((component data ...)
                   (if (eq? component 'public-key)
                       data
                       #f))
                  (_ #f))
                (canonical-sexp->sexp signature)))
         (public-key-json-string
          (sexp->json-string public-key-sexp)))

    (first
     (insert-missing-data-and-return-all-ids
      conn
      "narinfo_signature_public_keys"
      '(sexp_json)
      (list (list (cons "jsonb"
                        public-key-json-string)))))))

(define (select-outputs-for-successful-builds-without-known-nar-entries
         conn
         build-server-id
         guix-revision-commits)
  (define query
    (string-append "
SELECT DISTINCT derivation_output_details.path
FROM derivations
INNER JOIN derivation_outputs
  ON derivations.id = derivation_outputs.id
INNER JOIN derivation_output_details
  ON derivation_outputs.derivation_output_details_id = derivation_output_details.id
WHERE file_name IN (
  SELECT derivation_file_name
  FROM builds
  INNER JOIN build_status
    ON builds.id = build_status.build_id
  WHERE
    build_server_id = $1 AND
    build_status.status = 'succeeded'
) AND derivation_output_details.path NOT IN (
  SELECT store_path FROM nars
) AND
  derivations.id IN (
    SELECT derivation_id
    FROM package_derivations
    INNER JOIN guix_revision_package_derivations
      ON guix_revision_package_derivations.package_derivation_id = package_derivations.id
    INNER JOIN guix_revisions
      ON guix_revisions.id = guix_revision_package_derivations.revision_id
    WHERE guix_revisions.commit IN ("
                   (string-join (map quote-string guix-revision-commits)
                                ",")
                   ")
)
LIMIT 1500"))

  (map car (exec-query conn query (list (number->string
                                         build-server-id)))))

(define (select-nars-for-output conn output-file-name)
  (define query
    "
SELECT hash_algorithm, hash, size,
       (
         SELECT JSON_AGG(
           json_build_object('url', url, 'compression', compression, 'size', file_size)
         )
         FROM nar_urls
         WHERE nar_id = nars.id
       ) AS urls,
       (
         SELECT JSON_AGG(
           json_build_object(
             'version', version,
             'host_name', host_name,
             'data_hash', data_hash,
             'data_hash_algorithm', data_hash_algorithm,
             'data', data_json,
             'sig_val', sig_val_json,
             'narinfo_signature_public_key', (
               SELECT sexp_json
               FROM narinfo_signature_public_keys
               WHERE narinfo_signature_public_keys.id = narinfo_signature_public_key_id
             ),
             'body', narinfo_body,
             'signature_line', narinfo_signature_line
           )
         )
         FROM narinfo_signature_data
         INNER JOIN narinfo_signatures
           ON narinfo_signature_data.id = narinfo_signatures.narinfo_signature_data_id
         WHERE narinfo_signatures.nar_id = nars.id
       )
FROM nars
WHERE store_path = $1")

  (map
   (match-lambda
     ((hash-algorithm hash size urls-json signatures-json)
      (list hash-algorithm
            hash
            (string->number size)
            (vector->list
             (json-string->scm urls-json))
            (vector->list
             (json-string->scm signatures-json)))))
   (exec-query conn query (list output-file-name))))

(define (select-signing-key conn id)
  (define query
    "
SELECT sexp_json
FROM narinfo_signature_public_keys
WHERE id = $1")

  (match (exec-query conn query (list (number->string id)))
    (((sexp_json))
     (json-string->scm sexp_json))))
