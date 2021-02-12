;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (guix-data-service model derivation)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (gcrypt hash)
  #:use-module (squee)
  #:use-module (json)
  #:use-module (guix base16)
  #:use-module (guix base32)
  #:use-module (guix serialization)
  #:use-module (lzlib)
  #:use-module (guix inferior)
  #:use-module (guix memoization)
  #:use-module (guix derivations)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model utils)
  #:export (valid-systems
            valid-targets
            count-derivations
            select-derivation-by-file-name
            select-derivation-by-file-name-hash
            select-derivation-outputs-by-derivation-id
            select-derivation-sources-by-derivation-id
            select-derivation-references-by-derivation-id
            select-derivation-source-file-by-store-path
            select-derivation-source-file-nar-details-by-file-name
            select-derivation-source-file-nar-data-by-file-name
            select-derivation-source-file-data-by-file-name-hash
            select-derivation-by-output-filename
            select-derivations-using-output
            select-package-derivations-in-revision
            search-package-derivations-in-revision
            select-fixed-output-package-derivations-in-revision
            select-derivation-outputs-in-revision
            fix-derivation-output-details-hash-encoding
            select-derivations-by-revision-name-and-version
            select-derivation-inputs-by-derivation-id
            select-serialized-derivation-by-file-name
            select-existing-derivations
            select-derivations-by-id
            select-derivations-and-build-status
            derivation-file-names->derivation-ids))

(define (valid-systems conn)
  ;; TODO, use the database, but make it quick!
  '("aarch64-linux"
    "armhf-linux"
    "i586-gnu"
    "i686-linux"
    "mips64el-linux"
    "powerpc64le-linux"
    "x86_64-linux"))

(define (valid-targets conn)
  '("arm-linux-gnueabihf"
    "aarch64-linux-gnu"
    "powerpc-linux-gnu"
    "riscv64-linux-gnu"
    "i586-pc-gnu"))

(define (count-derivations conn)
  (first
   (exec-query
    conn
    "SELECT COUNT(*) FROM derivations")))

(define (select-existing-derivations file-names)
  (string-append "SELECT id, file_name "
                 "FROM derivations "
                 "WHERE file_name IN "
                 "(" (string-join (map (lambda (file-name)
                                         (simple-format #f "'~A'" file-name))
                                       file-names)
                                  ",")
                 ");"))

(define (select-from-derivation-output-details paths)
  (string-append
   "SELECT id, path FROM derivation_output_details "
   "WHERE path IN ("
   (string-join (map quote-string paths)
                ",")
   ")"))

(define (select-derivation-by-output-filename conn filename)
  (define query
    (string-append
     "SELECT derivations.file_name, derivation_outputs.id "
     "FROM derivation_output_details "
     "INNER JOIN derivation_outputs"
     " ON derivation_output_details.id = derivation_outputs.derivation_output_details_id "
     "INNER JOIN derivations"
     " ON derivation_outputs.derivation_id = derivations.id "
     "WHERE derivation_output_details.path = $1"))

  (exec-query conn query (list filename)))

(define (select-derivations-using-output conn output-id)
  (define query
    (string-append
     "SELECT derivations.file_name "
     "FROM derivations "
     "INNER JOIN derivation_inputs"
     " ON derivation_inputs.derivation_id = derivations.id "
     "WHERE derivation_output_id = $1 "
     "ORDER BY derivations.file_name "
     "LIMIT 100 "))

  (exec-query conn query (list output-id)))

(define (select-derivations-by-revision-name-and-version
         conn revision-commit-hash name version)
  (define query "
SELECT derivations.system,
       package_derivations.target,
       derivations.file_name,
       JSON_AGG(
         json_build_object(
           'build_server_id', builds.build_server_id,
           'build_server_build_id', builds.build_server_build_id,
           'status',  latest_build_status.status,
           'timestamp',  latest_build_status.timestamp,
           'build_for_equivalent_derivation',
           builds.derivation_file_name != derivations.file_name
         )
         ORDER BY latest_build_status.timestamp
       )
FROM derivations
INNER JOIN package_derivations
  ON derivations.id = package_derivations.derivation_id
INNER JOIN packages
  ON package_derivations.package_id = packages.id
INNER JOIN guix_revision_package_derivations
  ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
INNER JOIN guix_revisions
  ON guix_revision_package_derivations.revision_id = guix_revisions.id
INNER JOIN derivations_by_output_details_set
  ON derivations.id = derivations_by_output_details_set.derivation_id
LEFT OUTER JOIN builds
   ON derivations_by_output_details_set.derivation_output_details_set_id =
      builds.derivation_output_details_set_id
LEFT OUTER JOIN build_servers
  ON builds.build_server_id = build_servers.id
LEFT OUTER JOIN latest_build_status
  ON builds.id = latest_build_status.build_id
WHERE guix_revisions.commit = $1
  AND packages.name = $2
  AND packages.version = $3
GROUP BY derivations.system,
         package_derivations.target,
         derivations.file_name
ORDER BY derivations.system DESC,
         NULLIF(package_derivations.target, '') DESC NULLS FIRST,
         derivations.file_name")

  (map (match-lambda
         ((system target file-name builds-json)
          (list system
                target
                file-name
                (filter (lambda (build)
                          (string? (assoc-ref build "status")))
                        (vector->list
                         (json-string->scm builds-json)))))
         ((file_name system target)
          (list file_name system target)))
       (exec-query conn
                   query
                   (list revision-commit-hash name version))))

(define* (select-package-derivations-in-revision conn
                                                 commit-hash
                                                 #:key
                                                 systems
                                                 targets
                                                 minimum-builds
                                                 maximum-builds
                                                 limit-results
                                                 after-name
                                                 (include-builds? #t)
                                                 ;; build-status: failing,
                                                 ;; working, unknown
                                                 build-status)
  (define criteria
    (string-join
     `(,@(filter-map
          (lambda (field values)
            (if values
                (string-append
                 field " IN ("
                 (string-join (map (lambda (value)
                                     (simple-format #f "'~A'" value))
                                   values)
                              ",")
                 ")")
                #f))
          '("derivations.system"
            "target")
          (list systems
                targets))
       ,@(if minimum-builds
             (list
              (string-append
               "
(
  SELECT COUNT(*)
  FROM builds
  WHERE builds.derivation_output_details_set_id =
        derivations_by_output_details_set.derivation_output_details_set_id
) >= "
               (number->string minimum-builds)))
             '())
       ,@(if maximum-builds
             (list
              (string-append
               "
(
  SELECT COUNT(*)
  FROM builds
  WHERE builds.derivation_output_details_set_id =
        derivations_by_output_details_set.derivation_output_details_set_id
) <= "
               (number->string maximum-builds)))
             '())
       ,@(cond
          ((eq? build-status #f) '())
          ((eq? build-status 'failing)
           '("
(
  NOT EXISTS (
    SELECT 1
    FROM builds
    INNER JOIN latest_build_status
      ON builds.id = latest_build_status.build_id
    WHERE builds.derivation_output_details_set_id =
          derivations_by_output_details_set.derivation_output_details_set_id
      AND latest_build_status.status = 'succeeded'
  )
  AND EXISTS (
    SELECT 1
    FROM builds
    INNER JOIN latest_build_status
      ON builds.id = latest_build_status.build_id
    WHERE builds.derivation_output_details_set_id =
          derivations_by_output_details_set.derivation_output_details_set_id
      AND latest_build_status.status = 'failed'
  )
)"))
          ((eq? build-status 'working)
           '("
EXISTS (
  SELECT 1
  FROM builds
  INNER JOIN latest_build_status
    ON builds.id = latest_build_status.build_id
  WHERE builds.derivation_output_details_set_id =
        derivations_by_output_details_set.derivation_output_details_set_id
    AND latest_build_status.status = 'succeeded'
)"))
          ((eq? build-status 'unknown)
           '("
(
  NOT EXISTS (
    SELECT 1
    FROM builds
    INNER JOIN latest_build_status
      ON builds.id = latest_build_status.build_id
    WHERE builds.derivation_output_details_set_id =
          derivations_by_output_details_set.derivation_output_details_set_id
      AND latest_build_status.status = 'succeeded'
  )
  AND NOT EXISTS (
    SELECT 1
    FROM builds
    INNER JOIN latest_build_status
      ON builds.id = latest_build_status.build_id
    WHERE builds.derivation_output_details_set_id =
          derivations_by_output_details_set.derivation_output_details_set_id
      AND latest_build_status.status = 'failed'
  )
)"))
          (else
           (error "unknown build-status"))))
     " AND "))

  (define query
    (string-append
     "
SELECT derivations.file_name,
       derivations.system,
       package_derivations.target"
     (if include-builds?
         ",
       (
         SELECT JSON_AGG(
                  json_build_object(
                    'build_server_id', builds.build_server_id,
                    'build_server_build_id', builds.build_server_build_id,
                    'status',  latest_build_status.status,
                    'timestamp',  latest_build_status.timestamp,
                    'build_for_equivalent_derivation',
                    builds.derivation_file_name != derivations.file_name
                  )
                  ORDER BY latest_build_status.timestamp
                )
         FROM builds
         INNER JOIN latest_build_status
           ON builds.id = latest_build_status.build_id
         WHERE builds.derivation_output_details_set_id =
               derivations_by_output_details_set.derivation_output_details_set_id
       ) AS builds"
         "")
     "
FROM derivations
INNER JOIN derivations_by_output_details_set
  ON derivations.id = derivations_by_output_details_set.derivation_id
INNER JOIN package_derivations
  ON derivations.id = package_derivations.derivation_id
INNER JOIN guix_revision_package_derivations
  ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
INNER JOIN guix_revisions
  ON guix_revision_package_derivations.revision_id = guix_revisions.id
INNER JOIN packages
  ON package_derivations.package_id = packages.id
WHERE guix_revisions.commit = $1
"
     (if after-name
         " AND derivations.file_name > $2"
         "")
     (if (string-null? criteria)
         ""
         (string-append " AND " criteria))
     "
ORDER BY derivations.file_name
"
     (if limit-results
         (string-append
          " LIMIT " (number->string limit-results))
         "")))

  (map (match-lambda
         ((file_name system target builds)
          (list file_name
                system
                target
                (if (or (and (string? builds) (string-null? builds))
                        (eq? #f builds))
                    #()
                    (json-string->scm builds))))
         ((file_name system target)
          (list file_name system target)))
       (exec-query conn
                   query
                   `(,commit-hash
                     ,@(if after-name
                           (list after-name)
                           '())))))

(define* (search-package-derivations-in-revision conn
                                                 commit-hash
                                                 search-query
                                                 #:key
                                                 systems
                                                 targets
                                                 minimum-builds
                                                 maximum-builds
                                                 limit-results
                                                 after-name
                                                 (include-builds? #t)
                                                 ;; build-status: failing,
                                                 ;; working, unknown
                                                 build-status)
  (define criteria
    (string-join
     `(,@(filter-map
          (lambda (field values)
            (if values
                (string-append
                 field " IN ("
                 (string-join (map (lambda (value)
                                     (simple-format #f "'~A'" value))
                                   values)
                              ",")
                 ")")
                #f))
          '("derivations.system"
            "target")
          (list systems
                targets))
       ,@(if minimum-builds
             (list
              (string-append
               "
(
  SELECT COUNT(*)
  FROM builds
  WHERE builds.derivation_output_details_set_id =
        derivations_by_output_details_set.derivation_output_details_set_id
) >= "
               (number->string minimum-builds)))
             '())
       ,@(if maximum-builds
             (list
              (string-append
               "
(
  SELECT COUNT(*)
  FROM builds
  WHERE builds.derivation_output_details_set_id =
        derivations_by_output_details_set.derivation_output_details_set_id
) <= "
               (number->string maximum-builds)))
             '())
       ,@(cond
          ((eq? build-status #f) '())
          ((eq? build-status 'failing)
           '("
(
  NOT EXISTS (
    SELECT 1
    FROM builds
    INNER JOIN latest_build_status
      ON builds.id = latest_build_status.build_id
    WHERE builds.derivation_output_details_set_id =
          derivations_by_output_details_set.derivation_output_details_set_id
      AND latest_build_status.status = 'succeeded'
  )
  AND EXISTS (
    SELECT 1
    FROM builds
    INNER JOIN latest_build_status
      ON builds.id = latest_build_status.build_id
    WHERE builds.derivation_output_details_set_id =
          derivations_by_output_details_set.derivation_output_details_set_id
      AND latest_build_status.status = 'failed'
  )
)"))
          ((eq? build-status 'working)
           '("
EXISTS (
  SELECT 1
  FROM builds
  INNER JOIN latest_build_status
    ON builds.id = latest_build_status.build_id
  WHERE builds.derivation_output_details_set_id =
        derivations_by_output_details_set.derivation_output_details_set_id
    AND latest_build_status.status = 'succeeded'
)"))
          ((eq? build-status 'unknown)
           '("
(
  NOT EXISTS (
    SELECT 1
    FROM builds
    INNER JOIN latest_build_status
      ON builds.id = latest_build_status.build_id
    WHERE builds.derivation_output_details_set_id =
          derivations_by_output_details_set.derivation_output_details_set_id
      AND latest_build_status.status = 'succeeded'
  )
  AND NOT EXISTS (
    SELECT 1
    FROM builds
    INNER JOIN latest_build_status
      ON builds.id = latest_build_status.build_id
    WHERE builds.derivation_output_details_set_id =
          derivations_by_output_details_set.derivation_output_details_set_id
      AND latest_build_status.status = 'failed'
  )
)"))
          (else
           (error "unknown build-status"))))
     " AND "))

  (define query
    (string-append
     "
SELECT derivations.file_name,
       derivations.system,
       package_derivations.target"
     (if include-builds?
         ",
       (
         SELECT JSON_AGG(
                  json_build_object(
                    'build_server_id', builds.build_server_id,
                    'build_server_build_id', builds.build_server_build_id,
                    'status',  latest_build_status.status,
                    'timestamp',  latest_build_status.timestamp,
                    'build_for_equivalent_derivation',
                    builds.derivation_file_name != derivations.file_name
                  )
                  ORDER BY latest_build_status.timestamp
                )
         FROM builds
         INNER JOIN latest_build_status
           ON builds.id = latest_build_status.build_id
         WHERE builds.derivation_output_details_set_id =
               derivations_by_output_details_set.derivation_output_details_set_id
       ) AS builds"
         "")
     "
FROM derivations
INNER JOIN derivations_by_output_details_set
  ON derivations.id = derivations_by_output_details_set.derivation_id
INNER JOIN package_derivations
  ON derivations.id = package_derivations.derivation_id
INNER JOIN guix_revision_package_derivations
  ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
INNER JOIN guix_revisions
  ON guix_revision_package_derivations.revision_id = guix_revisions.id
INNER JOIN packages
  ON package_derivations.package_id = packages.id
WHERE guix_revisions.commit = $1
  AND derivations.file_name LIKE $2
"
     (if after-name
         " AND derivations.file_name > $3"
         "")
     (if (string-null? criteria)
         ""
         (string-append " AND " criteria))
     "
ORDER BY derivations.file_name
"
     (if limit-results
         (string-append
          " LIMIT " (number->string limit-results))
         "")))

  (map (match-lambda
         ((file_name system target)
          (list file_name
                system
                target))
         ((file_name system target builds)
          (list file_name
                system
                target
                (if (or (and (string? builds) (string-null? builds))
                        (eq? #f builds))
                    #()
                    (json-string->scm builds)))))
       (exec-query conn
                   query
                   `(,commit-hash
                     ,(string-append "%" search-query "%")
                     ,@(if after-name
                           (list after-name)
                           '())))))

(define* (select-fixed-output-package-derivations-in-revision
          conn
          commit
          system
          target
          #:key
          after-derivation-file-name
          (limit-results 50)
          ;; latest-build-status: failing,
          ;; working, unknown
          latest-build-status)
  (define query
    (string-append
     "
WITH RECURSIVE all_derivations(id) AS (
    SELECT package_derivations.derivation_id
    FROM package_derivations
    INNER JOIN guix_revision_package_derivations
      ON package_derivations.id =
         guix_revision_package_derivations.package_derivation_id
    INNER JOIN guix_revisions
      ON guix_revision_package_derivations.revision_id = guix_revisions.id
    WHERE guix_revisions.commit = $1
      AND package_derivations.system = $2
      AND package_derivations.target = $3
  UNION
    SELECT derivation_outputs.derivation_id
    FROM all_derivations
    INNER JOIN derivation_inputs
      ON all_derivations.id = derivation_inputs.derivation_id
    INNER JOIN derivation_outputs
      ON derivation_inputs.derivation_output_id = derivation_outputs.id
)
SELECT DISTINCT ON (derivations.file_name)
       derivations.file_name,
       (
          CASE
          WHEN latest_build_status.status IS NULL THEN NULL
          ELSE
            json_build_object(
              'build_server_id', builds.build_server_id,
              'build_server_build_id', builds.build_server_build_id,
              'status', latest_build_status.status,
              'timestamp', latest_build_status.timestamp
            )
          END
       ) AS latest_build
FROM all_derivations
INNER JOIN derivations
  ON all_derivations.id = derivations.id
INNER JOIN derivation_outputs
  ON all_derivations.id = derivation_outputs.derivation_id
INNER JOIN derivation_output_details
  ON derivation_outputs.derivation_output_details_id = derivation_output_details.id
LEFT JOIN builds
  -- This is intentional, as we want to build/check this exact derivation, not
  -- any others that happen to produce the same output
  ON derivations.file_name = builds.derivation_file_name
LEFT JOIN latest_build_status
  ON builds.id = latest_build_status.build_id
  -- These are the two interesting states, so ignore builds in any other states
 AND latest_build_status.status IN ('succeeded', 'failed')
WHERE derivation_output_details.hash IS NOT NULL"
     (if after-derivation-file-name
         "
  AND derivations.file_name > $5"
         "")
     (if latest-build-status
         (simple-format
          #f
          "
  AND latest_build_status.status = $~A"
          (if after-derivation-file-name 6 5))
         "")
     "
ORDER BY derivations.file_name, latest_build_status.timestamp DESC
LIMIT $4"))

  (map (match-lambda
         ((derivation_file_name latest_build)
          `((derivation_file_name . ,derivation_file_name)
            (latest_build         . ,(if (null? latest_build)
                                         'null
                                         (map (match-lambda
                                                ((key . value)
                                                 (cons (string->symbol key)
                                                       value)))
                                              (json-string->scm latest_build)))))))
       (exec-query-with-null-handling
        conn
        query
        `(,commit
          ,system
          ,target
          ,(number->string (or limit-results 999999)) ; TODO
          ,@(if after-derivation-file-name
                (list after-derivation-file-name)
                '())
          ,@(if latest-build-status
                (list latest-build-status)
                '())))))

(define* (select-derivation-outputs-in-revision conn
                                                commit-hash
                                                #:key
                                                search-query
                                                output-consistency
                                                nars-from-build-servers
                                                no-nars-from-build-servers
                                                system
                                                target
                                                limit-results
                                                after-path)
  (define query
    (string-append
     "
SELECT packages.name,
       packages.version,
       derivation_output_details.path,
       derivation_output_details.hash_algorithm,
       derivation_output_details.hash,
       derivation_output_details.recursive,
       (
         SELECT JSON_AGG(
           json_build_object(
             'build_server_id', narinfo_fetch_records.build_server_id,
             'hash_algorithm', nars.hash_algorithm,
             'hash', nars.hash,
             'size', nars.size
           )
         )
         FROM nars
         INNER JOIN narinfo_signatures
           ON nars.id = narinfo_signatures.nar_id
         INNER JOIN narinfo_signature_data
           ON narinfo_signature_data.id = narinfo_signatures.narinfo_signature_data_id
         INNER JOIN narinfo_fetch_records
           ON narinfo_signature_data.id = narinfo_fetch_records.narinfo_signature_data_id
         WHERE nars.store_path = derivation_output_details.path
       ) AS nars
FROM derivations
INNER JOIN derivation_outputs
  ON derivations.id = derivation_outputs.derivation_id
INNER JOIN derivation_output_details
  ON derivation_outputs.derivation_output_details_id = derivation_output_details.id
INNER JOIN package_derivations
  ON derivations.id = package_derivations.derivation_id
INNER JOIN guix_revision_package_derivations
  ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
INNER JOIN guix_revisions
  ON guix_revision_package_derivations.revision_id = guix_revisions.id
INNER JOIN packages
  ON package_derivations.package_id = packages.id
WHERE guix_revisions.commit = $1
"
     (let ((criteria
            `(,@(if after-path
                    '(" AND derivation_output_details.path > ")
                    '())
              ,@(if system
                    '(" AND package_derivations.system = ")
                    '())
              ,@(if target
                    '(" AND package_derivations.target = ")
                    '())
              ,@(if search-query
                    '(" AND derivation_output_details.path LIKE ")
                    '()))))
       (string-concatenate
        (map (lambda (query count)
               (simple-format #f "~A$~A"
                              query count))
             criteria
             (iota (length criteria) 2))))
     (if (list? nars-from-build-servers)
         (string-append
          "
AND ARRAY[" (string-join (map number->string nars-from-build-servers)
                         ", ")
"]::integer[] <@ COALESCE(( -- contained by
  SELECT ARRAY_AGG(narinfo_fetch_records.build_server_id)
  FROM nars
  INNER JOIN narinfo_signatures
    ON nars.id = narinfo_signatures.nar_id
  INNER JOIN narinfo_signature_data
    ON narinfo_signature_data.id = narinfo_signatures.narinfo_signature_data_id
  INNER JOIN narinfo_fetch_records
    ON narinfo_signature_data.id = narinfo_fetch_records.narinfo_signature_data_id
  WHERE nars.store_path = derivation_output_details.path
), ARRAY[]::integer[])")
         "")
     (if (list? no-nars-from-build-servers)
         (string-append
          "
AND NOT ARRAY[" (string-join (map number->string no-nars-from-build-servers)
                             ", ")
"]::integer[] && COALESCE((
  SELECT ARRAY_AGG(narinfo_fetch_records.build_server_id)
  FROM nars
  INNER JOIN narinfo_signatures
    ON nars.id = narinfo_signatures.nar_id
  INNER JOIN narinfo_signature_data
    ON narinfo_signature_data.id = narinfo_signatures.narinfo_signature_data_id
  INNER JOIN narinfo_fetch_records
    ON narinfo_signature_data.id = narinfo_fetch_records.narinfo_signature_data_id
  WHERE nars.store_path = derivation_output_details.path
), ARRAY[]::integer[])")
         "")
     (cond
      ((string=? output-consistency "any")
       "")
      ((string=? output-consistency "fixed-output")
       " AND derivation_output_details.hash IS NOT NULL")
      (else
       (string-append
        " AND derivation_output_details.hash IS NULL AND (
  SELECT
"
        (cond
         ((string=? output-consistency "unknown")
          "COUNT(DISTINCT narinfo_fetch_records.build_server_id) <= 1")
         ((string=? output-consistency "matching")
          "
    CASE
      WHEN (COUNT(DISTINCT narinfo_fetch_records.build_server_id) <= 1) THEN NULL
      ELSE (COUNT(DISTINCT nars.hash) = 1)
    END")
         ((string=? output-consistency "not-matching")
          "
    CASE
      WHEN (COUNT(DISTINCT narinfo_fetch_records.build_server_id) <= 1) THEN NULL
      ELSE (COUNT(DISTINCT nars.hash) > 1)
    END")
         (else
          (error "unknown reproducibility status")))
        "
  FROM nars
  INNER JOIN narinfo_signatures
    ON nars.id = narinfo_signatures.nar_id
  INNER JOIN narinfo_signature_data
    ON narinfo_signature_data.id = narinfo_signatures.narinfo_signature_data_id
  INNER JOIN narinfo_fetch_records
    ON narinfo_signature_data.id = narinfo_fetch_records.narinfo_signature_data_id
  WHERE nars.store_path = derivation_output_details.path
)")))
     "
ORDER BY derivation_output_details.path
"
     (if limit-results
         (string-append
          " LIMIT " (number->string limit-results))
         "")))

  (map (match-lambda
         ((package_name package_version
                        path hash_algorithm hash recursive nars_json)
          (list package_name
                package_version
                path
                hash
                hash_algorithm
                (string=? recursive "t")
                (if (null? nars_json)
                    #()
                    (json-string->scm nars_json)))))
       (exec-query-with-null-handling  conn
                                       query
                                       `(,commit-hash
                                         ,@(if after-path
                                               (list after-path)
                                               '())
                                         ,@(if system
                                               (list system)
                                               '())
                                         ,@(if target
                                               (list target)
                                               '())
                                         ,@(if search-query
                                               (list (string-append
                                                      "%" search-query "%"))
                                               '())))))

(define (fix-derivation-output-details-hash-encoding conn)
  (define (find-old-derivations-and-hashes conn)
    (exec-query
     conn
     "
SELECT id, hash
FROM derivation_output_details
WHERE hash_algorithm = 'sha256' AND char_length(hash) = 52 LIMIT 100"))

  (define (fix-batch data)
    (for-each
     (match-lambda
       ((id base32-hash)
        (exec-query
         conn
         "
UPDATE derivation_output_details
SET hash = $2
WHERE id = $1"
         (list id
               (bytevector->base16-string
                (nix-base32-string->bytevector base32-hash))))))
     data))

  (unless (null? (find-old-derivations-and-hashes conn))
    (with-postgresql-transaction
     conn
     (lambda (conn)
       (exec-query
        conn
        "
LOCK TABLE ONLY derivation_output_details
  IN SHARE ROW EXCLUSIVE MODE")

       (let loop ((data (find-old-derivations-and-hashes conn)))
         (unless (null? data)
           (fix-batch data)

           (simple-format #t "updated ~A old hashes\n"
                          (length data))

           ;; Recurse in case there are more to fix
           (loop (find-old-derivations-and-hashes conn))))))))

(define (insert-derivation-outputs conn
                                   derivation-id
                                   names-and-derivation-outputs)
  (define (insert-into-derivation-output-details derivation-outputs)
    (string-append
     "INSERT INTO derivation_output_details "
     "(path, hash_algorithm, hash, recursive) VALUES "
     (string-join
      (map
       (match-lambda
         (($ <derivation-output> path hash-algo hash recursive?)
          (string-append
           "("
           (string-join
            (list (quote-string path)
                  (value->quoted-string-or-null
                   (and=> hash-algo symbol->string))
                  (value->quoted-string-or-null
                   (and=> hash bytevector->base16-string))
                  (if recursive? "TRUE" "FALSE"))
            ",")
           ")")))
       derivation-outputs)
      ",")
     " RETURNING id"
     ";"))

  (define (insert-into-derivation-outputs output-names
                                          derivation-output-details-ids)
    (string-append "INSERT INTO derivation_outputs "
                   "(derivation_id, name, derivation_output_details_id) VALUES "
                   (string-join
                    (map (lambda (output-name derivation-output-details-id)
                           (simple-format
                            #f "(~A, '~A', ~A)"
                            derivation-id
                            output-name
                            derivation-output-details-id))
                         output-names
                         derivation-output-details-ids)
                    ",")
                   ";"))

  (define (select-derivation-output-details-sets-id derivation-output-details-ids)
    (match (exec-query
            conn
            (string-append
             "
SELECT id
FROM derivation_output_details_sets
WHERE derivation_output_details_ids = ARRAY["
             (string-join (map number->string
                               derivation-output-details-ids)
                          ",")
             "]"))
      (((id))
       (string->number id))
      (_ #f)))

  (define (insert-into-derivation-output-details-sets
           derivation-output-details-ids)
    (match (exec-query
            conn
            (string-append
             "
INSERT INTO derivation_output_details_sets (derivation_output_details_ids)
VALUES (ARRAY["
             (string-join (map number->string derivation-output-details-ids)
                          ",")
             "])
RETURNING id"))
      (((id))
       (string->number id))))

  (define (insert-into-derivations-by-output-details-set
           derivation_output_details_set_id)
    (exec-query
     conn
     "
INSERT INTO derivations_by_output_details_set
  (derivation_id, derivation_output_details_set_id)
VALUES ($1, $2)"
     (list (number->string derivation-id)
           (number->string derivation_output_details_set_id))))

  (let* ((derivation-outputs (map cdr names-and-derivation-outputs))
         (derivation-output-paths (map derivation-output-path
                                       derivation-outputs))

         (existing-derivation-output-details-entries
          (exec-query->vhash
           conn
           (select-from-derivation-output-details
            derivation-output-paths)
           second ;; path
           first)) ;; id

         (missing-entries (filter
                           (lambda (derivation-output)
                             (not (vhash-assoc
                                   (derivation-output-path derivation-output)
                                   existing-derivation-output-details-entries)))
                           derivation-outputs))

         (new-derivation-output-details-ids
          (if (null? missing-entries)
              '()
              (map car
                   (exec-query
                    conn
                    (insert-into-derivation-output-details missing-entries)))))

         (new-entries-id-lookup-vhash
          (two-lists->vhash (map derivation-output-path missing-entries)
                            new-derivation-output-details-ids))

         (derivation-output-details-ids
          (map (lambda (path)
                 (string->number
                  (cdr
                   (or (vhash-assoc path
                                    existing-derivation-output-details-entries)
                       (vhash-assoc path
                                    new-entries-id-lookup-vhash)
                       (error "missing derivation output details entry")))))
               derivation-output-paths))

         (derivation-output-names
          (map car names-and-derivation-outputs)))

    (exec-query conn
                (insert-into-derivation-outputs derivation-output-names
                                                derivation-output-details-ids))

    (insert-into-derivations-by-output-details-set
     (or
      (select-derivation-output-details-sets-id derivation-output-details-ids)
      (insert-into-derivation-output-details-sets derivation-output-details-ids)))

    derivation-output-details-ids))

(define (select-derivation-by-file-name-hash conn file-name-hash)
  (define query
    (string-append
     "SELECT id, file_name, builder, args, to_json(env_vars), system "
     "FROM derivations "
     "WHERE substring(file_name from 12 for 32) = $1"))

  (match (exec-query conn query (list file-name-hash))
    (()
     #f)
    (((id file_name builder args env_vars system))
     (list (string->number id)
           file_name
           builder
           (parse-postgresql-array-string args)
           (map (match-lambda
                  (#(key value)
                   `((key . ,key)
                     (value . ,value))))
                (vector->list (json-string->scm env_vars)))
           system))))

(define (select-derivation-by-file-name conn file-name)
  (define query
    (string-append
     "SELECT id, file_name, builder, args, to_json(env_vars), system "
     "FROM derivations "
     "WHERE file_name = $1"))

  (match (exec-query conn query (list file-name))
    (()
     #f)
    (((id file_name builder args env_vars system))
     (list (string->number id)
           file_name
           builder
           (parse-postgresql-array-string args)
           (map (match-lambda
                  (#(key value)
                   `((key . ,key)
                     (value . ,value))))
                (vector->list (json-string->scm env_vars)))
           system))))

(define select-derivation-output-id
  (mlambda (conn name path)
    (match (exec-query
            conn
            (string-append
             "SELECT derivation_outputs.id FROM derivation_outputs "
             "INNER JOIN derivations ON "
             "derivation_outputs.derivation_id = derivations.id "
             "WHERE derivations.file_name = '" path "' "
             "AND derivation_outputs.name = '" name "';"))
      (((id))
       id)
      (()
       (error (simple-format
               #f "cannot find derivation-output with name ~A and path ~A"
               name path))))))

(define (select-derivation-outputs-by-derivation-id conn id)
  (define query
    (string-append
     "
SELECT derivation_outputs.name, derivation_output_details.path,
  derivation_output_details.hash_algorithm, derivation_output_details.hash,
  derivation_output_details.recursive
FROM derivation_outputs
INNER JOIN derivation_output_details ON
  derivation_outputs.derivation_output_details_id = derivation_output_details.id
WHERE derivation_id = $1
ORDER BY derivation_outputs.name"))

  (map
   (match-lambda
     ((name path hash_algorithm hash recursive)
      (list name
            path
            hash_algorithm
            hash
            (string=? recursive "t"))))
   (exec-query conn query (list (number->string id)))))

(define (select-derivation-inputs-by-derivation-id conn id)
  (define query
    (string-append
     "
SELECT derivations.file_name,
       JSON_AGG(
         json_build_object(
           'output_name', derivation_outputs.name,
           'store_filename', derivation_output_details.path
         )
         ORDER BY derivation_outputs.name
       )
FROM derivation_inputs
INNER JOIN derivation_outputs
  ON derivation_outputs.id = derivation_inputs.derivation_output_id
INNER JOIN derivation_output_details
  ON derivation_outputs.derivation_output_details_id = derivation_output_details.id
INNER JOIN derivations
  ON derivation_outputs.derivation_id = derivations.id
WHERE derivation_inputs.derivation_id = $1
GROUP BY derivations.file_name
ORDER BY derivations.file_name"))

  (map (match-lambda
         ((derivation-file-name outputs-json)
          (list derivation-file-name
                (json-string->scm outputs-json))))
       (exec-query conn query (list (number->string id)))))

(define (select-derivation-sources-by-derivation-id conn id)
  (define query
    (string-append
     "
SELECT derivation_source_files.store_path
FROM derivation_source_files
INNER JOIN derivation_sources
  ON derivation_source_files.id = derivation_sources.derivation_source_file_id
WHERE derivation_sources.derivation_id = $1
ORDER BY 1"))

  (map first
       (exec-query conn query (list (number->string id)))))

(define (select-derivation-references-by-derivation-id conn id)
  (define query
    (string-append
     "
SELECT * FROM (
    SELECT derivation_source_files.store_path
    FROM derivation_source_files
    INNER JOIN derivation_sources
      ON derivation_source_files.id = derivation_sources.derivation_source_file_id
    WHERE derivation_sources.derivation_id = $1
  UNION ALL
    SELECT derivations.file_name
    FROM derivation_inputs
    INNER JOIN derivation_outputs
      ON derivation_outputs.id = derivation_inputs.derivation_output_id
    INNER JOIN derivation_output_details
      ON derivation_outputs.derivation_output_details_id = derivation_output_details.id
    INNER JOIN derivations
      ON derivation_outputs.derivation_id = derivations.id
    WHERE derivation_inputs.derivation_id = $1
    GROUP BY derivations.file_name
) AS data
ORDER BY 1"))

  (map first
       (exec-query conn query (list (number->string id)))))

(define (select-derivation-source-file-by-store-path conn store-path)
  (define query
    "
SELECT id
FROM derivation_source_files
WHERE store_path = $1")

  (map car (exec-query conn query (list store-path))))

(define (select-derivation-source-file-data-by-file-name-hash conn hash)
  (match (exec-query
          conn
          "
SELECT derivation_source_files.store_path,
       derivation_source_file_nars.compression,
       length(derivation_source_file_nars.data) AS compressed_size,
       derivation_source_file_nars.hash_algorithm,
       derivation_source_file_nars.hash,
       derivation_source_file_nars.uncompressed_size
FROM derivation_source_file_nars
INNER JOIN derivation_source_files
  ON derivation_source_file_nars.derivation_source_file_id =
     derivation_source_files.id
WHERE substring(derivation_source_files.store_path from 12 for 32) = $1"
          (list hash))
    (((store_path compression compressed_size hash_algorithm hash uncompressed_size))
     (list store_path
           compression
           (string->number compressed_size)
           hash_algorithm
           hash
           (string->number uncompressed_size)))
    (() #f)))

(define (select-derivation-source-file-nar-details-by-file-name conn file-name)
  (match (exec-query
          conn
          "
SELECT compression, hash_algorithm, hash,
       uncompressed_size, length(data) AS compressed_size
FROM derivation_source_file_nars
INNER JOIN derivation_source_files
  ON derivation_source_file_nars.derivation_source_file_id =
     derivation_source_files.id
WHERE derivation_source_files.store_path = $1"
          (list file-name))
    (((compression hash_algorithm hash uncompressed_size compressed_size))
     `((compression       . ,compression)
       (hash_algorithm    . ,hash_algorithm)
       (hash              . ,hash)
       (uncompressed_size . ,(string->number uncompressed_size))
       (compressed_size   . ,(string->number compressed_size))))
    (() #f)))

(define (select-derivation-source-file-nar-data-by-file-name conn file-name)
  (match (exec-query
          conn
          "
SELECT data
FROM derivation_source_file_nars
INNER JOIN derivation_source_files
  ON derivation_source_file_nars.derivation_source_file_id =
     derivation_source_files.id
WHERE derivation_source_files.store_path = $1"
          (list file-name))
    (((data))
     (base16-string->bytevector
      ;; Drop \x from the start of the string
      (string-drop data 2)))
    (() #f)))

(define (select-serialized-derivation-by-file-name conn derivation-file-name)
  (define (double-quote s)
    (string-append
     "\"" s "\""))

  (define (round-brackets-list items)
    (string-append
     "("
     (string-join items ",")
     ")"))

  (define (square-brackets-list items)
    (string-append
     "["
     (string-join items ",")
     "]"))

  (let ((derivation (select-derivation-by-file-name conn
                                                    derivation-file-name)))
    (if derivation
        (let ((derivation-inputs (select-derivation-inputs-by-derivation-id
                                  conn
                                  (first derivation)))
              (derivation-outputs (select-derivation-outputs-by-derivation-id
                                   conn
                                   (first derivation)))
              (derivation-sources (select-derivation-sources-by-derivation-id
                                   conn
                                   (first derivation))))
          (string-append
           "Derive"
           (round-brackets-list
            `(;; Outputs
              ,(square-brackets-list
                (map (match-lambda
                       ((output-name path hash-algorithm hash recursive?)
                        (round-brackets-list
                         (list
                          (double-quote output-name)
                          (double-quote path)
                          (double-quote
                           (string-append
                            (if recursive? "r:" "")
                            (or hash-algorithm "")))
                          (double-quote (or hash ""))))))
                     derivation-outputs))
              ;; Inputs
              ,(square-brackets-list
                (map (match-lambda
                       ((file-name outputs)
                        (round-brackets-list
                         (list
                          (double-quote file-name)
                          (square-brackets-list
                           (map (lambda (output)
                                  (double-quote
                                   (assoc-ref output "output_name")))
                                (vector->list outputs)))))))
                     derivation-inputs))
              ;; Sources
              ,(square-brackets-list
                (map double-quote derivation-sources))
              ;; Other parts
              ,@(match derivation
                  ((id file-name builder args env-vars system)
                   (list
                    (double-quote system)
                    (double-quote builder)
                    (square-brackets-list
                     (map double-quote args))
                    (square-brackets-list
                     (map (lambda (env-var)
                            (round-brackets-list
                             (list (with-output-to-string
                                     (lambda ()
                                       (write (assq-ref env-var 'key))))
                                   (with-output-to-string
                                     (lambda ()
                                       (write (assq-ref env-var 'value)))))))
                          env-vars)))))))))
        #f)))

(define (insert-derivation-inputs conn derivation-id derivation-inputs)
  (define (insert-into-derivation-inputs output-ids)
    (string-append "INSERT INTO derivation_inputs "
                   "(derivation_id, derivation_output_id) VALUES "
                   (string-join
                    (map (lambda (output-id)
                           (simple-format
                            #f "(~A, ~A)"
                            derivation-id output-id))
                         output-ids)
                    ",")
                   ";"))

  (unless (null? derivation-inputs)
    (exec-query
     conn
     (insert-into-derivation-inputs
      (append-map
       (match-lambda
         (($ <derivation-input> derivation-or-path sub-derivations)
          (let ((path
                 (match derivation-or-path
                   ((? derivation? d)
                    ;; The first field changed to a derivation (from the file
                    ;; name) in 5cf4b26d52bcea382d98fb4becce89be9ee37b55
                    (derivation-file-name d))
                   ((? string? s)
                    s))))
            (map (lambda (sub-derivation)
                   (select-derivation-output-id conn
                                                sub-derivation
                                                path))
                 sub-derivations))))
       derivation-inputs)))))

(define (select-from-derivation-source-files store-paths)
  (string-append
   "SELECT id, store_path FROM derivation_source_files "
   "WHERE store_path IN ("
   (string-join (map quote-string store-paths)
                ",")
   ");"))

(define (insert-derivation-sources conn derivation-id sources)
  (define (insert-into-derivation-sources derivation-source-file-ids)
    (string-append
     "INSERT INTO derivation_sources "
     "(derivation_id, derivation_source_file_id) VALUES "
     (string-join
      (map (lambda (derivation-source-file-id)
             (simple-format
              #f "(~A, ~A)" derivation-id derivation-source-file-id))
           derivation-source-file-ids)
      ",")
     ";"))

  (let ((sources-ids
         (insert-missing-data-and-return-all-ids
          conn
          "derivation_source_files"
          '(store_path)
          (map list sources))))

    (exec-query conn
                (insert-into-derivation-sources sources-ids))

    sources-ids))

(define (insert-derivation-source-file-nar conn id source-file)
  (define missing?
    (match (exec-query
            conn
            "SELECT 1 FROM derivation_source_file_nars WHERE derivation_source_file_id = $1"
            (list (number->string id)))
      (() #t)
      (_ #f)))

  (when missing?
    (let* ((nar-bytevector   (call-with-values
                                 (lambda ()
                                   (open-bytevector-output-port))
                               (lambda (port get-bytevector)
                                 (write-file source-file port)
                                 (get-bytevector))))
           (data-string       (bytevector->base16-string
                               (call-with-values
                                   (lambda ()
                                     (open-bytevector-output-port))
                                 (lambda (port get-bytevector)
                                   (call-with-lzip-output-port port
                                     (lambda (port)
                                       (put-bytevector port nar-bytevector))
                                     #:level 9)
                                   (get-bytevector)))))
           (hash              (bytevector->nix-base32-string
                               (sha256 nar-bytevector)))
           (uncompressed-size (bytevector-length nar-bytevector)))
      (exec-query
       conn
       "
INSERT INTO derivation_source_file_nars (
  derivation_source_file_id,
  compression,
  hash_algorithm,
  hash,
  uncompressed_size,
  data
) VALUES ($1, $2, $3, $4, $5, $6)"
       (list (number->string id)
             "lzip"
             "sha256"
             hash
             (number->string uncompressed-size)
             (string-append "\\x" data-string))))))

(define* (backfill-derivation-source-file-nars conn #:key
                                               (batch-size 10000)
                                               (loop? #t))
  (define (missing-batch)
    (exec-query
     conn
     "
SELECT id, store_path
FROM derivation_source_files
WHERE id NOT IN (
  SELECT derivation_source_file_id FROM derivation_source_file_nars
)
LIMIT $1"
     (list (number->string batch-size))))

  (let loop ((batch (missing-batch)))
    (unless (null? batch)
      (for-each
       (match-lambda
         ((id source-file)
          (if (file-exists? source-file)
              (begin
                (insert-derivation-source-file-nar conn
                                                   (string->number id)
                                                   source-file)
                (simple-format #t "inserting ~A\n" source-file))
              (simple-format #t "missing ~A\n" source-file))))
       batch)
      (when loop? (loop (missing-batch))))))

(define (insert-missing-derivations conn
                                    derivation-ids-hash-table
                                    derivations)
  (define (ensure-input-derivations-exist input-derivation-file-names)
    (unless (null? input-derivation-file-names)
      (simple-format
       #t "debug: ensure-input-derivations-exist: processing ~A derivations\n"
       (length input-derivation-file-names))
      (let ((existing-derivation-entries
             (derivation-file-names->vhash conn
                                           derivation-ids-hash-table
                                           input-derivation-file-names)))
        (simple-format
         #t
         "debug: ensure-input-derivations-exist: checking for missing input derivations\n")
        (let ((missing-derivations-filenames
               (filter (lambda (derivation-file-name)
                         (not (vhash-assoc derivation-file-name
                                           existing-derivation-entries)))
                       input-derivation-file-names)))

          (unless (null? missing-derivations-filenames)
            (simple-format
             #f
             "debug: ensure-input-derivations-exist: inserting missing input derivations\n")
            ;; Ensure all the input derivations exist
            (insert-missing-derivations
             conn
             derivation-ids-hash-table
             (map read-derivation-from-file
                  missing-derivations-filenames)))))))

  (define (insert-into-derivations)
    (string-append
     "INSERT INTO derivations "
     "(file_name, builder, args, env_vars, system) VALUES "
     (string-join
      (map (match-lambda
             (($ <derivation> outputs inputs sources
                              system builder args env-vars file-name)
              (simple-format
               #f "('~A', '~A', ARRAY[~A]::varchar[], ARRAY[~A], '~A')"
               file-name
               builder
               (string-join (map quote-string args) ",")
               (string-join (map (match-lambda
                                   ((key . value)
                                    (string-append
                                     "['" key '"', $$"
                                     value "$$ ]")))
                                 env-vars)
                            ",")
               system)))
           derivations)
      ",")
     " RETURNING id"
     ";"))

  (simple-format
   #t "debug: insert-missing-derivations: inserting ~A derivations\n"
   (length derivations))
  (let ((derivation-ids
         (map (lambda (result)
                (string->number (car result)))
              (exec-query conn (insert-into-derivations)))))

    (simple-format
     #t "debug: insert-missing-derivations: updating hash table\n")
    (for-each (lambda (derivation derivation-id)
                (hash-set! derivation-ids-hash-table
                           (derivation-file-name derivation)
                           derivation-id))
              derivations
              derivation-ids)

    (simple-format
     #t "debug: insert-missing-derivations: inserting outputs\n")
    (for-each (lambda (derivation-id derivation)
                (insert-derivation-outputs conn
                                           derivation-id
                                           (derivation-outputs derivation)))
              derivation-ids
              derivations)

    (simple-format
     #t "debug: insert-missing-derivations: inserting sources\n")
    (for-each (lambda (derivation-id derivation)
                (let* ((sources (derivation-sources derivation))
                       (sources-ids
                        (insert-derivation-sources conn
                                                   derivation-id
                                                   (derivation-sources derivation))))
                  (map (lambda (id source-file)
                         (insert-derivation-source-file-nar conn
                                                            id
                                                            source-file))
                       sources-ids
                       sources)))
              derivation-ids
              derivations)

    (simple-format
     #t "debug: insert-missing-derivations: ensure-input-derivations-exist\n")

    (ensure-input-derivations-exist (deduplicate-strings
                                     (map derivation-input-path
                                          (append-map
                                           derivation-inputs
                                           derivations))))

    (simple-format
     #t "debug: insert-missing-derivations: inserting inputs\n")
    (for-each (lambda (derivation-id derivation)
                (insert-derivation-inputs conn
                                          derivation-id
                                          (derivation-inputs derivation)))

              derivation-ids
              derivations)

    derivation-ids))

(define (select-derivations-by-id conn ids)
  (define query
    (string-append "SELECT id, file_name "
                   "FROM derivations "
                   "WHERE id IN "
                   "(" (string-join (map (lambda (id)
                                           (simple-format #f "'~A'" id))
                                         ids)
                                    ",")
                   ");"))

  (exec-query conn query))

(define* (select-derivations-and-build-status conn #:key
                                              file-names
                                              systems
                                              targets
                                              build-statuses)
  (define criteria
    (string-join
     (filter-map
      (lambda (field values)
        (if values
            (string-append
             field " IN ("
             (string-join (map (lambda (value)
                                 (simple-format #f "'~A'" value))
                               values)
                          ",")
             ")")
            #f))
      '("derivations.file_name"
        "derivations.system"
        "target"
        "latest_build_status.status")
      (list (deduplicate-strings file-names)
            systems
            targets
            build-statuses))
     " AND "))

  (define query
    (string-append
     "
SELECT
  derivations.file_name,
  derivations.system,
  package_derivations.target,
  latest_build_status.status
FROM derivations
INNER JOIN package_derivations
  ON derivations.id = package_derivations.derivation_id
INNER JOIN derivations_by_output_details_set
  ON derivations.id = derivations_by_output_details_set.derivation_id
LEFT OUTER JOIN builds
  ON derivations.derivation_output_details_set_id =
     builds.derivation_output_details_set_id
LEFT OUTER JOIN latest_build_status
  ON builds.id = latest_build_status.build_id
WHERE " criteria ";"))

  (exec-query conn query))

(define (deduplicate-derivations derivations)
  (define sorted-derivations
    (sort derivations
          (lambda (a b)
            (string<? (derivation-file-name a)
                      (derivation-file-name b)))))

  (pair-fold
   (match-lambda*
     (((x) result)
      (cons x result))
     (((x y rest ...) result)
      (if (string=? (derivation-file-name x)
                    (derivation-file-name y))
          result
          (cons x result))))
   '()
   sorted-derivations))

(define (derivation-file-names->vhash conn derivation-ids-hash-table file-names)
  (simple-format #t "debug: derivation-file-names->vhash: ~A file-names\n"
                 (length file-names))
  (match (fold (match-lambda*
                 ((file-name (result . missing-file-names))
                  (let ((cached-id (hash-ref derivation-ids-hash-table
                                             file-name)))
                    (if cached-id
                        (cons (vhash-cons file-name cached-id result)
                              missing-file-names)
                        (cons result
                              (cons file-name missing-file-names))))))
               (cons vlist-null '())
               file-names)
    ((result)
     (simple-format
      #t "debug: derivation-file-names->vhash: lookup ~A file-names, all found\n"
      (length file-names))
     result)
    ((result . missing-file-names)
     (simple-format
      #t "debug: derivation-file-names->vhash: lookup ~A file-names, ~A not cached\n"
      (length file-names) (length missing-file-names))
     (let ((result-for-missing-file-names
            (exec-query->vhash
             conn
             (select-existing-derivations missing-file-names)
             second ;; file_name
             (lambda (result)
               (string->number (first result)))))) ;; id
       (simple-format
        #t "debug: derivation-file-names->vhash: adding ~A entries to the cache\n"
        (vlist-length result-for-missing-file-names))
       (vhash-fold
        (lambda (key value combined)
          ;; Update the derivation-ids-hash-table as we go through the vhash
          (hash-set! derivation-ids-hash-table key value)
          (vhash-cons key value combined))
        result
        result-for-missing-file-names)))))

(define (derivation-file-names->derivation-ids conn derivation-file-names)
  (if (null? derivation-file-names)
      '()
      (let* ((derivations-count (length derivation-file-names))
             (derivation-ids-hash-table (make-hash-table derivations-count)))
        (simple-format
         #t "debug: derivation-file-names->derivation-ids: processing ~A derivations\n"
         derivations-count)
        (let* ((existing-derivation-entries
                (derivation-file-names->vhash conn
                                              derivation-ids-hash-table
                                              derivation-file-names))

               (missing-derivations
                (map read-derivation-from-file
                     (deduplicate-strings
                      (filter (lambda (derivation-file-name)
                                (not (vhash-assoc derivation-file-name
                                                  existing-derivation-entries)))
                              derivation-file-names))))

               (new-derivation-entries
                (if (null? missing-derivations)
                    '()
                    (insert-missing-derivations conn
                                                derivation-ids-hash-table
                                                missing-derivations)))

               (new-entries-id-lookup-vhash
                (two-lists->vhash (map derivation-file-name missing-derivations)
                                  new-derivation-entries)))

          (map (lambda (derivation-file-name)
                 (cdr
                  (or (vhash-assoc derivation-file-name
                                   existing-derivation-entries)
                      (vhash-assoc derivation-file-name
                                   new-entries-id-lookup-vhash)
                      (error "missing derivation id"))))
               derivation-file-names)))))
