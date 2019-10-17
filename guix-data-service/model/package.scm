(define-module (guix-data-service model package)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix inferior)
  #:use-module (guix-data-service model utils)
  #:export (select-existing-package-entries
            select-packages-in-revision
            search-packages-in-revision
            count-packages-in-revision
            inferior-packages->package-ids

            select-package-versions-for-revision
            package-versions-for-branch))

(define (select-existing-package-entries package-entries)
  (string-append "SELECT id, packages.name, packages.version, "
                 "packages.package_metadata_id "
                 "FROM packages "
                 "JOIN (VALUES "
                 (string-join (map (lambda (package-entry)
                                     (apply
                                      simple-format
                                      #f "('~A', '~A', ~A)"
                                      package-entry))
                                   package-entries)
                              ", ")
                 ") AS vals (name, version, package_metadata_id) "
                 "ON packages.name = vals.name AND "
                 "packages.version = vals.version AND "
                 "packages.package_metadata_id = vals.package_metadata_id"))

(define* (select-packages-in-revision conn commit-hash
                                      #:key limit-results
                                      after-name)
  (define query
    (string-append "
WITH data AS (
  SELECT packages.name, packages.version, package_metadata.synopsis,
    package_metadata.description, package_metadata.home_page,
    locations.file, locations.line, locations.column_number,
    (SELECT JSON_AGG((license_data.*))
     FROM (
       SELECT licenses.name, licenses.uri, licenses.comment
       FROM licenses
       INNER JOIN license_sets ON licenses.id = ANY(license_sets.license_ids)
       WHERE license_sets.id = package_metadata.license_set_id
       ORDER BY licenses.name
     ) AS license_data
    ) AS licenses
  FROM packages
  INNER JOIN package_metadata
    ON packages.package_metadata_id = package_metadata.id
  LEFT OUTER JOIN locations
    ON package_metadata.location_id = locations.id
  WHERE packages.id IN (
   SELECT package_derivations.package_id
   FROM package_derivations
   INNER JOIN guix_revision_package_derivations
     ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
   INNER JOIN guix_revisions
     ON guix_revision_package_derivations.revision_id = guix_revisions.id
   WHERE guix_revisions.commit = $1
  )
  ORDER BY packages.name, packages.version
), package_names AS (
  SELECT DISTINCT name
  FROM data"
    (if after-name
        "\nWHERE name > $2\n"
        "")
    "  ORDER BY name"
    (if limit-results
        (string-append " LIMIT " (number->string limit-results))
        "")
")
SELECT data.*
FROM data
WHERE data.name IN (SELECT name FROM package_names);"))

  (exec-query conn query
              `(,commit-hash
                ,@(if after-name
                      (list after-name)
                      '()))))

(define* (search-packages-in-revision conn commit-hash
                                      search-query
                                      #:key limit-results)
  (define query
    (string-append
     "
SELECT packages.name,
       packages.version,
       package_metadata.synopsis,
       package_metadata.description,
       package_metadata.home_page,
       locations.file, locations.line, locations.column_number,
       (SELECT JSON_AGG((license_data.*))
        FROM (
          SELECT licenses.name, licenses.uri, licenses.comment
          FROM licenses
          INNER JOIN license_sets ON licenses.id = ANY(license_sets.license_ids)
          WHERE license_sets.id = package_metadata.license_set_id
          ORDER BY licenses.name
        ) AS license_data
       ) AS licenses
FROM packages
INNER JOIN package_metadata
  ON packages.package_metadata_id = package_metadata.id
LEFT OUTER JOIN locations
  ON package_metadata.location_id = locations.id
WHERE packages.id IN (
 SELECT package_derivations.package_id
 FROM package_derivations
 INNER JOIN guix_revision_package_derivations
   ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
 INNER JOIN guix_revisions
   ON guix_revision_package_derivations.revision_id = guix_revisions.id
 WHERE guix_revisions.commit = $1
)
AND to_tsvector(name || ' ' || synopsis) @@ plainto_tsquery($2)
ORDER BY (
           ts_rank_cd(
             to_tsvector(name),
             plainto_tsquery($2),
             2 -- divide rank by the document length
           )
           * 4 -- as the name is more important
         ) +
         ts_rank_cd(
           to_tsvector(synopsis),
           plainto_tsquery($2),
           32 -- divide the rank by itself + 1
         ) DESC,
          -- to make the order stable
         name,
         version
"
     (if limit-results
         (string-append "\nLIMIT " (number->string limit-results))
         "")))

  (exec-query conn query
              (list commit-hash search-query)))

(define (count-packages-in-revision conn commit-hash)
  (define query
    "
SELECT COUNT(DISTINCT packages.name)
FROM packages
WHERE packages.id IN (
 SELECT package_derivations.package_id
 FROM package_derivations
 INNER JOIN guix_revision_package_derivations
   ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
 INNER JOIN guix_revisions
   ON guix_revision_package_derivations.revision_id = guix_revisions.id
 WHERE guix_revisions.commit = $1
)")

  (exec-query conn query (list commit-hash)))

(define (insert-into-package-entries package-entries)
  (string-append "INSERT INTO packages "
                 "(name, version, package_metadata_id) VALUES "
                 (string-join
                  (map
                   (match-lambda
                     ((name version package_metadata_id)
                      (simple-format #f "('~A', '~A', ~A)"
                                     name
                                     version
                                     package_metadata_id)))
                   package-entries)
                  ",")
                 " RETURNING id"
                 ";"))

(define (inferior-packages->package-ids conn package-entries)
  (insert-missing-data-and-return-all-ids
   conn
   "packages"
   '(name version package_metadata_id)
   package-entries))

(define (select-package-versions-for-revision conn
                                              commit
                                              package-name)
  (define query "
SELECT DISTINCT version FROM packages
INNER JOIN package_derivations
  ON packages.id = package_derivations.package_id
INNER JOIN guix_revision_package_derivations
  ON package_derivations.id = guix_revision_package_derivations.package_derivation_id
INNER JOIN guix_revisions
  ON guix_revision_package_derivations.revision_id = guix_revisions.id
WHERE guix_revisions.commit = $1 AND packages.name = $2
ORDER BY version")

  (map
   car
   (exec-query conn query (list commit package-name))))

(define (package-versions-for-branch conn
                                     git-repository-id
                                     branch-name
                                     package-name)
  (exec-query
   conn
   "
SELECT package_version,
       first_guix_revisions.commit AS first_guix_revision_commit,
       first_git_branches.datetime AS first_datetime,
       last_guix_revisions.commit AS last_guix_revision_commit,
       last_git_branches.datetime AS last_datetime
FROM package_versions_by_guix_revision_range
INNER JOIN guix_revisions AS first_guix_revisions
  ON first_guix_revision_id = first_guix_revisions.id
INNER JOIN git_branches AS first_git_branches
  ON first_guix_revisions.git_repository_id = first_git_branches.git_repository_id
 AND first_guix_revisions.commit = first_git_branches.commit
INNER JOIN guix_revisions AS last_guix_revisions
  ON last_guix_revision_id = last_guix_revisions.id
INNER JOIN git_branches AS last_git_branches
  ON last_guix_revisions.git_repository_id = last_git_branches.git_repository_id
 AND last_guix_revisions.commit = last_git_branches.commit
WHERE package_name = $1
AND package_versions_by_guix_revision_range.git_repository_id = $2
AND package_versions_by_guix_revision_range.branch_name = $3
ORDER BY first_datetime DESC, package_version DESC"
   (list package-name
         (number->string git-repository-id)
         branch-name)))

