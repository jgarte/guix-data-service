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

(define-module (guix-data-service model guix-revision)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:export (count-guix-revisions
            most-recent-n-guix-revisions
            commit->revision-id
            insert-guix-revision
            guix-commit-exists?
            guix-revision-exists?
            select-guix-revision-for-branch-and-datetime
            guix-revisions-cgit-url-bases))

(define (count-guix-revisions conn)
  (match (exec-query
          conn
          "SELECT COUNT(*) FROM guix_revisions")
    (((x)) (string->number x))))

(define (most-recent-n-guix-revisions conn n)
  (exec-query conn "SELECT * FROM guix_revisions ORDER BY id DESC LIMIT 10"))

(define (commit->revision-id conn commit)
  (match (exec-query
          conn "SELECT id FROM guix_revisions WHERE commit = $1 LIMIT 1"
          (list commit))
    (((id))
     id)
    (() #f)))

(define (insert-guix-revision conn git-repository-id commit)
  (define insert
    "
INSERT INTO guix_revisions (git_repository_id, commit)
  VALUES ($1, $2) RETURNING id")

  (match (exec-query conn insert (list git-repository-id commit))
    (((id)) id)))

(define (guix-commit-exists? conn commit)
  (define query
    "SELECT EXISTS(SELECT 1 FROM guix_revisions WHERE commit = $1)")

  (let ((result (caar
                 (exec-query conn query (list commit)))))
    (string=? result "t")))

(define (guix-revision-exists? conn git-repository-id commit)
  (define query
    (string-append "SELECT EXISTS("
                   "SELECT 1 FROM guix_revisions WHERE "
                   "git_repository_id = '" git-repository-id "' "
                   "AND commit = '" commit "')"
                   ";"))

  (let ((result (caar
                 (exec-query conn query))))
    (string=? result "t")))

(define (select-guix-revision-for-branch-and-datetime conn branch datetime)
  (define query
    "
SELECT guix_revisions.id,
       guix_revisions.commit,
       guix_revisions.git_repository_id,
       git_branches.datetime
FROM guix_revisions
INNER JOIN git_branches
  ON git_branches.commit = guix_revisions.commit
 AND git_branches.git_repository_id = guix_revisions.git_repository_id
WHERE git_branches.name = $1 AND git_branches.datetime <= $2
ORDER BY git_branches.datetime DESC
LIMIT 1")

  (car
   (exec-query conn query (list branch
                                (date->string datetime "~1 ~3")))))

(define (guix-revisions-cgit-url-bases conn guix-revision-ids)
  (map
   car
   (exec-query
    conn
    (simple-format #f "
SELECT cgit_url_base
FROM git_repositories
WHERE cgit_url_base IS NOT NULL AND id IN (
  SELECT git_repository_id
  FROM guix_revisions
  WHERE id IN (VALUES ~A));"
                   (string-join
                    (map (lambda (id)
                           (string-append "(" id ")"))
                         guix-revision-ids)
                    ",")))))
