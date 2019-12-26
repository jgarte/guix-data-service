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

(define-module (guix-data-service model package-derivation)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix-data-service model utils)
  #:export (insert-package-derivations
            count-packages-derivations-in-revision))

(define (insert-package-derivations conn
                                    package-ids-systems-and-targets
                                    derivation-ids)
  (define data-4-tuples
    (map (match-lambda*
           (((package-id system target) derivation-id)
            (list package-id
                  derivation-id
                  system
                  target)))
         package-ids-systems-and-targets
         derivation-ids))

  (if (null? data-4-tuples)
      '()
      (insert-missing-data-and-return-all-ids
       conn
       "package_derivations"
       '(package_id derivation_id system target)
       data-4-tuples)))

(define (count-packages-derivations-in-revision conn commit-hash)
  (define query
    "
SELECT package_derivations.system, package_derivations.target,
COUNT(DISTINCT package_derivations.derivation_id)
FROM package_derivations
WHERE package_derivations.id IN (
 SELECT guix_revision_package_derivations.package_derivation_id
 FROM guix_revision_package_derivations
 INNER JOIN guix_revisions
   ON guix_revision_package_derivations.revision_id = guix_revisions.id
 WHERE guix_revisions.commit = $1
)
GROUP BY package_derivations.system, package_derivations.target
ORDER BY package_derivations.system DESC, package_derivations.target DESC")

  (exec-query conn query (list commit-hash)))
