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

(define-module (guix-data-service branch-updated-emails)
  #:use-module (email email)
  #:use-module (guix-data-service jobs load-new-guix-revision)
  #:export (enqueue-job-for-email))

(define %repository-url-for-repo
  '(("guix" . "https://git.savannah.gnu.org/git/guix.git")))

(define (enqueue-job-for-email conn email)
  (let* ((headers       (email-headers email))
         (x-git-repo    (assq-ref headers 'x-git-repo))
         (x-git-reftype (assq-ref headers 'x-git-reftype))
         (x-git-refname (assq-ref headers 'x-git-refname))
         (x-git-newrev  (assq-ref headers 'x-git-newrev)))
    (when (and (and (string? x-git-reftype)
                    (string=? x-git-reftype "branch"))
               (and (string? x-git-repo)
                    (string=? x-git-repo "guix"))
               (string? x-git-newrev))
      (enqueue-load-new-guix-revision-job
       conn
       (assoc-ref %repository-url-for-repo
                  x-git-repo)
       x-git-newrev
       (string-append x-git-repo " " x-git-refname " updated")))))
