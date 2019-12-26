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
