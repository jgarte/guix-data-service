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

(define-module (guix-data-service model lint-warning-message)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model utils)
  #:export (lint-warning-message-data->lint-warning-message-ids
            lint-warning-message-locales-for-revision
            lint-warning-message-data->lint-warning-message-set-id))

(define (lint-warning-message-data->lint-warning-message-ids conn
                                                             messages-by-locale)
  (insert-missing-data-and-return-all-ids
   conn
   "lint_warning_messages"
   '(locale message)
   (map (match-lambda
          ((locale . message)
           (list locale message)))
        messages-by-locale)))

(define (insert-lint-warning-message-set conn lint-message-ids)
  (let ((query
         (string-append
          "INSERT INTO lint_warning_message_sets (message_ids) VALUES "
          (string-append
           "('{"
           (string-join
            (map number->string
                 (sort lint-message-ids <))
            ", ")
           "}')")
          " RETURNING id")))

    (match (exec-query conn query)
      (((id)) id))))

(define (lint-warning-message-data->lint-warning-message-set-id
         conn
         messages-by-locale)

  (let* ((lint-warning-message-ids
          (lint-warning-message-data->lint-warning-message-ids
           conn messages-by-locale))
         (lint-message-set-id
          (exec-query
           conn
           (string-append
            "SELECT id FROM lint_warning_message_sets "
            "WHERE message_ids = ARRAY["
            (string-join (map number->string
                              (sort lint-warning-message-ids <)) ", ")
            "]"))))

    (string->number
     (match lint-message-set-id
       (((id)) id)
       (()
        (insert-lint-warning-message-set conn lint-warning-message-ids))))))

(define (lint-warning-message-locales-for-revision conn commit-hash)
  (exec-query
   conn
   "
SELECT DISTINCT lint_warning_messages.locale
FROM lint_warning_messages
INNER JOIN lint_warning_message_sets
  ON lint_warning_messages.id = ANY (lint_warning_message_sets.message_ids)
INNER JOIN lint_warnings
  ON lint_warning_message_sets.id = lint_warnings.lint_warning_message_set_id
INNER JOIN guix_revision_lint_warnings
  ON lint_warnings.id = guix_revision_lint_warnings.lint_warning_id
INNER JOIN guix_revisions
  ON guix_revision_lint_warnings.guix_revision_id = guix_revisions.id
 AND guix_revisions.commit = $1"
   (list commit-hash)))
