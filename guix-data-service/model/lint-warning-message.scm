(define-module (guix-data-service model lint-warning-message)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service model utils)
  #:export (lint-warning-message-data->lint-warning-message-ids

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
                 (sort (map string->number lint-message-ids) <))
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
            (string-join lint-warning-message-ids ", ")
            "]"))))

    (match lint-message-set-id
      (((id)) id)
      (()
       (insert-lint-warning-message-set conn lint-warning-message-ids)))))
