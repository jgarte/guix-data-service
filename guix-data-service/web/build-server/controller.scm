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

(define-module (guix-data-service web build-server controller)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (json)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service web render)
  #:use-module (guix-data-service web query-parameters)
  #:use-module (guix-data-service jobs load-new-guix-revision)
  #:use-module (guix-data-service model build)
  #:use-module (guix-data-service model build-server)
  #:use-module (guix-data-service model build-status)
  #:use-module (guix-data-service model nar)
  #:use-module (guix-data-service model build-server-token-seed)
  #:use-module (guix-data-service web util)
  #:use-module (guix-data-service web view html)
  #:use-module (guix-data-service web jobs html)
  #:use-module (guix-data-service web build-server html)
  #:export (build-server-controller))

(define (render-build mime-types
                      conn
                      build-server-id
                      query-parameters)
  (if (any-invalid-query-parameters? query-parameters)
      (case (most-appropriate-mime-type
             '(application/json text/html)
             mime-types)
        ((application/json)
         (render-json
          `((error . "invalid query"))))
        (else
         (render-html
          #:sxml (view-build query-parameters
                             #f
                             #f))))
      (let* ((derivation-file-name
              (assq-ref query-parameters 'derivation_file_name))
             (build-server-build-id
              (assq-ref query-parameters 'build_server_build_id))
             (build
              (if build-server-build-id
                  (select-build-by-build-server-and-build-server-build-id
                   conn
                   build-server-id
                   build-server-build-id)
                  (select-build-by-build-server-and-derivation-file-name
                   conn
                   build-server-id
                   derivation-file-name))))
        (if build
            (render-html
             #:sxml
             (view-build query-parameters
                         build
                         (if (member
                              (assoc-ref (last (vector->list (third build)))
                                         "status")
                              '("failed-dependency"
                                "scheduled")) ; scheduled, because the
                                              ; guix-build-coordinator doesn't
                                              ; mark builds as
                                              ; failed-dependency
                             (select-required-builds-that-failed
                              conn
                              build-server-id
                              derivation-file-name)
                             #f)))
            (render-html
             #:sxml (general-not-found
                     "Build not found"
                     "No build found for this build server and derivation.")
             #:code 404)))))

(define (render-build-servers mime-types
                              build-servers)
  (render-html
   #:sxml
   (view-build-servers build-servers)))

(define (render-build-server mime-types
                             build-server)
  (render-html
   #:sxml
   (view-build-server build-server)))

(define (handle-build-event-submission parsed-query-parameters
                                       build-server-id-string
                                       body
                                       conn
                                       secret-key-base)
  (define build-server-id
    (string->number build-server-id-string))

  (define (handle-derivation-events items)
    (unless (null? items)
      (let ((build-ids
             (insert-builds conn
                            build-server-id
                            (map (lambda (item)
                                   (assoc-ref item "derivation"))
                                 items)
                            (map (lambda (item)
                                   (assoc-ref item "build_id"))
                                 items))))
        (insert-build-statuses
         conn
         build-ids
         (map
          (lambda (item-data)
            (list (assoc-ref item-data "timestamp")
                  (assoc-ref item-data "event")))
          items)))))

  (define (process-items items)
    (with-postgresql-transaction
     conn
     (lambda (conn)
       (handle-derivation-events
        (filter (lambda (item)
                  (let ((type (assoc-ref item "type")))
                    (if type
                        (string=? type "build")
                        (begin
                          (simple-format (current-error-port)
                                         "warning: unknown type for event: ~A\n"
                                         item)
                          #f))))
                items)))))

  (if (any-invalid-query-parameters? parsed-query-parameters)
      (render-json
       '((error . "no token provided"))
       #:code 400)
      (let ((provided-token (assq-ref parsed-query-parameters 'token))
            (permitted-tokens (compute-tokens-for-build-server
                               conn
                               secret-key-base
                               build-server-id)))
        (if (member provided-token
                    (map cdr permitted-tokens)
                    string=?)
            (catch
              'json-invalid
              (lambda ()
                (let ((body-string (utf8->string body)))
                  (let* ((body-json (json-string->scm body-string))
                         (items (and=> (assoc-ref body-json "items")
                                       vector->list)))
                    (cond
                     ((eq? items #f)
                      (render-json
                       '((error . "missing items key"))
                       #:code 400))
                     ((null? items)
                      (render-json
                       '((error . "no items to process"))
                       #:code 400))
                     (else
                      (catch
                        #t
                        (lambda ()
                          (process-items items)
                          (no-content))
                        (lambda (key . args)
                          (simple-format (current-error-port)
                                         "error processing events: ~A: ~A\n"
                                         key
                                         args)
                          (for-each (lambda (item)
                                      (simple-format (current-error-port)
                                                     "  ~A\n" item))
                                    items)
                          (render-json
                           '((error . "could not process events"))
                           #:code 500))))))))
              (lambda (key . args)
                (render-json
                 '((error . "could not parse body as JSON"))
                 #:code 400)))
            (render-json
             '((error . "error"))
             #:code 403)))))

(define (handle-signing-key-request conn id)
  (render-html
   #:sxml (view-signing-key
           (select-signing-key conn id))))

(define (build-server-controller request
                                 method-and-path-components
                                 mime-types
                                 body
                                 conn
                                 secret-key-base)
  (match method-and-path-components
    (('GET "build-servers")
     (let ((build-servers (select-build-servers conn)))
       (render-build-servers mime-types
                             build-servers)))
    (('GET "build-server" build-server-id)
     (let ((build-server (select-build-server conn (string->number
                                                    build-server-id))))
       (if build-server
           (render-build-server mime-types
                                build-server)
           (general-not-found "Build server not found" ""))))
    (('GET "build-server" build-server-id "build")
     (let ((parsed-query-parameters
            (parse-query-parameters
             request
             `((derivation_file_name  ,identity)
               (build_server_build_id ,identity)))))
       (render-build mime-types
                     conn
                     (string->number build-server-id)
                     parsed-query-parameters)))
    (('POST "build-server" build-server-id "build-events")
     (let ((parsed-query-parameters
            (parse-query-parameters
             request
             `((token ,identity #:required)))))
       (handle-build-event-submission parsed-query-parameters
                                      build-server-id
                                      body
                                      conn
                                      secret-key-base)))
    (('GET "build-server" "signing-key" id)
     (handle-signing-key-request conn
                                 (string->number id)))
    (_ #f)))
