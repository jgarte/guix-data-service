(define-module (guix-data-service model license)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (squee)
  #:use-module (guix inferior)
  #:use-module (guix-data-service model utils)
  #:export (inferior-packages->license-id-lists))

(define inferior-package-id
  (@@ (guix inferior) inferior-package-id))

(define (inferior-packages->license-data inf packages)
  (define (proc packages)
    `(map (lambda (inferior-package-id)
            (let ((package (hashv-ref %package-table inferior-package-id)))
              (match (package-license package)
                ((? license? license)
                 (list
                  (list (license-name license)
                        (license-uri license)
                        (license-comment license))))
                ((values ...)
                 (map (match-lambda
                        ((? license? license)
                         (list (license-name license)
                               (license-uri license)
                               (license-comment license)))
                        (x
                         (simple-format
                          (current-error-port)
                          "error: unknown license value ~A for package ~A"
                          x package)
                         '()))
                      values))
                (x
                 (simple-format
                  (current-error-port)
                  "error: unknown license value ~A for package ~A"
                  x package)
                 '()))))
          (list ,@(map inferior-package-id packages))))

  (inferior-eval '(use-modules (guix licenses)) inf)
  (inferior-eval (proc packages) inf))

(define (insert-licenses values)
  (string-append
   "INSERT INTO licenses "
   "(name, uri, comment) "
   "VALUES "
   (string-join
    (map (lambda (license-values)
           (string-append
            "("
            (string-join
             (map value->quoted-string-or-null
                  license-values)
             ", ")
            ")"))
         values)
    ", ")
   " RETURNING id"))

(define (inferior-packages->license-id-lists conn inf packages)
  (define license-data
    (inferior-packages->license-data inf packages))

  (define (sort-license-ids ids)
    (map number->string
         (sort (map string->number ids) <)))

  (define (non-string-to-false lst)
    (map (lambda (value)
           (if (string? value)
               value
               #f))
         lst))

  (define (empty-string-to-false lst)
    ;; TODO squee returns empty strings for null values, which will probably
    ;; cause problems
    (map (lambda (value)
           (if (string? value)
               (if (string-null? value)
                   #f
                   value)
               value))
         lst))

  (let* ((unique-license-tuples
          (filter (lambda (license-tuple)
                    (not (null? license-tuple)))
                  (delete-duplicates
                   (map
                    (lambda (lst)
                      (non-string-to-false
                       (empty-string-to-false lst)))
                    (concatenate license-data)))))
         (existing-license-entries
          (exec-query->vhash conn
                             "SELECT id, name, uri, comment FROM licenses"
                             (lambda (vals)
                               (non-string-to-false
                                (empty-string-to-false (cdr vals))))
                             first)) ;; id
         (missing-license-entries
          (delete-duplicates
           (filter (lambda (values)
                     (not (vhash-assoc values
                                       existing-license-entries)))
                   unique-license-tuples)))
         (new-license-entries
          (if (null? missing-license-entries)
              '()
              (map first
                   (exec-query conn
                               (insert-licenses missing-license-entries)))))
         (new-entries-id-lookup-vhash
          (two-lists->vhash missing-license-entries
                            new-license-entries)))

    (map (lambda (license-value-lists)
           (sort-license-ids
            (map (lambda (license-values)
                   (cdr
                    (or (vhash-assoc license-values
                                     existing-license-entries)
                        (vhash-assoc license-values
                                     new-entries-id-lookup-vhash)
                        (begin
                          (error "missing license entry"
                                 license-values)))))
                 (map (lambda (lst)
                        (non-string-to-false
                         (empty-string-to-false lst)))
                      license-value-lists))))
         license-data)))
