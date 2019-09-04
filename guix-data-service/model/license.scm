(define-module (guix-data-service model license)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
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

(define (inferior-packages->license-id-lists conn inf packages)
  (define license-data
    (inferior-packages->license-data inf packages))

  (define (string-or-null v)
    (if (string? v)
        v
        ;; save non string values as NULL
        NULL))

  (insert-missing-data-and-return-all-ids
   conn
   "licenses"
   `(name uri comment)
   (map (lambda (license-tuples)
          (map
           (match-lambda
             ((name uri comment)
              (list name
                    (string-or-null uri)
                    (string-or-null comment))))
           license-tuples))
        license-data)
   #:sets-of-data? #t))
