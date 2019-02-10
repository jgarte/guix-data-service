(define-module (guix-data-service model utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (squee)
  #:export (quote-string
            value->quoted-string-or-null
            exec-query->vhash
            two-lists->vhash))

(define (quote-string s)
  (string-append "'" s "'"))

(define (value->quoted-string-or-null value)
  (if (string? value)
      (string-append "$STR$" value "$STR$")
      "NULL"))

(define (exec-query->vhash conn query field-function value-function)
  (fold (lambda (row result)
          (vhash-cons (field-function row)
                      (value-function row)
                      result))
        vlist-null
        (exec-query conn query)))

(define (two-lists->vhash l1 l2)
  (fold (lambda (key value result)
          (vhash-cons key value result))
        vlist-null
        l1
        l2))
