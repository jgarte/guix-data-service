(define-module (guix-data-service model utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 receive)
  #:use-module (squee)
  #:export (quote-string
            value->quoted-string-or-null
            non-empty-string-or-false
            exec-query->vhash
            two-lists->vhash
            deduplicate-strings
            group-list-by-first-n-fields))

(define (quote-string s)
  (string-append "'" s "'"))

(define (value->quoted-string-or-null value)
  (if (string? value)
      (string-append "$STR$" value "$STR$")
      "NULL"))

(define (non-empty-string-or-false s)
  (if (string? s)
      (if (string-null? s)
          #f
          s)
      #f))

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

(define (deduplicate-strings strings)
  (pair-fold
   (lambda (pair result)
     (if (null? (cdr pair))
         (cons (first pair) result)
         (if (string=? (first pair) (second pair))
             result
             (cons (first pair) result))))
   '()
   (sort strings string<?)))

(define (group-list-by-first-n-fields n lists)
  (fold (lambda (lst groups)
          (receive (key vals)
              (split-at lst n)
            (append
             (alist-delete key groups)
             `((,key . ,(append
                         (or (assoc-ref groups key)
                             '())
                         (list vals)))))))
        '()
        lists))
