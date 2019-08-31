(define-module (guix-data-service model utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 receive)
  #:use-module (squee)
  #:export (quote-string
            value->quoted-string-or-null
            value->sql-boolean
            non-empty-string-or-false
            exec-query->vhash
            two-lists->vhash
            deduplicate-strings
            group-list-by-first-n-fields
            insert-missing-data-and-return-all-ids))

(define (quote-string s)
  (string-append "$STR$" s "$STR$"))

(define (value->quoted-string-or-null value)
  (if (string? value)
      (string-append "$STR$" value "$STR$")
      "NULL"))

(define (value->sql-boolean v)
  (match v
    ((? boolean? x)
     (if x "TRUE" "FALSE"))
    ((? string? x)
     (if (or (string=? x "t")
             (string=? x "TRUE"))
         "TRUE"
         "FALSE"))))

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

(define (insert-missing-data-and-return-all-ids
         conn
         table-name
         fields-and-handlers
         data)
  (define fields (map symbol->string
                      (map first fields-and-handlers)))

  (define handlers (map cdr fields-and-handlers))

  (define select-query
    (string-append
     "SELECT id, "
     (string-join (map (lambda (field)
                         (string-append table-name "." field))
                       fields)
                  ", ")
     " FROM " table-name
     " JOIN (VALUES "
     ;; TODO This doesn't handle NULL values
     (string-join
      (map
       (lambda (field-values)
         (string-append
          "("
          (string-join
           (map (lambda (value handler)
                  (handler value))
                field-values
                handlers)
           ",")
          ")"))
       data)
      ", ")
     ") AS vals (" (string-join fields ", ") ") "
     "ON "
     (string-join
      (map (lambda (field)
             (string-append
              table-name "." field " = vals." field))
           fields)
      " AND ")))

  (define (insert-sql missing-data)
    (string-append
     "INSERT INTO " table-name " ("
     (string-join fields ", ")
     ") VALUES "
     (string-join
      (map (lambda (field-values)
             (string-append
              "("
              (string-join
               (map (lambda (value handler)
                     (handler value))
                    field-values
                    handlers)
               ", ")
              ")"))
           missing-data)
      ", ")
     " RETURNING id"))

  (define (normalise-database-values data)
    (map (match-lambda
           ((? boolean? b)
            (if b "t" "f"))
           ((? number? n)
            (number->string n))
           ((? symbol? s)
            (symbol->string s))
           ((? string? s)
            s))
         data))

  (let* ((existing-entries
          (exec-query->vhash conn
                             select-query
                             cdr
                             first))
         (missing-entries
          (filter (lambda (field-values)
                    (not (vhash-assoc (normalise-database-values field-values)
                                      existing-entries)))
                  data))
         (new-entries
          (if (null? missing-entries)
              '()
              (map first
                   (exec-query conn (insert-sql missing-entries)))))
         (new-entries-lookup-vhash
          (two-lists->vhash missing-entries
                            new-entries)))

    (map (lambda (field-values)
           (cdr
            (or (vhash-assoc (normalise-database-values field-values)
                             existing-entries)
                (vhash-assoc field-values
                             new-entries-lookup-vhash)
                (error "missing entry" field-values))))
         data)))
