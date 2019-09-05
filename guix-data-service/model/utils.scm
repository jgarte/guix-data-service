(define-module (guix-data-service model utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 receive)
  #:use-module (squee)
  #:use-module (guix-data-service database)
  #:export (NULL
            quote-string
            value->quoted-string-or-null
            non-empty-string-or-false
            exec-query->vhash
            two-lists->vhash
            deduplicate-strings
            group-list-by-first-n-fields
            insert-missing-data-and-return-all-ids))

(define NULL '())

(define (quote-string s)
  (string-append "$STR$" s "$STR$"))

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
        (exec-query-with-null-handling conn query)))

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

(define* (insert-missing-data-and-return-all-ids
          conn
          table-name
          fields
          data
          #:key
          sets-of-data?
          delete-duplicates?)
  (define field-strings
    (map symbol->string fields))

  (define value->sql
    (match-lambda
      ((? string? s)
       (string-append "$STR$" s "$STR$"))
      ((? symbol? s)
       (string-append "$STR$"
                      (symbol->string s)
                      "$STR$"))
      ((? number? n)
       (number->string n))
      ((? boolean? b)
       (if b "TRUE" "FALSE"))
      ((? null?)
       "NULL")
      (v
       (error
        (simple-format #f "error: unknown type for value: ~A" v)))))

  (define select-query
    (string-append
     "SELECT id, "
     (string-join (map (lambda (field)
                         (string-append table-name "." field))
                       field-strings)
                  ", ")
     " FROM " table-name
     " JOIN (VALUES "
     (string-join
      (map
       (lambda (field-values)
         (string-append
          "("
          (string-join (map value->sql field-values) ",")
          ")"))
       (if sets-of-data?
           (delete-duplicates
            (concatenate data))
           data))
      ", ")
     ") AS vals (" (string-join field-strings ", ") ") "
     "ON "
     (string-join
      (map (lambda (field)
             (string-append
              "(" table-name "." field " = vals." field
              " OR (" table-name "." field " IS NULL AND"
              " vals." field " IS NULL))"))
           field-strings)
      " AND ")))

  (define (insert-sql missing-data)
    (string-append
     "INSERT INTO " table-name " ("
     (string-join field-strings ", ")
     ") VALUES "
     (string-join
      (map (lambda (field-values)
             (string-append
              "("
              (string-join
               (map (lambda (value)
                      (value->sql value))
                    field-values)
               ", ")
              ")"))
           missing-data)
      ", ")
     " RETURNING id"))

  (define (normalise-values data)
    (map (match-lambda
           ((? boolean? b)
            (if b "t" "f"))
           ((? number? n)
            (number->string n))
           ((? symbol? s)
            (symbol->string s))
           ((? string? s)
            s)
           ((? null? s)
            ;; exec-query-with-null-handling specifies NULL values as '()
            '()))
         data))

  (let* ((existing-entries
          (exec-query->vhash conn
                             select-query
                             cdr
                             (lambda (result)
                               (string->number (first result)))))
         (missing-entries
          (filter (lambda (field-values)
                    (not (vhash-assoc
                          ;; Normalise at this point, so that the proper value
                          ;; to insert is carried forward
                          (normalise-values field-values)
                          existing-entries)))
                  (if sets-of-data?
                      (delete-duplicates (concatenate data))
                      (if delete-duplicates?
                          (delete-duplicates data)
                          data))))
         (new-entries
          (if (null? missing-entries)
              '()
              (map (lambda (result)
                     (string->number (first result)))
                   (exec-query conn (insert-sql missing-entries)))))
         (new-entries-lookup-vhash
          (two-lists->vhash missing-entries
                            new-entries)))

    (if sets-of-data?
        (map (lambda (field-value-lists)
               ;; Normalise the result at this point, ensuring that the id's
               ;; in the set are sorted
               (sort
                (map (lambda (field-values)
                       (cdr
                        (or (vhash-assoc (normalise-values field-values)
                                         existing-entries)
                            (vhash-assoc field-values
                                         new-entries-lookup-vhash)
                            (error "missing entry" field-values))))
                     field-value-lists)
                <))
             data)
        (map (lambda (field-values)
               (cdr
                (or (vhash-assoc (normalise-values field-values)
                                 existing-entries)
                    (vhash-assoc field-values
                                 new-entries-lookup-vhash)
                    (error "missing entry" field-values))))
             data))))
