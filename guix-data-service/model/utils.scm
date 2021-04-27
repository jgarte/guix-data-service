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

(define-module (guix-data-service model utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 receive)
  #:use-module (squee)
  #:use-module (guix-data-service database)
  #:use-module (guix-data-service utils)
  #:export (NULL
            quote-string
            value->quoted-string-or-null
            non-empty-string-or-false
            exec-query->vhash
            two-lists->vhash
            parse-postgresql-array-string
            deduplicate-strings
            group-list-by-first-n-fields
            group-to-alist
            group-to-alist/vector
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

(define (parse-postgresql-array-string s)
  (if (string=? s "{}")
      '()
      (string-split
       (string-drop-right
        (string-drop s 1)
        1)
       #\,)))

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

(define (group-to-alist process lst)
  (fold (lambda (element result)
          (match (process element)
            ((key . value)
             (match (assoc key result)
               ((_ . existing-values)
                `((,key . ,(cons value existing-values))
                  ,@(alist-delete key result)))
               (#f
                `((,key . (,value))
                  ,@result))))))
        '()
        lst))

(define (group-to-alist/vector process lst)
  (map
   (match-lambda
     ((label . items)
      (cons label (list->vector items))))
   (group-to-alist process lst)))

(define (table-schema conn table-name)
  (let ((results
         (exec-query
          conn
          "
SELECT column_name, data_type, is_nullable
FROM information_schema.columns
WHERE table_name = $1"
          (list table-name))))
    (if (null? results)
        (error
         (simple-format #f "missing schema for ~A: ~A"
                        table-name
                        results))
        (map
         (match-lambda
           ((column_name data_type is_nullable)
            (list column_name
                  data_type
                  (string=? is_nullable "YES"))))
         results))))

(define* (insert-missing-data-and-return-all-ids
          conn
          table-name
          fields
          data
          #:key
          sets-of-data?
          delete-duplicates?
          use-temporary-table?)
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
      ((cast . value)
       (string-append
        (value->sql value) "::" cast))
      (v
       (error
        (simple-format #f "error: unknown type for value: ~A" v)))))

  (define schema-details
    (table-schema conn table-name))

  (define (field-can-be-null? field)
    (match (find (lambda (column-data)
                   (string=? field
                             (car column-data)))
                 schema-details)
      ((column-name data-type is-nullable?) is-nullable?)
      (#f
       (simple-format
        (current-error-port)
        "error: couldn't find data for ~A in ~A\n"
        field
        schema-details)
       (error "error: field-can-be-null?"))))

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
             (string-concatenate
              `("("
                ,table-name "." ,field " = vals." ,field
                ,@(if (field-can-be-null? field)
                      `(" OR (" ,table-name "." ,field " IS NULL AND"
                        " vals." ,field " IS NULL"
                        ")")
                      '())
                ")")))
           field-strings)
      " AND ")))

  (define (temp-table-select-query temp-table-name)
    (string-append
     "SELECT " table-name ".id, "
     (string-join (map (lambda (field)
                         (string-append table-name "." field))
                       field-strings)
                  ", ")
     " FROM " table-name
     " INNER JOIN " temp-table-name
     " ON "
     (string-join
      (map (lambda (field)
             (string-concatenate
              `("("
                ,table-name "." ,field " = " ,temp-table-name "." ,field
                ,@(if (field-can-be-null? field)
                      `(" OR ("
                        ,table-name "." ,field " IS NULL"
                        " AND "
                        ,temp-table-name "." ,field " IS NULL"
                        ")")
                      '())
                ")")))
           field-strings)
      " AND ")))

  (define* (insert-sql missing-data
                       #:key
                       (table-name table-name))
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

  (define (format-json json)
    ;; PostgreSQL formats JSON strings differently to guile-json, so use
    ;; PostgreSQL to do the formatting
    (caar
     (exec-query
      conn
      (string-append
       "SELECT $STR$" json "$STR$::jsonb"))))

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
           ((? null? n)
            ;; exec-query-with-null-handling specifies NULL values as '()
            n)
           ((cast . value)
            (if (string=? cast "jsonb")
                (format-json value)
                value))
           (unknown
            (error (simple-format #f "normalise-values: error: ~A\n" unknown))))
         data))

  (let* ((existing-entries
          (if use-temporary-table?
              (let ((temp-table-name
                     (string-append "temp_" table-name))
                    (data
                     (if sets-of-data?
                         (delete-duplicates (concatenate data))
                         (if delete-duplicates?
                             (delete-duplicates data)
                             data))))
                ;; Create a temporary table to store the data
                (exec-query
                 conn
                 (string-append "CREATE TEMPORARY TABLE "
                                temp-table-name
                                " (LIKE "
                                table-name
                                " INCLUDING ALL)"))
                (exec-query
                 conn
                 (string-append
                  "ANALYZE " temp-table-name))

                ;; Populate the temporary table
                (if (null? data)
                    '()
                    (with-time-logging (string-append "populating " temp-table-name)
                      (exec-query conn
                                  (insert-sql data
                                              #:table-name temp-table-name))))
                ;; Use the temporary table to find the existing values
                (let ((result
                       (with-time-logging
                           (string-append "querying the " temp-table-name)
                         (exec-query->vhash
                          conn
                          (temp-table-select-query temp-table-name)
                          cdr
                          (lambda (result)
                            (string->number (first result)))))))

                  (exec-query conn (string-append "DROP TABLE " temp-table-name))
                  result))

              ;; If not using a temporary table, just do a single SELECT query
              (if (null? data)
                  '()
                  (exec-query->vhash conn
                                     select-query
                                     cdr
                                     (lambda (result)
                                       (string->number (first result)))))))
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
                            new-entries))
         (all-ids
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
    (values all-ids
            new-entries)))
