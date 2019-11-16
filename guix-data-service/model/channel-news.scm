(define-module (guix-data-service model channel-news)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:use-module (guix channels)
  #:use-module (guix-data-service model utils)
  #:export (insert-channel-news-entries-for-guix-revision))

(define (insert-channel-news-entry-text conn text)
  (insert-missing-data-and-return-all-ids
   conn
   "channel_news_entry_text"
   '(lang text)
   (map (match-lambda
          ((lang . text)
           (list lang text)))
        text)))

(define (insert-channel-news-entry conn commit tag)
  (match (exec-query
          conn
          (string-append
           "INSERT INTO channel_news_entries (commit, tag) VALUES ("
           (value->quoted-string-or-null commit)
           ","
           (value->quoted-string-or-null tag)
           ") RETURNING id"))
    (((id))
     (string->number id))))

(define (insert-channel-news-entries conn channel-news-entries)
  (define select-channel-news-entries
    "
SELECT channel_news_entries.id,
       channel_news_entries.commit,
       channel_news_entries.tag,
       (
         SELECT ARRAY_AGG(
           channel_news_entry_titles.channel_news_entry_text_id
           ORDER BY channel_news_entry_titles.channel_news_entry_text_id
         )
         FROM channel_news_entry_titles
         WHERE channel_news_entry_id = channel_news_entries.id
       ) AS title_text,
       (
         SELECT ARRAY_AGG(
           channel_news_entry_bodies.channel_news_entry_text_id
           ORDER BY channel_news_entry_bodies.channel_news_entry_text_id
         )
         FROM channel_news_entry_bodies
         WHERE channel_news_entry_id = channel_news_entries.id
       ) AS body_text
FROM channel_news_entries
ORDER BY id")

  (define existing
    (exec-query->vhash conn
                       select-channel-news-entries
                       (match-lambda
                         ((_ commit tag title-ids body-ids)
                          (list commit
                                tag
                                (map string->number
                                     (parse-postgresql-array-string title-ids))
                                (map string->number
                                     (parse-postgresql-array-string body-ids)))))
                       (lambda (result)
                         (string->number (first result)))))

  (map
   (lambda (entry)
     (let ((commit (channel-news-entry-commit entry))
           (tag (channel-news-entry-tag entry))
           (title-ids
            (sort (insert-channel-news-entry-text
                   conn (channel-news-entry-title entry))
                  <))
           (body-ids
            (sort (insert-channel-news-entry-text
                   conn
                   (channel-news-entry-body entry))
                  <)))
       (or (and=> (vhash-assoc (list (or commit '())
                                     (or tag '())
                                     title-ids
                                     body-ids)
                               existing)
                  (match-lambda
                    ((value . key)
                     key)))
           (let ((channel-news-entry-id
                  (insert-channel-news-entry conn commit tag)))
             (for-each
              (lambda (table ids)
                (exec-query
                 conn
                 (string-append
                  "INSERT INTO " table
                  " VALUES "
                  (string-join
                   (map (lambda (id)
                          (simple-format #f "(~A, ~A)"
                                         channel-news-entry-id
                                         id))
                        ids)
                   ", "))))
              '("channel_news_entry_titles"
                "channel_news_entry_bodies")
              (list title-ids
                    body-ids))

             channel-news-entry-id))))
   channel-news-entries))

(define (insert-channel-news-entries-for-guix-revision
         conn
         guix-revision-id
         channel-news-entries)
  (unless (null? channel-news-entries)
    (let ((channel-news-entry-ids
           (insert-channel-news-entries conn channel-news-entries)))
      (exec-query
       conn
       (string-append
        "INSERT INTO guix_revision_channel_news_entries "
        "(guix_revision_id, channel_news_entry_id, index) VALUES "
        (string-join
         (map (lambda (id index)
                (simple-format #f "(~A,~A,~A)" guix-revision-id id index))
              channel-news-entry-ids
              (iota (length channel-news-entries)))
         ", ")))))
  #t)
