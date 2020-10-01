;;; Guix Data Service -- Information about Guix over time
;;; Copyright © 2020 Christopher Baines <mail@cbaines.net>
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

(define-module (guix-data-service metrics)
  #:use-module (ice-9 match)
  #:use-module (squee)
  #:export (fetch-high-level-table-size-metrics
            fetch-pg-stat-user-tables-metrics))

(define (fetch-high-level-table-size-metrics conn)
  ;; Adapted from https://wiki.postgresql.org/wiki/Disk_Usage
  (define query
    "
WITH RECURSIVE pg_inherit(inhrelid, inhparent) AS (
  SELECT inhrelid, inhparent
  FROM pg_inherits
  UNION
  SELECT child.inhrelid, parent.inhparent
  FROM pg_inherit child, pg_inherits parent
  WHERE child.inhparent = parent.inhrelid
), pg_inherit_short AS (
  SELECT *
  FROM pg_inherit
  WHERE inhparent NOT IN (SELECT inhrelid FROM pg_inherit)
)
SELECT table_name,
       row_estimate,
       table_bytes,
       index_bytes,
       toast_bytes
FROM (
  SELECT *, total_bytes-index_bytes-COALESCE(toast_bytes,0) AS table_bytes
  FROM (
    SELECT c.oid,
           nspname AS table_schema,
           relname AS table_name,
           SUM(c.reltuples) OVER (partition BY parent) AS row_estimate,
           SUM(pg_total_relation_size(c.oid)) OVER (partition BY parent) AS total_bytes,
           SUM(pg_indexes_size(c.oid)) OVER (partition BY parent) AS index_bytes,
           SUM(pg_total_relation_size(reltoastrelid)) OVER (partition BY parent) AS toast_bytes,
           parent
    FROM (
      SELECT pg_class.oid,
             reltuples,
             relname,
             relnamespace,
             pg_class.reltoastrelid,
             COALESCE(inhparent, pg_class.oid) parent
      FROM pg_class
      LEFT JOIN pg_inherit_short ON inhrelid = oid
      WHERE relkind IN ('r', 'p')
    ) c
    LEFT JOIN pg_namespace n ON n.oid = c.relnamespace
  ) a
  WHERE oid = parent
    AND table_schema = 'guix_data_service'
) a;")

  (map (match-lambda
         ((name row-estimate table-bytes index-bytes toast-bytes)
          (list name
                (or (string->number row-estimate) 0)
                (or (string->number table-bytes) 0)
                (or (string->number index-bytes) 0)
                (or (string->number toast-bytes) 0))))
       (exec-query conn query)))

(define (fetch-pg-stat-user-tables-metrics conn)
  (define query
    "
SELECT relname, seq_scan, seq_tup_read,
       idx_scan, idx_tup_fetch,
       n_tup_ins, n_tup_upd, n_tup_del, n_tup_hot_upd,
       n_live_tup, n_dead_tup, n_mod_since_analyze,
       COALESCE(extract(epoch from last_vacuum), 0),
       COALESCE(extract(epoch from last_autovacuum), 0),
       COALESCE(extract(epoch from last_analyze), 0),
       COALESCE(extract(epoch from last_autoanalyze), 0),
       vacuum_count, autovacuum_count, analyze_count, autoanalyze_count
 FROM pg_stat_user_tables")

  (map (match-lambda
         ((relname seq-scan seq-tup-read
                   idx-scan idx-tup-fetch
                   n-tup-ins n-tup-upd n-tup-del n-tup-hot-upd
                   n-live-tup n-dead-tup n-mod-since-analyze
                   last-vacuum last-autovacuum last-analyze last-autoanalyze
                   vacuum-count autovacuum-count analyze-count autoanalyze-count)
          `((name                . ,relname)
            (seq-scan            . ,seq-scan)
            (seq-tup-read        . ,seq-tup-read)
            (idx-scan            . ,idx-scan)
            (idx-tup-fetch       . ,idx-tup-fetch)
            (n-tup-ins           . ,n-tup-ins)
            (n-tup-upd           . ,n-tup-upd)
            (n-tup-del           . ,n-tup-del)
            (n-tup-hot-upd       . ,n-tup-hot-upd)
            (n-live-tup          . ,n-live-tup)
            (n-dead-tup          . ,n-dead-tup)
            (n-mod-since-analyze . ,n-mod-since-analyze)
            (last-vacuum         . ,last-vacuum)
            (last-autovacuum     . ,last-autovacuum)
            (last-analyze        . ,last-analyze)
            (last-autoanalyze    . ,last-autoanalyze)
            (vacuum-count        . ,vacuum-count)
            (autovacuum-count    . ,autovacuum-count)
            (analyze-count       . ,analyze-count)
            (autoanalyze-count   . ,autoanalyze-count))))
       (exec-query conn query)))
