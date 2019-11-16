-- Revert guix-data-service:channel_news_tables from pg

BEGIN;

DROP TABLE guix_revision_channel_news_entries;
DROP TABLE channel_news_entry_titles;
DROP TABLE channel_news_entry_bodies;
DROP TABLE channel_news_entry_text;
DROP TABLE channel_news_entries;

COMMIT;
