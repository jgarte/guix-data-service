-- Deploy guix-data-service:channel_news_tables to pg

BEGIN;

CREATE TABLE channel_news_entries (
  id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  commit varchar,
  tag varchar
);

CREATE TABLE guix_revision_channel_news_entries (
  guix_revision_id integer NOT NULL REFERENCES guix_revisions (id),
  channel_news_entry_id integer NOT NULL REFERENCES channel_news_entries (id),
  index integer NOT NULL,
  PRIMARY KEY (guix_revision_id, channel_news_entry_id)
);

CREATE TABLE channel_news_entry_text (
  id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  lang varchar NOT NULL,
  text varchar NOT NULL,
  UNIQUE (lang, text)
);

CREATE TABLE channel_news_entry_titles (
  channel_news_entry_id
    integer NOT NULL REFERENCES channel_news_entries (id),
  channel_news_entry_text_id
    integer NOT NULL REFERENCES channel_news_entry_text (id)
);

CREATE TABLE channel_news_entry_bodies (
  channel_news_entry_id
    integer NOT NULL REFERENCES channel_news_entries (id),
  channel_news_entry_text_id
    integer NOT NULL REFERENCES channel_news_entry_text (id)
);

COMMIT;
