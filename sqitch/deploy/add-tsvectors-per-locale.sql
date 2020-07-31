-- Deploy guix-data-service:add-tsvectors-per-locale to pg

BEGIN;

CREATE TABLE package_metadata_tsvectors(
  package_metadata_id integer NOT NULL REFERENCES package_metadata(id),
  locale varchar NOT NULL,
  synopsis_and_description tsvector NOT NULL,
  package_synopsis_id integer NOT NULL,
  package_description_id integer NOT NULL,
  PRIMARY KEY(locale, package_metadata_id)
);


INSERT INTO package_metadata_tsvectors (package_metadata_id, locale, synopsis_and_description,
                                        package_synopsis_id, package_description_id)
SELECT DISTINCT ON (package_metadata.id, locale)
         package_metadata.id,
         CASE WHEN translated_package_synopsis.locale != 'en_US.utf8'
              THEN translated_package_synopsis.locale
              ELSE translated_package_descriptions.locale
         END AS locale,
         (  setweight(to_tsvector(translated_package_synopsis.synopsis), 'B') ||
            setweight(to_tsvector(translated_package_descriptions.description), 'C')
         ),
         translated_package_synopsis.id,
         translated_package_descriptions.id
FROM package_metadata
INNER JOIN (
  SELECT package_description_sets.id AS package_description_set_id,
         package_descriptions.id, package_descriptions.description,
         package_descriptions.locale
  FROM package_description_sets
  INNER JOIN package_descriptions
    ON package_descriptions.id = ANY (package_description_sets.description_ids)
  ORDER BY package_description_sets.id,
           CASE WHEN package_descriptions.locale = 'en_US.utf8' THEN 1
                ELSE 2
           END DESC
) AS translated_package_descriptions
  ON package_metadata.package_description_set_id =
     translated_package_descriptions.package_description_set_id
INNER JOIN (
  SELECT package_synopsis_sets.id AS package_synopsis_set_id,
         package_synopsis.id, package_synopsis.synopsis,
         package_synopsis.locale
  FROM package_synopsis_sets
  INNER JOIN package_synopsis
    ON package_synopsis.id = ANY (package_synopsis_sets.synopsis_ids)
  ORDER BY package_synopsis_sets.id,
           CASE WHEN package_synopsis.locale = 'en_US.utf8' THEN 1
                ELSE 2
           END DESC
) AS translated_package_synopsis
  ON package_metadata.package_synopsis_set_id =
     translated_package_synopsis.package_synopsis_set_id
  AND (translated_package_descriptions.locale = translated_package_synopsis.locale
       OR translated_package_descriptions.locale = 'en_US.utf8')
ORDER BY package_metadata.id, locale,
         CASE WHEN translated_package_synopsis.locale =
                  translated_package_descriptions.locale THEN 1
              ELSE 0
         END DESC;

CREATE INDEX tsv_idx ON package_metadata_tsvectors USING gin(synopsis_and_description);

CREATE INDEX package_metadata_id_packages_idx ON packages USING btree(package_metadata_id);

CREATE INDEX package_metadata_id_package_metadata_tsvectors_idx ON package_metadata_tsvectors USING btree(package_metadata_id);

COMMIT;
