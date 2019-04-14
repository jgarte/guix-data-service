-- Verify guix-data-service:initial_import on pg

BEGIN;

SELECT id, url, lookup_all_derivations
  FROM guix_data_service.build_servers WHERE FALSE;

SELECT id, status_fetched_at, internal_build_id, starttime, stoptime status
  FROM guix_data_service.build_status WHERE FALSE;

SELECT internal_id, id, build_server_id, derivation_id
  FROM guix_data_service.builds WHERE FALSE;

SELECT derivation_id, derivation_output_id
  FROM guix_data_service.derivation_inputs WHERE FALSE;

SELECT id, path, hash_algorithm, hash, recursive
  FROM guix_data_service.derivation_output_details WHERE FALSE;

SELECT derivation_id, name, derivation_output_details_id, id
  FROM guix_data_service.derivation_outputs WHERE FALSE;

SELECT id, store_path
  FROM guix_data_service.derivation_source_files WHERE FALSE;

SELECT derivation_id, derivation_source_file_id
  FROM guix_data_service.derivation_sources WHERE FALSE;

SELECT id, file_name, builder, args, env_vars, system
  FROM guix_data_service.derivations WHERE FALSE;

SELECT revision_id, package_derivation_id
  FROM guix_data_service.guix_revision_package_derivations WHERE FALSE;

SELECT id, url, commit, store_path
  FROM guix_data_service.guix_revisions WHERE FALSE;

SELECT id, url, commit, source
  FROM guix_data_service.load_new_guix_revision_jobs WHERE FALSE;

SELECT id, package_id, derivation_id, system, target
  FROM guix_data_service.package_derivations WHERE FALSE;

SELECT id, sha1_hash, synopsis, description, home_page
  FROM guix_data_service.package_metadata WHERE FALSE;

SELECT id, name, version, package_metadata_id
  FROM guix_data_service.packages WHERE FALSE;

ROLLBACK;
