-- Revert guix-data-service:initial_import from pg

BEGIN;

DROP TABLE guix_data_service.build_servers;
DROP TABLE guix_data_service.build_status;
DROP TABLE guix_data_service.builds;
DROP TABLE guix_data_service.derivation_inputs;
DROP TABLE guix_data_service.derivation_output_details;
DROP TABLE guix_data_service.derivation_outputs;
DROP TABLE guix_data_service.derivation_source_files;
DROP TABLE guix_data_service.derivation_sources;
DROP TABLE guix_data_service.derivations;
DROP TABLE guix_data_service.guix_revision_package_derivations;
DROP TABLE guix_data_service.guix_revisions;
DROP TABLE guix_data_service.load_new_guix_revision_jobs;
DROP TABLE guix_data_service.package_derivations;
DROP TABLE guix_data_service.package_metadata;
DROP TABLE guix_data_service.packages;

COMMIT;
