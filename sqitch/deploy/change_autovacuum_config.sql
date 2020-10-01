-- Deploy guix-data-service:change_autovacuum_config to pg

BEGIN;

ALTER TABLE derivations SET (autovacuum_vacuum_scale_factor = 0.01);
ALTER TABLE derivations SET (autovacuum_analyze_scale_factor = 0.01);

ALTER TABLE derivation_inputs SET (autovacuum_vacuum_scale_factor = 0.01);
ALTER TABLE derivation_inputs SET (autovacuum_analyze_scale_factor = 0.01);

ALTER TABLE guix_revision_package_derivations SET (autovacuum_vacuum_scale_factor = 0.01);
ALTER TABLE guix_revision_package_derivations SET (autovacuum_analyze_scale_factor = 0.01);

ALTER TABLE derivation_sources SET (autovacuum_vacuum_scale_factor = 0.01);
ALTER TABLE derivation_sources SET (autovacuum_analyze_scale_factor = 0.01);

ALTER TABLE derivation_outputs SET (autovacuum_vacuum_scale_factor = 0.01);
ALTER TABLE derivation_outputs SET (autovacuum_analyze_scale_factor = 0.01);

COMMIT;
