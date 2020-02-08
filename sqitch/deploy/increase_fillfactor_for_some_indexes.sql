-- Deploy guix-data-service:increase_fillfactor_for_some_indexes to pg

BEGIN;

ALTER INDEX derivation_inputs_pkey SET (fillfactor = 100);
ALTER INDEX derivation_inputs_derivation_output_id_idx SET (fillfactor = 100);
ALTER INDEX guix_revision_package_derivations_pkey SET (fillfactor = 100);
ALTER INDEX derivations_pkey SET (fillfactor = 100);
ALTER INDEX file_name_unique SET (fillfactor = 100);
ALTER INDEX id_unique SET (fillfactor = 100);
ALTER INDEX derivations_hash SET (fillfactor = 100);
ALTER INDEX package_derivations_pkey SET (fillfactor = 100);
ALTER INDEX package_derivations_id_key SET (fillfactor = 100);
ALTER INDEX package_derivations_derivation_id SET (fillfactor = 100);
ALTER INDEX package_derivations_package_id_idx SET (fillfactor = 100);

REINDEX INDEX derivation_inputs_pkey;
REINDEX INDEX derivation_inputs_derivation_output_id_idx;
REINDEX INDEX guix_revision_package_derivations_pkey;
REINDEX INDEX derivations_pkey;
REINDEX INDEX file_name_unique;
REINDEX INDEX id_unique;
REINDEX INDEX derivations_hash;
REINDEX INDEX package_derivations_pkey;
REINDEX INDEX package_derivations_id_key;
REINDEX INDEX package_derivations_derivation_id;
REINDEX INDEX package_derivations_package_id_idx;

COMMIT;
