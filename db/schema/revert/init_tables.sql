-- Revert petitions-schema:init_tables from pg

BEGIN;

DROP TABLE signers_aggrs;
DROP TABLE signers;
DROP TABLE petitions_locale;
DROP TABLE petitions;
DROP TABLE locales;

COMMIT;
