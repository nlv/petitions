-- Revert petitions-schema:letters from pg

BEGIN;

DROP TABLE signers2letters;
DROP TABLE letters;
DROP TABLE orgs2petitions;
DROP TABLE organizations;

ALTER TABLE signers DROP CONSTRAINT signers_petition_id_uniq;

COMMIT;
