-- Verify petitions-schema:letters on pg

BEGIN;

SELECT id FROM signers_aggrs WHERE FALSE;
SELECT id FROM signers2letters WHERE FALSE;
SELECT id FROM letters WHERE FALSE;
SELECT id FROM orgs2petitions WHERE FALSE;
SELECT id FROM organizations WHERE FALSE;

-- TODO
-- ALTER TABLE signers DROP CONSTRAINT signers_petition_id_uniq;

ROLLBACK;
