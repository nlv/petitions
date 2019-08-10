-- Verify petitions-schema:signers_status on pg

BEGIN;

SELECT status FROM signers WHERE FALSE;

-- TODO
-- Проверка наличия индекса idx_signers_status

ROLLBACK;
