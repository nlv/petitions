-- Revert petitions-schema:signers_status from pg

BEGIN;

DROP INDEX idx_signers_status;

ALTER TABLE signers DROP COLUMN status;

DROP TYPE signers_status;

COMMIT;
