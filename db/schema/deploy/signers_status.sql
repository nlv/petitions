-- Deploy petitions-schema:signers_status to pg
-- requires: letters

BEGIN;

CREATE TYPE signers_status AS ENUM ('new', 'duplicate', 'prepared', 'sent');

ALTER TABLE signers 
ADD COLUMN status signers_status NOT NULL DEFAULT 'new';

CREATE INDEX idx_signers_status ON signers (petition_id, status);

COMMIT;
