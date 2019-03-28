-- Deploy petitions-schema:letters to pg
-- requires: init_tables

BEGIN;

ALTER TABLE signers ADD CONSTRAINT signers_petition_id_uniq UNIQUE (id, petition_id);

CREATE TABLE organizations (
     id           SERIAL               PRIMARY KEY
    ,name         TEXT        NOT NULL UNIQUE
    ,post_address TEXT    
    ,email        VARCHAR(60)
);

CREATE TABLE orgs2petitions (
     id          SERIAL                PRIMARY KEY
    ,petition_id INTEGER      NOT NULL REFERENCES petitions (id)
    ,org_id      INTEGER      NOT NULL REFERENCES organizations (id)
    ,UNIQUE (petition_id, org_id)
);

CREATE TABLE letters (
     id          SERIAL                PRIMARY KEY
    ,petition_id INTEGER      NOT NULL 
    ,org_id      INTEGER      NOT NULL
    ,description TEXT
    ,date_sent   TIMESTAMP
    -- ,status      
    ,UNIQUE(id, petition_id)
    ,FOREIGN KEY (petition_id, org_id) REFERENCES orgs2petitions (petition_id, org_id)
);

CREATE TABLE signers2letters (
     id          SERIAL                PRIMARY KEY
    ,petition_id INTEGER      NOT NULL
    ,letter_id   INTEGER      NOT NULL
    ,signer_id   INTEGER      NOT NULL
    ,FOREIGN KEY (letter_id, petition_id) REFERENCES letters (id, petition_id)
    ,FOREIGN KEY (signer_id, petition_id) REFERENCES signers (id, petition_id)
);

COMMIT;
