CREATE TABLE locales (
     code        VARCHAR(10)  NOT NULL UNIQUE CHECK (LOWER(code) = code)
);

CREATE TABLE petitions (
     id          SERIAL       PRIMARY KEY
    ,code        VARCHAR(40)  NOT NULL UNIQUE
    ,name        VARCHAR(255) NOT NULL UNIQUE
    ,description TEXT         NOT NULL
    ,content     TEXT         NOT NULL
    ,locale      VARCHAR(10)  NOT NULL REFERENCES locales (code)
    ,insdate     TIMESTAMP    NOT NULL DEFAULT NOW()
);

CREATE TABLE petitions_locale (
     id          SERIAL                PRIMARY KEY
    ,petition_id INTEGER      NOT NULL REFERENCES petitions (id)
    ,locale      VARCHAR(10)  NOT NULL REFERENCES locales (code)
    ,name        VARCHAR(255) NOT NULL 
    ,description TEXT         NOT NULL
    ,content     TEXT         NOT NULL
    ,insdate     TIMESTAMP    NOT NULL DEFAULT NOW()
    ,UNIQUE (petition_id, locale)
);

CREATE TABLE signers (
     id               SERIAL                PRIMARY KEY
    ,petition_id      INTEGER      NOT NULL REFERENCES petitions (id)
    ,first_name       VARCHAR(255) NOT NULL 
    ,last_name        VARCHAR(255) NOT NULL 
    ,country          VARCHAR(255) NOT NULL
    ,city             VARCHAR(255) NOT NULL
    ,organization     VARCHAR(255) 
    ,email            VARCHAR(255) 
    ,phone            VARCHAR(255)    
    ,birth_year       INTEGER      CHECK (birth_year >= 1900 AND birth_year <= extract (YEAR FROM NOW()) - 16)
    ,gender           VARCHAR(1)   CHECK (gender IS NULL OR gender IN ('M', 'F'))
    ,notifies_enabled BOOLEAN      NOT NULL DEFAULT FALSE
    ,ip               INET         
    ,inet_domain      VARCHAR(255)
    ,insdate     TIMESTAMP    NOT NULL DEFAULT NOW()
    ,CHECK (email IS NOT NULL OR phone IS NOT NULL)
);
