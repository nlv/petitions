-- Verify petitions-schema:init_tables on pg


SELECT code FROM locales
WHERE code = 'ru';

SELECT code FROM locales
WHERE code = 'en';

SELECT code FROM locales
WHERE code = 'de';

SELECT code FROM locales
WHERE code = 'el';

SELECT code FROM locales
WHERE code = 'sr';

SELECT code FROM locales
WHERE code = 'uk';

SELECT id FROM petitions
WHERE FALSE;

SELECT id FROM petitions_locale
WHERE FALSE;

SELECT id FROM signers
WHERE FALSE;

SELECT id FROM signers_aggrs
WHERE FALSE;

