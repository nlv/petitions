INSERT INTO locales
VALUES
('ru'),
('en'),
('de')
;

INSERT INTO petitions
(code, name, description, locale)
VALUES 
('for-all-goods', 'За все хорошее!', 'Надо добиться улучшение жизни НАРОДА!!!!!!', 'ru'),
('stop-cars', 'STOP CARS!', 'STOP USING CARS!!!!!!!', 'en')
;

INSERT INTO petitions_locale
(petition_id, locale, name, description)
VALUES
((SELECT id FROM petitions WHERE name = 'За все хорошее!'), 'en', 'For all goods', 'We have done it'),
((SELECT id FROM petitions WHERE name = 'STOP CARS!'), 'ru', 'Стоп машинам!', 'Ходить пешком!'),
((SELECT id FROM petitions WHERE name = 'STOP CARS!'), 'de', 'Ahtung! STOP AVTO!', 'Shprihen ze dojch!')
;

INSERT INTO signers
(
    petition_id, 
    first_name, 
    last_name, 
    country, 
    organization, 
    email, 
    phone,
    birth_year, 
    gender, 
    notifies_enabled,
    ip,
    inet_domain
)
VALUES
(
    (SELECT id FROM petitions WHERE name = 'За все хорошее!'), 
    'Петр', 
    'Петров', 
    'Россия',
    'ООО ППП',
    'nlv@lab.ru',
    '9139133322',
    '1972',
    'M',
    TRUE,
    '193.168.6.6',
    'rbc.ru'
);
