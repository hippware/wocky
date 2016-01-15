/* Copyright 2016 Hippware */

TRUNCATE TABLE wocky_shared.username_to_user;
TRUNCATE TABLE wocky_localhost.user;
TRUNCATE TABLE wocky_localhost.contacts;

BEGIN BATCH
INSERT INTO wocky_shared.username_to_user (domain, username, id)
 VALUES ('localhost', 'alice', 699bf62c-b897-11e5-9912-ba0be0483c18);
INSERT INTO wocky_localhost.user (id, domain, username, password)
 VALUES (699bf62c-b897-11e5-9912-ba0be0483c18, 'localhost', 'alice', 'password');
APPLY BATCH;

BEGIN BATCH
INSERT INTO wocky_shared.username_to_user (domain, username, id)
 VALUES ('localhost', 'madhatter', 699bfa64-b897-11e5-9912-ba0be0483c18);
INSERT INTO wocky_localhost.user (id, domain, username, password)
 VALUES (699bfa64-b897-11e5-9912-ba0be0483c18, 'localhost', 'madhatter', 'password');
APPLY BATCH;

BEGIN BATCH
INSERT INTO wocky_shared.username_to_user (domain, username, id)
 VALUES ('localhost', 'chesirecat', 699bff32-b897-11e5-9912-ba0be0483c18);
INSERT INTO wocky_localhost.user (id, domain, username, password)
 VALUES (699bff32-b897-11e5-9912-ba0be0483c18, 'localhost', 'chesirecat', 'password');
APPLY BATCH;

INSERT INTO wocky_localhost.roster (
    user,
    domain,
    contact,
    active,
    nick,
    groups,
    version
) VALUES (
    699bf62c-b897-11e5-9912-ba0be0483c18,
    'localhost',
    '699bfa64-b897-11e5-9912-ba0be0483c18@localhost',
    true,
    'madhatter',
    {'friends'},
    now()
);

INSERT INTO wocky_localhost.roster (
    user,
    domain,
    contact,
    active,
    nick,
    groups,
    version
) VALUES (
    699bf62c-b897-11e5-9912-ba0be0483c18,
    'localhost',
    '699bff32-b897-11e5-9912-ba0be0483c18@localhost',
    true,
    'chesirecat',
    {'friends', 'starred'},
    now()
);
