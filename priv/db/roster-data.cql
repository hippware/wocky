/* Copyright 2016 Hippware */

TRUNCATE TABLE wocky_shared.handle_to_user;
TRUNCATE TABLE wocky_localhost.user;
TRUNCATE TABLE wocky_localhost.roster;

BEGIN BATCH
INSERT INTO wocky_shared.handle_to_user (user, server, handle)
 VALUES (699bf62c-b897-11e5-9912-ba0be0483c18, 'localhost', 'alice');
INSERT INTO wocky_localhost.user (user, server, handle, password)
 VALUES (699bf62c-b897-11e5-9912-ba0be0483c18, 'localhost', 'alice', 'password');
APPLY BATCH;

BEGIN BATCH
INSERT INTO wocky_shared.handle_to_user (user, server, handle)
 VALUES (699bfa64-b897-11e5-9912-ba0be0483c18, 'localhost', 'madhatter');
INSERT INTO wocky_localhost.user (user, server, handle, password)
 VALUES (699bfa64-b897-11e5-9912-ba0be0483c18, 'localhost', 'madhatter', 'password');
APPLY BATCH;

BEGIN BATCH
INSERT INTO wocky_shared.handle_to_user (user, server, handle)
 VALUES (699bff32-b897-11e5-9912-ba0be0483c18, 'localhost', 'chesirecat');
INSERT INTO wocky_localhost.user (user, server, handle, password)
 VALUES (699bff32-b897-11e5-9912-ba0be0483c18, 'localhost', 'chesirecat', 'password');
APPLY BATCH;

INSERT INTO wocky_localhost.roster (
    user,
    server,
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
    server,
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
