/* Copyright (c) 2015 Hippware */

/*
-- Keyspace for single server development
CREATE KEYSPACE <name>
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};

-- Sample keyspace for multi server
CREATE KEYSPACE <name>
    WITH REPLICATION = {'class' : 'NetworkTopologyStrategy', 'datacenter1': 3};

-- Use the keyspace just created
USE <name>;
*/

-- ToDo: firstname, lastname
CREATE TABLE user (
    user timeuuid,
    server text,
    handle text,
    password text,
    phone_number text,
    status bigint,
    user_status text,
    avatar timeuuid,
    PRIMARY KEY (user)
);

CREATE TABLE last_activity (
    user timeuuid,
    server text,
    timestamp timestamp,
    status text,
    PRIMARY KEY (user)
);

CREATE TABLE roster (
    user timeuuid,
    server text,
    contact text,
    active boolean,
    nick text,
    groups set<text>,
    ask text,
    askmessage text,
    subscription text,
    version timeuuid,
    PRIMARY KEY (user, contact)
);

CREATE INDEX ON roster(active);
CREATE INDEX ON roster(version);
