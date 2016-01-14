/* Copyright (c) 2015 Hippware */

/*
-- Keyspace for single server development
CREATE KEYSPACE wocky_shared
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};

-- Sample keyspace for multi server
CREATE KEYSPACE wocky_shared
    WITH REPLICATION = {'class' : 'NetworkTopologyStrategy', 'datacenter1': 3};

-- Use the keyspace just created
USE wocky_shared;
*/

CREATE TABLE handle_to_user (
    user timeuuid,
    server text,
    handle text,
    PRIMARY KEY (server, handle)
);

CREATE TABLE phone_number_to_user (
    user timeuuid,
    server text,
    phone_number text,
    PRIMARY KEY (phone_number)
);
