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

-- A dummy table for generating UUIDs ie. "SELECT NOW() from uuidgen"
CREATE TABLE uuidgen (
    id int,
    PRIMARY KEY (id)
);
-- Needs at least one row
INSERT INTO uuidgen (id) VALUES (0);

CREATE TABLE username_to_user (
    domain text,
    username text,
    id timeuuid,
    PRIMARY KEY (domain, username)
);

CREATE TABLE phone_number_to_user (
    phone_number text,
    id timeuuid,
    domain text,
    PRIMARY KEY (phone_number)
);
