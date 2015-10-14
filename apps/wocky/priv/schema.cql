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

-- A dummy table for generating UUIDs ie. "SELECT NOW() from uuidgen"
CREATE TABLE uuidgen (
    id int,
    PRIMARY KEY (id)    
);
-- Needs at least one row
INSERT INTO uuidgen (id) VALUES (0);

-- ToDo: firstname, lastname
CREATE TABLE user (
    id timeuuid,
    domain text,
    username text,
    stored_key text,
    server_key text,
    salt text,
    iteration_count int,
    phone_number text,
    status bigint,
    user_status text,
    avatar timeuuid,
    PRIMARY KEY (id)
);
