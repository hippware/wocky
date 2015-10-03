/* Copyright (c) 2015 Hippware */

/*
Keyspace for single server development
CREATE KEYSPACE wocky
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};

Sample keyspace for multi server
CREATE KEYSPACE wocky
    WITH REPLICATION = {'class' : 'NetworkTopologyStrategy', 'datacenter1': 3};
*/

USE wocky;

-- A dummy table for generating UUIDs ie. "SELECT NOW() from uuidgen"
CREATE TABLE uuidgen (
    id int,
    PRIMARY KEY (id)    
);
-- Needs at least one row
INSERT INTO uuidgen (id) VALUES (0);
