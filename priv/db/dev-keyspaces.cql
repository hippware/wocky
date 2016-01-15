/* Copyright (c) 2016 Hippware */

/*
-- Keyspace for single server development
CREATE KEYSPACE <name>
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};

-- Sample keyspace for multi server
CREATE KEYSPACE <name>
    WITH REPLICATION = {'class' : 'NetworkTopologyStrategy', 'datacenter1': 3};
*/

-- Keyspaces for single server development
CREATE KEYSPACE wocky_shared
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};

CREATE KEYSPACE wocky_localhost
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};


-- Keyspaces for single server testing
CREATE KEYSPACE wocky_test_shared
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};

CREATE KEYSPACE wocky_test_localhost
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};
