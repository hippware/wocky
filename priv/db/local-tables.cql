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
    user timeuuid,      // User ID
    server text,        // User Server
    handle text,        // User handle (as seen by other users)
    password text,      // Password hash
    phone_number text,  // User's phone number
    status bigint,
    user_status text,
    avatar timeuuid,    // UUID of file containing user's avatar
    PRIMARY KEY (user)
);

/* Table for storing details of users' last activty on the server. This is
 * updated only when a user logs out or disconnects
 */
CREATE TABLE last_activity (
    user timeuuid,          // User ID
    server text,            // User Server
    timestamp timestamp,    // Timestamp of last user logoff
    status text,            // Text set in last user presence
                            // with type of "unavailable"
    PRIMARY KEY (user)
);

/* Table for storing messages sent to a user while they're offline
 */
CREATE TABLE offline_msg (
    user timeuuid,          // User ID
    server text,            // User Server
    msg_id timeuuid,        // Unique message ID
    timestamp timestamp,    // Message timestamp
    expire timestamp,       // Message expiry (as timestamp)
    from_id text,           // Sending user JID
    to_id text,             // Receiving user JID
    packet text,            // Full XML of <message> element
    PRIMARY KEY (user, timestamp, msg_id)
) WITH CLUSTERING ORDER BY (timestamp ASC);

CREATE TABLE roster (
    user timeuuid,
    server text,
    contact text,
    nick text,
    groups set<text>,
    ask text,
    askmessage text,
    subscription text,
    version timeuuid,
    PRIMARY KEY (user, contact)
);

CREATE INDEX ON roster(version);

CREATE TABLE session (
    sid blob,
    node text,
    user timeuuid,
    server text,
    jid_user timeuuid,
    jid_server text,
    jid_resource blob,
    priority int,
    info blob,
    PRIMARY KEY (sid)
);

CREATE INDEX ON session(node);

CREATE TABLE user_to_sids (
    jid_user timeuuid,
    sids set<blob>,
    PRIMARY KEY (jid_user)
);
