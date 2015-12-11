/* Copyright (c) 2015 Hippware */

-- Keyspace for single server development
CREATE KEYSPACE wocky_shared
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};

-- Use the keyspace just created
USE wocky_shared;

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

-- Keyspace for single server development
CREATE KEYSPACE wocky_localhost
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};

-- Use the keyspace just created
USE wocky_localhost;

-- ToDo: firstname, lastname
CREATE TABLE user (
    id timeuuid,
    domain text,
    username text,
    password text,
    phone_number text,
    status bigint,
    user_status text,
    avatar timeuuid,
    PRIMARY KEY (id)
);
