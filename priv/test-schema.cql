/* Copyright (c) 2015 Hippware */

CREATE KEYSPACE wocky_test_shared
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};

USE wocky_test_shared;

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

CREATE KEYSPACE wocky_test_localhost
    WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};

USE wocky_test_localhost;

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

CREATE TABLE last_activity (
    user text,
    timestamp timestamp,
    status text,
    PRIMARY KEY (user)
);
