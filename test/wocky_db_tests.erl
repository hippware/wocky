%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for ejabberd_auth_cassandra.erl
-module(wocky_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky.hrl").
-include("wocky_db_seed.hrl").


-define(LONG_STRING, <<"Lorem ipsum dolor sit amet, consectetur cras amet.">>).

to_keyspace_test_() -> {
  "to_keyspace/1", [
    { "should replace non-alphanumeric characters with underscores", [
      ?_assertEqual(<<"abc123___">>, wocky_db:to_keyspace("abc123$!@"))
    ]},
    { "should truncate strings at 48 characters", [
      ?_assert(byte_size(?LONG_STRING) > 48),
      ?_assertEqual(48, byte_size(wocky_db:to_keyspace(?LONG_STRING)))
    ]}
]}.

invalid_keyspace_test_() -> {
  "query", [
    { "should return an error when given an invalid keyspace", [
      ?_assertMatch({error, _},
                    wocky_db:query(<<"dud">>, "SELECT * FROM user", [], quorum))
    ]}
]}.

empty_batch_test_() -> {
  "batch_query", [
    { "should return {ok, void} when given an empty query list", [
      ?_assertEqual({ok, void},
                    wocky_db:batch_query(shared, [], quorum))
    ]}
]}.

is_valid_id_test_() ->
  { "is_valid_id", [
    { "returns true if the user ID is a valid UUID", [
      ?_assert(wocky_db:is_valid_id(?USER)),
      ?_assert(wocky_db:is_valid_id(ossp_uuid:make(v1, text))),
      ?_assert(wocky_db:is_valid_id(ossp_uuid:make(v1, binary))),
      ?_assert(wocky_db:is_valid_id(ossp_uuid:make(v4, text))),
      ?_assert(wocky_db:is_valid_id(ossp_uuid:make(v4, binary)))
    ]},
    { "returns false if the user ID is not a valid UUID", [
      ?_assertNot(wocky_db:is_valid_id(<<"alice">>))
    ]}
  ]}.


test_build_query_cases(Fun, Cases) ->
    lists:foreach(
      fun ({Query, Args}) ->
          ?assertEqual(Query, lists:flatten(apply(Fun, Args)))
      end,
      Cases).

build_select_query_test() ->
    test_build_query_cases(fun wocky_db:build_select_query/4, [
        {"SELECT * FROM users",
         [users, all, [], 0]},
        {"SELECT * FROM users WHERE user = ?",
         [users, all, [user], 0]},
        {"SELECT * FROM users WHERE user = ? AND server = ?",
         [users, all, [user, server], 0]},
        {"SELECT * FROM users WHERE user = ? AND server = ? LIMIT 1",
         [users, all, [user, server], 1]},
        {"SELECT user FROM users",
         [users, [user], [], 0]},
        {"SELECT user FROM users WHERE server = ?",
         [users, [user], [server], 0]},
        {"SELECT user, server FROM users",
         [users, [user, server], [], 0]},
        {"SELECT max(version) FROM users",
         [users, ['max(version)'], [], 0]}
    ]).

build_insert_query_test() ->
    test_build_query_cases(fun wocky_db:build_insert_query/3, [
        {"INSERT INTO users (user) VALUES (?)",
         [users, [user], false]},
        {"INSERT INTO users (user, server) VALUES (?, ?)",
         [users, [user, server], false]},
        {"INSERT INTO users (user) VALUES (?) IF NOT EXISTS",
         [users, [user], true]},
        {"INSERT INTO users (user, server) VALUES (?, ?) IF NOT EXISTS",
         [users, [user, server], true]},
        {"INSERT INTO users (user) VALUES (?) USING TTL ?",
         [users, [user, '[ttl]'], false]},
        {"INSERT INTO users (user) VALUES (?) IF NOT EXISTS USING TTL ?",
         [users, [user, '[ttl]'], true]}
    ]).

build_update_query_test() ->
    test_build_query_cases(fun wocky_db:build_update_query/3, [
        {"UPDATE users SET password = ?",
         [users, [password], []]},
        {"UPDATE users SET password = ?, handle = ?",
         [users, [password, handle], []]},
        {"UPDATE users SET password = ? WHERE user = ?",
         [users, [password], [user]]},
        {"UPDATE users SET password = ? WHERE user = ? AND server = ?",
         [users, [password], [user, server]]},
        {"UPDATE users SET password = ?, handle = ? WHERE user = ?",
         [users, [password, handle], [user]]},
        {"UPDATE users SET password = ?, handle = ?"
         " WHERE user = ? AND server = ?",
         [users, [password, handle], [user, server]]}
    ]).

build_delete_query_test() ->
    test_build_query_cases(fun wocky_db:build_delete_query/3, [
        {"DELETE FROM users",
         [users, all, []]},
        {"DELETE FROM users",
         [users, [], []]},
        {"DELETE FROM users WHERE user = ?",
         [users, all, [user]]},
        {"DELETE FROM users WHERE user = ? AND server = ?",
         [users, all, [user, server]]},
        {"DELETE server FROM users",
         [users, [server], []]},
        {"DELETE user, server FROM users",
         [users, [user, server], []]},
        {"DELETE server FROM users WHERE user = ?",
         [users, [server], [user]]},
        {"DELETE server FROM users WHERE user = ? AND server = ?",
         [users, [server], [user, server]]}
    ]).

build_truncate_query_test() ->
    test_build_query_cases(fun wocky_db:build_truncate_query/1, [
        {"TRUNCATE TABLE users", [users]}
    ]).

build_drop_query_test() ->
    test_build_query_cases(fun wocky_db:build_drop_query/2, [
        {"DROP TABLE IF EXISTS users", [table, users]},
        {"DROP KEYSPACE IF EXISTS users", [keyspace, users]},
        {"DROP MATERIALIZED VIEW IF EXISTS users", ['materialized view', users]}
    ]).

build_create_keyspace_query_test() ->
    test_build_query_cases(fun wocky_db:build_create_keyspace_query/3, [
        {"CREATE KEYSPACE IF NOT EXISTS test_ks WITH REPLICATION = "
         "{'class': 'SimpleStrategy', 'replication_factor': 1}",
         [<<"test_ks">>, simple, 1]},
        {"CREATE KEYSPACE IF NOT EXISTS test_ks WITH REPLICATION = "
         "{'class': 'NetworkTopologyStrategy', 'dc1': 3}",
         [<<"test_ks">>, topology, [{dc1, 3}]]},
        {"CREATE KEYSPACE IF NOT EXISTS test_ks WITH REPLICATION = "
         "{'class': 'NetworkTopologyStrategy', 'dc1': 3, 'dc2': 2}",
         [<<"test_ks">>, topology, [{dc1, 3}, {dc2, 2}]]}
    ]).

build_create_table_query_test() ->
    TD = #table_def{name = test_tbl, columns = [{id, uuid}], primary_key = id},
    test_build_query_cases(fun wocky_db:build_create_table_query/1, [
        {"CREATE TABLE IF NOT EXISTS test_tbl (id uuid, PRIMARY KEY (id))",
         [TD]},
        {"CREATE TABLE IF NOT EXISTS test_tbl (id uuid, PRIMARY KEY (id))",
         [TD#table_def{primary_key = [id]}]},
        {"CREATE TABLE IF NOT EXISTS test_tbl"
         " (id uuid, PRIMARY KEY (foo, bar, baz))",
         [TD#table_def{primary_key = [foo, bar, baz]}]},
        {"CREATE TABLE IF NOT EXISTS test_tbl"
         " (id uuid, PRIMARY KEY ((foo, bar), baz))",
         [TD#table_def{primary_key = [[foo, bar], baz]}]},
        {"CREATE TABLE IF NOT EXISTS test_tbl"
         " (id uuid, PRIMARY KEY ((foo, bar, baz)))",
         [TD#table_def{primary_key = [[foo, bar, baz]]}]},
        {"CREATE TABLE IF NOT EXISTS test_tbl"
         " (first text, second text, PRIMARY KEY (id))",
         [TD#table_def{columns = [{first, text}, {second, text}]}]},
        {"CREATE TABLE IF NOT EXISTS test_tbl (id uuid, PRIMARY KEY (id))"
         " WITH CLUSTERING ORDER BY (foo ASC)",
         [TD#table_def{order_by = foo}]},
        {"CREATE TABLE IF NOT EXISTS test_tbl (id uuid, PRIMARY KEY (id))"
         " WITH CLUSTERING ORDER BY (foo ASC)",
         [TD#table_def{order_by = [{foo, asc}]}]},
        {"CREATE TABLE IF NOT EXISTS test_tbl (id uuid, PRIMARY KEY (id))"
         " WITH CLUSTERING ORDER BY (bar DESC)",
         [TD#table_def{order_by = [{bar, desc}]}]},
        {"CREATE TABLE IF NOT EXISTS test_tbl (id set<int>, PRIMARY KEY (id))",
         [TD#table_def{columns = [{id, {set, int}}]}]},
        {"CREATE TABLE IF NOT EXISTS test_tbl (id list<int>, PRIMARY KEY (id))",
         [TD#table_def{columns = [{id, {list, int}}]}]},
        {"CREATE TABLE IF NOT EXISTS test_tbl"
         " (id map<int,int>, PRIMARY KEY (id))",
         [TD#table_def{columns = [{id, {map, int, int}}]}]}
    ]).

build_create_index_query_test() ->
    test_build_query_cases(fun wocky_db:build_create_index_query/2, [
        {"CREATE INDEX IF NOT EXISTS ON users (user)",
         [users, [user]]},
        {"CREATE INDEX IF NOT EXISTS ON users (user, server)",
         [users, [user, server]]}
    ]).

build_create_view_query_test() ->
    test_build_query_cases(fun wocky_db:build_create_view_query/5, [
        {"CREATE MATERIALIZED VIEW IF NOT EXISTS roster_version AS"
         " SELECT * FROM roster"
         " WHERE user IS NOT NULL"
           " AND contact IS NOT NULL"
         " PRIMARY KEY (user, contact)"
         " WITH CLUSTERING ORDER BY (version ASC)",
        [roster_version, roster, all, [user, contact],
         [{version, asc}]]},
        {"CREATE MATERIALIZED VIEW IF NOT EXISTS roster_version AS"
         " SELECT * FROM roster"
         " WHERE user IS NOT NULL"
           " AND version IS NOT NULL"
           " AND contact IS NOT NULL"
         " PRIMARY KEY (user, version, contact)",
        [roster_version, roster, [], [user, version, contact], []]},
        {"CREATE MATERIALIZED VIEW IF NOT EXISTS roster_version AS"
         " SELECT user, version FROM roster"
         " WHERE user IS NOT NULL"
           " AND version IS NOT NULL"
           " AND contact IS NOT NULL"
         " PRIMARY KEY (user, version, contact)"
         " WITH CLUSTERING ORDER BY (version ASC)",
        [roster_version, roster, [user, version], [user, version, contact],
         [{version, asc}]]}
    ]).

build_count_query_test() ->
    test_build_query_cases(fun wocky_db:build_count_query/2, [
        {"SELECT COUNT(*) FROM users WHERE user = ? AND server = ?",
         [users, [user, server]]},
        {"SELECT COUNT(*) FROM roster",
         [roster, []]},
        {"SELECT COUNT(*) FROM roster WHERE user = ?",
         [roster, [user]]}
    ]).
