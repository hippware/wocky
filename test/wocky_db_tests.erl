%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for ejabberd_auth_cassandra.erl
-module(wocky_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky.hrl").


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
                    wocky_db:query(<<"bogus">>, "SELECT * FROM user", quorum))
    ]}
]}.


test_build_query_cases(Fun, Cases) ->
    lists:foreach(
      fun ({Query, Args}) ->
          ?assertEqual(Query, lists:flatten(apply(Fun, Args)))
      end,
      Cases).

build_select_query_test() ->
    test_build_query_cases(fun wocky_db:build_select_query/3, [
        {"SELECT * FROM users",
         [users, all, []]},
        {"SELECT * FROM users WHERE user = ?",
         [users, all, [{user, foo}]]},
        {"SELECT * FROM users WHERE user = ? AND server = ?",
         [users, all, [{user, foo}, {server, bar}]]},
        {"SELECT user FROM users",
         [users, [user], []]},
        {"SELECT user FROM users WHERE server = ?",
         [users, [user], [{server, foo}]]},
        {"SELECT user, server FROM users",
         [users, [user, server], []]}
    ]).

build_insert_query_test() ->
    test_build_query_cases(fun wocky_db:build_insert_query/2, [
        {"INSERT INTO users (user) VALUES (?)",
         [users, [user]]},
        {"INSERT INTO users (user, server) VALUES (?, ?)",
         [users, [user, server]]}
    ]).

build_truncate_query_test() ->
    test_build_query_cases(fun wocky_db:build_truncate_query/1, [
        {"TRUNCATE TABLE users", [users]}
    ]).

build_drop_query_test() ->
    test_build_query_cases(fun wocky_db:build_drop_query/2, [
        {"DROP TABLE users", [table, users]},
        {"DROP KEYSPACE users", [keyspace, users]}
    ]).

build_create_keyspace_query_test() ->
    test_build_query_cases(fun wocky_db:build_create_keyspace_query/3, [
        {"CREATE KEYSPACE IF NOT EXISTS test_ks WITH REPLICATION = "
         "{'class': 'SimpleStrategy', 'replication_factor': 1}",
         [test_ks, simple, 1]},
        {"CREATE KEYSPACE IF NOT EXISTS test_ks WITH REPLICATION = "
         "{'class': 'NetworkTopologyStrategy', 'dc1': 3}",
         [test_ks, topology, [{dc1, 3}]]},
        {"CREATE KEYSPACE IF NOT EXISTS test_ks WITH REPLICATION = "
         "{'class': 'NetworkTopologyStrategy', 'dc1': 3, 'dc2': 2}",
         [test_ks, topology, [{dc1, 3}, {dc2, 2}]]}
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
         [TD#table_def{order_by = [{bar, desc}]}]}
    ]).

build_create_index_query_test() ->
    test_build_query_cases(fun wocky_db:build_create_index_query/2, [
        {"CREATE INDEX IF NOT EXISTS ON users (user)",
         [users, [user]]},
        {"CREATE INDEX IF NOT EXISTS ON users (user, server)",
         [users, [user, server]]}
    ]).


%% This is a very simple test to verify that basic communication with
%% Cassandra works. It probably isn't worth testing this functionality more
%% thoroughly since at that point we would essentially be testing the
%% Cassandra driver.

wocky_db_api_smoke_test() ->
    ok = wocky_app:start(),

    Q1 = "INSERT INTO handle_to_user (user, server, handle) VALUES (?, ?, ?)",
    Values = [
      #{user => now, server => <<"localhost">>, handle => <<"alice">>},
      #{user => now, server => <<"localhost">>, handle => <<"bob">>},
      #{user => now, server => <<"localhost">>, handle => <<"charlie">>}
    ],
    ok = wocky_db:multi_query(shared, Q1, Values, quorum),

    %% You aren't supposed to use batches like this, but this is just a test
    Queries = [
      {Q1, #{user => now, server => <<"localhost">>, handle => <<"dan">>}},
      {Q1, #{user => now, server => <<"localhost">>, handle => <<"ed">>}},
      {Q1, #{user => now, server => <<"localhost">>, handle => <<"frank">>}}
    ],
    {ok, void} = wocky_db:batch_query(shared, Queries, logged, quorum),

    QueryVals = [
      {Q1, #{user => now, server => <<"localhost">>, handle => <<"gary">>}},
      {Q1, #{user => now, server => <<"localhost">>, handle => <<"harry">>}},
      {Q1, #{user => now, server => <<"localhost">>, handle => <<"inigo">>}}
    ],
    ok = wocky_db:multi_query(shared, QueryVals, quorum),

    Q2 = "SELECT handle FROM handle_to_user",
    {ok, R1} = wocky_db:query(shared, Q2, quorum),
    ?assert(is_list(wocky_db:rows(R1))),
    ?assertEqual(9, length(wocky_db:rows(R1))),

    ?assert(is_map(wocky_db:single_row(R1))),
    ?assertEqual(1, maps:size(wocky_db:single_row(R1))),

    NotBob = fun (#{handle := H}) -> H =/= <<"bob">> end,
    ?assertEqual(8, wocky_db:count(NotBob, R1)),

    Q3 = "TRUNCATE handle_to_user",
    {ok, _} = wocky_db:query(shared, Q3, quorum),

    {ok, R2} = wocky_db:query(shared, Q2, quorum),
    ?assertEqual(0, length(wocky_db:rows(R2))),
    ?assertEqual(undefined, wocky_db:single_row(R2)),

    wocky_app:stop(),
    ok.
