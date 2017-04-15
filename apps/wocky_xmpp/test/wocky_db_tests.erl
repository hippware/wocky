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
