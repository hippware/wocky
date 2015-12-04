%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for ejabberd_auth_cassandra.erl
-module(cassandra_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


%%
%% Suite configuration
%%

all() ->
    [my_test_case].

groups() ->
    [].

suite() ->
    [{timetrap, {seconds, 30}}].


%%
%% Setup/Teardown
%%

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.


%%
%% Testcases
%%

my_test_case(_Config) ->
    ok.
