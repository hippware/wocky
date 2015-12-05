%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for wocky_db_user.erl
-module(wocky_db_user_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(DOMAIN, <<"localhost">>).
-define(USERNAME, <<"bob">>).
-define(PASSWORD, <<"password">>).

%%
%% Suite configuration
%%

all() -> [
          create_user_creates_user,
          create_user_fails_if_user_exists,
          get_password_gets_password,
          get_password_fails_if_user_dne,
          set_password_sets_password,
          set_password_fails_if_user_dne,
          remove_user_removes_user,
          remove_user_succeeds_if_user_dne
         ].

suite() ->
    [{timetrap, {seconds, 30}}].


%%
%% Setup/Teardown
%%

init_per_suite(Config) ->
    AppConfig = [
        {host, "localhost"},
        {wocky_db_seestar, [
            {auth, {seestar_password_auth, {<<"cassandra">>, <<"cassandra">>}}},
            {keyspaces, [
                {host, [{keyspace, "wocky_test_%h"}]},
                {shared, [{keyspace, "wocky_test_shared"}]}
            ]}
        ]}
    ],
    ok = wocky_app:start(AppConfig),
    Config.

end_per_suite(_Config) ->
    ok = wocky_app:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    {ok, _} = wocky_db:pquery(shared, <<"TRUNCATE username_to_user">>, [], quorum),
    {ok, _} = wocky_db:pquery(?DOMAIN, <<"TRUNCATE user">>, [], quorum),
    ok.


%%
%% Testcases
%%

create_user_creates_user(_Config) ->
    false = wocky_db_user:does_user_exist(?DOMAIN, ?USERNAME),
    ok    = wocky_db_user:create_user(?DOMAIN, ?USERNAME, ?PASSWORD),
    true  = wocky_db_user:does_user_exist(?DOMAIN, ?USERNAME),
    ok.

create_user_fails_if_user_exists(_Config) ->
    ok = wocky_db_user:create_user(?DOMAIN, ?USERNAME, ?PASSWORD),
    true  = wocky_db_user:does_user_exist(?DOMAIN, ?USERNAME),
    {error, exists} = wocky_db_user:create_user(?DOMAIN, ?USERNAME, ?PASSWORD),
    ok.

get_password_gets_password(_Config) ->
    ok = wocky_db_user:create_user(?DOMAIN, ?USERNAME, ?PASSWORD),
    ?PASSWORD = wocky_db_user:get_password(?DOMAIN, ?USERNAME),
    ok.

get_password_fails_if_user_dne(_Config) ->
    false = wocky_db_user:does_user_exist(?DOMAIN, ?USERNAME),
    {error, not_found} = wocky_db_user:get_password(?DOMAIN, ?USERNAME),
    ok.

set_password_sets_password(_Config) ->
    ok = wocky_db_user:create_user(?DOMAIN, ?USERNAME, ?PASSWORD),
    ok = wocky_db_user:set_password(?DOMAIN, ?USERNAME, <<"newpass">>),
    <<"newpass">> = wocky_db_user:get_password(?DOMAIN, ?USERNAME),
    ok.

set_password_fails_if_user_dne(_Config) ->
    false = wocky_db_user:does_user_exist(?DOMAIN, ?USERNAME),
    {error, not_found} = wocky_db_user:set_password(?DOMAIN, ?USERNAME, <<"newpass">>),
    ok.

remove_user_removes_user(_Config) ->
    ok    = wocky_db_user:create_user(?DOMAIN, ?USERNAME, ?PASSWORD),
    true  = wocky_db_user:does_user_exist(?DOMAIN, ?USERNAME),
    ok    = wocky_db_user:remove_user(?DOMAIN, ?USERNAME),
    false = wocky_db_user:does_user_exist(?DOMAIN, ?USERNAME),
    ok.

remove_user_succeeds_if_user_dne(_Config) ->
    false = wocky_db_user:does_user_exist(?DOMAIN, ?USERNAME),
    ok    = wocky_db_user:remove_user(?DOMAIN, ?USERNAME),
    ok.
