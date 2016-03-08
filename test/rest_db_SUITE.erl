%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for wocky_reg
-module(rest_db_SUITE).

-export([
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([
         unauthorized/1,
         reset_db/1
        ]).

-include("wocky_db_seed.hrl").

-define(URL, "http://localhost:1096/wocky/v1/db/reset").

all() -> [{group, db}].

groups() ->
    [{db, [sequence], db_cases()}].

db_cases() ->
    [
     unauthorized,
     reset_db
    ].

init_per_suite(Config) ->
    test_helper:start_ejabberd(),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [user, auth_token]),
    ok = wocky_db_seed:prepare_tables(shared, [handle_to_user,
                                               phone_number_to_user]),
    Config.

end_per_suite(_Config) ->
    test_helper:stop_ejabberd(),
    ok.

init_per_group(_Group, ConfigIn) -> ConfigIn.

end_per_group(_Group, Config) -> Config.

init_per_testcase(_CaseName, Config) ->
    wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [user, auth_token]),
    wocky_db_seed:clear_tables(shared, [handle_to_user,
                                        phone_number_to_user]),
    ok = wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [user, auth_token]),
    ok = wocky_db_seed:seed_tables(shared, [handle_to_user,
                                            phone_number_to_user]),
    Config.

end_per_testcase(_CaseName, Config) ->
    Config.

%%%===================================================================
%%% Tests
%%%===================================================================

unauthorized(_) ->
    JSON = encode(unauthorized_test_data()),
    {ok, {403, _}} = request(JSON).

reset_db(_) ->
    wocky_db_user:update_user(#{user => ?ALICE, server => ?LOCAL_CONTEXT,
                                auth_user => <<"badauthuser">>}),

    JSON = encode(test_data()),
    {ok, {201, []}} = request(JSON),

    #{auth_user := ?AUTH_USER} = wocky_db_user:get_user_data(?ALICE,
                                                             ?LOCAL_CONTEXT).



%%%===================================================================
%%% Helpers
%%%===================================================================

encode(Data) ->
    iolist_to_binary(mochijson2:encode({struct, Data})).

request(Body) ->
    httpc:request(post, {?URL, [],
                  "application/json", Body}, [], [{full_result, false}]).

unauthorized_test_data() ->
    [
     {uuid, ?ALICE},
     {resource, ?RESOURCE},
     {sessionID, <<"asldkfjsadlkj">>}
    ].

test_data() ->
    [
     {uuid, ?ALICE},
     {resource, ?RESOURCE},
     {sessionID, ?TOKEN}
    ].

