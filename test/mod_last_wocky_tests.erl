%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_last_cassandra.erl
-module(mod_last_wocky_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").

-import(mod_last_wocky, [get_last/2, count_active_users/2, set_last_info/4,
                         remove_user/2]).


mod_last_wocky_test_() -> {
  "mod_last_wocky",
  setup, fun before_all/0, fun after_all/1,
  [
    test_set_last_info(),
    test_active_user_count(),
    test_last_activity(),
    test_remove_user()
  ]
}.

before_all() ->
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [last_activity]),
    ok.

after_all(_) ->
    ok = wocky_app:stop().

before_each() ->
    {ok, _} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, last_activity),
    ok.

after_each(_) ->
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [last_activity]).

test_set_last_info() ->
    { "set_last_info", setup, fun before_each/0, fun after_each/1, [
        { "Creates a new user and validates their state", [
            ?_assertMatch(not_found, get_last(?TIM, ?SERVER)),
            ?_assertMatch(ok, set_last_info(?TIM, ?SERVER, 1024,
                                            <<"This is Tim's status">>)),
            ?_assertMatch({ok, 1024, "This is Tim's status"},
                          get_last(?TIM, ?SERVER))
        ]}
    ]}.

test_active_user_count() ->
    { "active_user_count", foreach, fun before_each/0, fun after_each/1, [
        { "Returns a count of active users for a given server and timestamp", [
            ?_assertMatch(5, count_active_users(?SERVER, 0)),
            ?_assertMatch(3, count_active_users(?SERVER, 800)),
            ?_assertMatch(2, count_active_users(?SERVER, 998)),
            ?_assertMatch(1, count_active_users(?SERVER, 999)),
            ?_assertMatch(0, count_active_users(?SERVER, 1000))
        ]}
    ]}.

test_last_activity() ->
    { "last_activity", setup,  fun before_each/0, fun after_each/1, [
        { "Returns timestamp and status where record exists", [
            ?_assertMatch({ok, 1000, "Not here"},
                          get_last(?ALICE, ?SERVER)),
            ?_assertMatch({ok, 666, ""},
                          get_last(?BOB, ?SERVER)),
            ?_assertMatch({ok, 999, "Excited"},
                          get_last(?KAREN, ?SERVER)),
            ?_assertMatch({ok, 777, "Ennui"},
                          get_last(?CAROL, ?SERVER))
        ]},
        { "Returns not_found when a record does not exist", [
            ?_assertMatch(not_found, get_last(?BADUSER, ?SERVER))
        ]}
    ]}.

test_remove_user() ->
    { "remove_user", setup, fun before_each/0, fun after_each/1, [
        { "Deletes existing users", [
            ?_assertMatch(ok, remove_user(?BOB, ?SERVER)),
            ?_assertMatch(ok, remove_user(?CAROL, ?SERVER)),
            ?_assertMatch(3, count_active_users(?SERVER, 0)),
            ?_assertMatch(not_found, get_last(?BOB, ?SERVER)),
            ?_assertMatch(not_found, get_last(?CAROL, ?SERVER))
        ]}
    ]}.
