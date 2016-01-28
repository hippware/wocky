%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_last_cassandra.erl
-module(mod_last_wocky_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").


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

uuid(Name, Users) ->
    E = lists:keyfind(Name, 1, Users),
    element(4, E).

before_each() ->
    Users = [{<<"bob">>, 666, <<"">>, ossp_uuid:make(v1, text)},
             {<<"alicia">>, 777, <<"Ennui">>, ossp_uuid:make(v1, text)},
             {<<"robert">>, 888, <<"Bored">>, ossp_uuid:make(v1, text)},
             {<<"karen">>, 999, <<"Excited">>, ossp_uuid:make(v1, text)},
             {<<"alice">>, 1000, <<"Not here">>, ossp_uuid:make(v1, text)}],

    InactiveUsers = [{<<"tim">>, not_set, not_set, ossp_uuid:make(v1, text)}],

    Values = [#{user => UID, timestamp => wocky_db:seconds_to_timestamp(T),
                status => S, server => ?SERVER} || {_, T, S, UID} <- Users],

    Q = "INSERT INTO last_activity (user, server, timestamp, status)"
        " VALUES (?, ?, ?, ?)",
    wocky_db:multi_query(?SERVER, Q, Values, quorum),
    Users ++ InactiveUsers.

after_each(_) ->
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [last_activity]),
    ok.

test_set_last_info() ->
    { "set_last_info", setup, fun before_each/0, fun after_each/1,
      fun(Users) -> [
        { "Creates a new user and validates their state", [
            ?_assertMatch(not_found,
                          mod_last_wocky:get_last(uuid(<<"tim">>, Users),
                                                  ?SERVER)),
            ?_assertMatch(ok,
                          mod_last_wocky:set_last_info(uuid(<<"tim">>, Users),
                                                  ?SERVER,
                          1024, <<"This is Tim's status">>)),
            ?_assertMatch({ok, 1024, "This is Tim's status"},
                          mod_last_wocky:get_last(uuid(<<"tim">>, Users),
                                                  ?SERVER))
        ]}
    ] end}.

test_active_user_count() ->
    { "active_user_count", foreach, fun before_each/0, fun after_each/1, [
        { "Returns a count of active users for a given server and timestamp", [
            ?_assertMatch(5, mod_last_wocky:count_active_users(?SERVER, 0)),
            ?_assertMatch(3, mod_last_wocky:count_active_users(?SERVER, 800)),
            ?_assertMatch(2, mod_last_wocky:count_active_users(?SERVER, 998)),
            ?_assertMatch(1, mod_last_wocky:count_active_users(?SERVER, 999)),
            ?_assertMatch(0, mod_last_wocky:count_active_users(?SERVER, 1000))
        ]}
    ]}.

test_last_activity() ->
    { "last_activity", setup,  fun before_each/0, fun after_each/1,
      fun(Users) -> [
        { "Returns timestamp and status where record exists", [
            ?_assertMatch({ok, 1000, "Not here"},
                          mod_last_wocky:get_last(uuid(<<"alice">>, Users),
                                                  ?SERVER)),
            ?_assertMatch({ok, 666, ""},
                          mod_last_wocky:get_last(uuid(<<"bob">>, Users),
                                                  ?SERVER)),
            ?_assertMatch({ok, 999, "Excited"},
                          mod_last_wocky:get_last(uuid(<<"karen">>, Users),
                                                  ?SERVER)),
            ?_assertMatch({ok, 777, "Ennui"},
                          mod_last_wocky:get_last(uuid(<<"alicia">>, Users),
                                                  ?SERVER))
        ]},
        { "Returns not_found when a record does not exist", [
            ?_assertMatch(not_found,
                          mod_last_wocky:get_last(
                            ossp_uuid:make(v1, text), ?SERVER))
        ]}
    ] end}.

test_remove_user() ->
    { "remove_user", setup, fun before_each/0, fun after_each/1, fun(Users) -> [
        { "Deletes existing users", [
            ?_assertMatch(ok, mod_last_wocky:remove_user(uuid(<<"bob">>,
                                                              Users), ?SERVER)),
            ?_assertMatch(ok, mod_last_wocky:remove_user(uuid(<<"alicia">>,
                                                              Users), ?SERVER)),
            ?_assertMatch(3, mod_last_wocky:count_active_users(?SERVER, 0)),
            ?_assertMatch(not_found, mod_last_wocky:get_last(uuid(<<"bob">>,
                                                              Users), ?SERVER)),
            ?_assertMatch(not_found, mod_last_wocky:get_last(uuid(<<"alicia">>,
                                                              Users), ?SERVER))
        ]}
    ] end}.
