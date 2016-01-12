%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_last_cassandra.erl
-module(mod_last_wocky_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DOMAIN, "localhost").

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
    ok = wocky_app:start().

after_all(_) ->
    ok = wocky_app:stop().

before_each() ->
    InitialVals = [{<<"bob">>, 666, <<"">>},
                   {<<"alicia">>, 777, <<"Ennui">>},
                   {<<"robert">>, 888, <<"Bored">>},
                   {<<"karen">>, 999, <<"Excited">>},
                   {<<"alice">>, 1000, <<"Still not here">>}],
    Values = [[{user, N}, {timestamp, T}, {status, S}] ||
              {N, T, S} <- InitialVals],
    Q = "INSERT INTO last_activity (user, timestamp, status) VALUES (?, ?, ?)",
    {ok, _} = wocky_db:batch_query(?DOMAIN, Q, Values, logged, quorum),
    ok.

after_each(_) ->
    {ok, _} = wocky_db:query(?DOMAIN, <<"TRUNCATE last_activity">>, quorum),
    ok.

test_set_last_info() ->
    { "set_last_info", foreach, fun before_each/0, fun after_each/1, [
        { "Creates a new user", [
            ?_assertMatch(not_found,
                          mod_last_wocky:get_last(<<"tim">>, ?DOMAIN)),
            ?_assertMatch(ok, mod_last_wocky:set_last_info(<<"tim">>, ?DOMAIN,
                          1024, <<"This is Tim's status">>)),
            ?_assertMatch({ok, 1024, <<"This is Tim's status">>},
                          mod_last_wocky:get_last(<<"tim">>, ?DOMAIN))
        ]},
        { "Returns an error for an insert into an unknown domain", [
            ?_assertMatch({error, _},
                          mod_last_wocky:set_last_info(<<"tim">>,
                             <<"otherdomain">>, 1024, <<"Tim's not here man">>))
        ]}
    ]}.

test_active_user_count() ->
    { "active_user_count", foreach, fun before_each/0, fun after_each/1, [
        { "Returns a count of active users for a given server and timestamp", [
            ?_assertMatch(5, mod_last_wocky:count_active_users(?DOMAIN, 0)),
            ?_assertMatch(3, mod_last_wocky:count_active_users(?DOMAIN, 800)),
            ?_assertMatch(2, mod_last_wocky:count_active_users(?DOMAIN, 998)),
            ?_assertMatch(1, mod_last_wocky:count_active_users(?DOMAIN, 999)),
            ?_assertMatch(0, mod_last_wocky:count_active_users(?DOMAIN, 1000))
        ]},
        { "Returns 0 for unknown domain", [
            ?_assertMatch(0,
                   mod_last_wocky:count_active_users(<<"otherdomain2">>, 1))
        ]}
    ]}.

test_last_activity() ->
    { "last_activity", foreach,  fun before_each/0, fun after_each/1, [
        { "Returns timestamp and status where record exists", [
            ?_assertMatch({ok, 1000, <<"Still not here">>},
                          mod_last_wocky:get_last(<<"alice">>, ?DOMAIN)),
            ?_assertMatch({ok, 666, <<"">>}, 
                          mod_last_wocky:get_last(<<"bob">>, ?DOMAIN)),
            ?_assertMatch({ok, 999, <<"Excited">>},
                          mod_last_wocky:get_last(<<"karen">>, ?DOMAIN)),
            ?_assertMatch({ok, 777, <<"Ennui">>},
                          mod_last_wocky:get_last(<<"alicia">>, ?DOMAIN))
        ]},
        { "Returns not_found when a record does not exist", [
            ?_assertMatch(not_found,
                          mod_last_wocky:get_last(<<"Bob">>, ?DOMAIN))
        ]},
        { "Returns an error when the domain is invalid", [
            ?_assertMatch({error, _},
                          mod_last_wocky:get_last(<<"xyzzy">>, <<"xyzzy">>))
        ]}
    ]}.

test_remove_user() ->
    { "remove_user", foreach, fun before_each/0, fun after_each/1, [
        { "Deletes existing users", [
            ?_assertMatch(ok, mod_last_wocky:remove_user(<<"bob">>, ?DOMAIN)),
            ?_assertMatch(ok,
                          mod_last_wocky:remove_user(<<"alicia">>, ?DOMAIN)),
            ?_assertMatch(3, mod_last_wocky:count_active_users(?DOMAIN, 0)),
            ?_assertMatch(not_found,
                          mod_last_wocky:get_last(<<"bob">>, ?DOMAIN)),
            ?_assertMatch(not_found,
                          mod_last_wocky:get_last(<<"alicia">>, ?DOMAIN))
        ]},
        { "Returns an error for an insert into an unknown domain", [
            ?_assertMatch({error, _},
                          mod_last_wocky:remove_user(<<"tim">>, <<"flibble">>))
        ]}
    ]}.
