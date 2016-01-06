%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_last_cassandra.erl
-module(mod_last_wocky_tests).

-include_lib("eunit/include/eunit.hrl").


mod_last_wocky_test_() -> {
  "mod_last_wocky",
  setup, fun before_all/0, fun after_all/1,
  [
    test_active_user_count(),
    test_last()
  ]
}.

before_all() ->
    ok = wocky_app:start(),
    ok.

after_all(_) ->
    ok = wocky_app:stop(),
    ok.

before_each() ->
    mod_last_wocky:set_last_info(<<"alice">>, <<"localhost">>, 555, <<"Not here">>),
    mod_last_wocky:set_last_info(<<"bob">>, <<"localhost">>, 666, <<"">>),
    mod_last_wocky:set_last_info(<<"alicia">>, <<"remotehost">>, 777, <<"Ennui">>),
    mod_last_wocky:set_last_info(<<"robert">>, <<"remotehost">>, 888, <<"Bored">>),
    mod_last_wocky:set_last_info(<<"karen">>, <<"localhost">>, 999, <<"Excited">>),
    mod_last_wocky:set_last_info(<<"alice">>, <<"localhost">>, 1000, <<"Still not here">>), % Update alice
    ok.

after_each(_) ->
    [mod_last_wocky:remove_user(U, <<"localhost">>) || U <- [<<"alice">>, <<"bob">>, <<"karen">>]],
    [mod_last_wocky:remove_user(U, <<"remotehost">>) || U <- [<<"robert">>, <<"alicia">>]],
    ok.

%% Simple placeholder test. Delete and replace with something more meaningful.
test_active_user_count() ->
    { "active_user_count", foreach, fun before_each/0, fun after_each/1, [
        { "Count users", [
            ?_assertMatch(3, mod_last_wocky:count_active_users(<<"localhost">>, 0)),
            ?_assertMatch(2, mod_last_wocky:count_active_users(<<"remotehost">>, 0)),
            ?_assertMatch(1, mod_last_wocky:count_active_users(<<"remotehost">>, 800)),
            ?_assertMatch(2, mod_last_wocky:count_active_users(<<"localhost">>, 998)),
            ?_assertMatch(1, mod_last_wocky:count_active_users(<<"localhost">>, 999)),
            ?_assertMatch(0, mod_last_wocky:count_active_users(<<"localhost">>, 1000)),
            ?_assertMatch(1, mod_last_wocky:count_active_users(<<"remotehost">>, 800)),
            ?_assertMatch(0, mod_last_wocky:count_active_users(<<"otherhost">>, 1))
        ]}
    ]}.

test_last() ->
    { "last_activity", foreach,  fun before_each/0, fun after_each/1, [
        { "Check last activity", [
            ?_assertMatch({ok, 1000, <<"Still not here">>}, mod_last_wocky:get_last(<<"alice">>, <<"localhost">>)),
            ?_assertMatch({ok, 666, <<"">>}, mod_last_wocky:get_last(<<"bob">>, <<"localhost">>)),
            ?_assertMatch(not_found, mod_last_wocky:get_last(<<"Bob">>, <<"localhost">>)),
            ?_assertMatch({ok, 999, <<"Excited">>}, mod_last_wocky:get_last(<<"karen">>, <<"localhost">>)),
            ?_assertMatch(not_found, mod_last_wocky:get_last(<<"karen">>, <<"remotehost">>)),
            ?_assertMatch(not_found, mod_last_wocky:get_last(<<"xyzzy">>, <<"xyzzy">>))
        ]}
    ]}.
