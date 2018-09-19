%%% @copyright 2017+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_notifications
-module(notifications_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [new_follower].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    Config1 = test_helper:setup_users(Config, [alice, bob]),
    meck:new(?wocky_push, [passthrough]),
    meck:expect(?wocky_push, notify_all, fun(_, _) -> ok end),
    escalus:init_per_testcase(CaseName, Config1).

end_per_testcase(CaseName, Config) ->
    meck:unload(),
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% mod_wocky_notification tests
%%--------------------------------------------------------------------

new_follower(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun (Alice, Bob) ->
        test_helper:follow(Alice, Bob),
        timer:sleep(500),

        [{_, {?wocky_push, notify_all,
              [?BOB, #{user := EUser, follower := EFollower}]}, _}]
        = meck:history(?wocky_push),

        ?assertEqual(maps:get(id, EUser), ?BOB),
        ?assertEqual(maps:get(id, EFollower), ?ALICE),

        % A follow-back (ie a befriending) should not generate an additional
        % notification
        meck:reset(?wocky_push),
        test_helper:follow(Bob, Alice),
        ?assertEqual(meck:num_calls(?wocky_push, notify_all, '_'), 0)
      end).
