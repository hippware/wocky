%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite mod_wocky_access.erl
-module(access_SUITE).

-compile(export_all).
-compile({parse_transform, fun_chain}).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").

-import(test_helper, [expect_iq_success/2]).


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          bot_access
         ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    Config2 = fun_chain:first(Config,
        escalus:init_per_suite(),
        test_helper:setup_users([alice, tim, bob])
    ),

    Alice = ?wocky_user:find(?ALICE),
    Bob = ?wocky_user:find(?BOB),

    Bot = ?wocky_factory:insert(bot, #{id => ?BOT, user => Alice}),

    ?wocky_bot:share(Bot, Bob, Alice),

    Config2.

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

bot_access(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
      %% Alice can see her own bot
      expect_allow(
        expect_iq_success(
          access_query(bot_node(?BOT), ?BJID(?ALICE), <<"view">>), Alice)),

      %% Tim cannot see Alice's bot because he's not a friend
      expect_deny(
        expect_iq_success(
          access_query(bot_node(?BOT), ?BJID(?TIM), <<"view">>), Alice)),

      %% Bob can see the bot because he's an affiliate
      expect_allow(
        expect_iq_success(
          access_query(bot_node(?BOT), ?BJID(?BOB), <<"view">>), Alice)),

      %% Alice canedit it because she's the owner
      expect_allow(
        expect_iq_success(
          access_query(bot_node(?BOT), ?BJID(?ALICE), <<"modify">>), Alice)),
      expect_allow(
        expect_iq_success(
          access_query(bot_node(?BOT), ?BJID(?ALICE), <<"delete">>), Alice)),

      %% Bob cannot edit it because he's not the owner
      expect_deny(
        expect_iq_success(
          access_query(bot_node(?BOT), ?BJID(?BOB), <<"modify">>), Alice)),
      expect_deny(
        expect_iq_success(
          access_query(bot_node(?BOT), ?BJID(?BOB), <<"delete">>), Alice)),

      %% Nobody has permissions on a non-existant bot
      expect_deny(
        expect_iq_success(
          access_query(bot_node(?wocky_id:new()),
                       ?BJID(?ALICE), <<"view">>), Alice))
    end).

access_query(ID, Actor, Op) ->
    test_helper:iq_get(?NS_ACCESS,
                       #xmlel{name = <<"query">>,
                              attrs = [{<<"node">>, <<ID/binary>>},
                                       {<<"actor">>, Actor},
                                       {<<"op">>, Op}]}).

expect_allow(#xmlel{children = [#xmlel{name = <<"allow">>}]}) -> ok;
expect_allow(Stanza) -> ct:fail("Not an allow stanza: ~p", [Stanza]).
expect_deny(#xmlel{children = [#xmlel{name = <<"deny">>}]}) -> ok;
expect_deny(Stanza) -> ct:fail("Not a deny stanza: ~p", [Stanza]).
expect_redirect(#xmlel{children = [#xmlel{name = <<"redirect">>,
                                          children =
                                          [#xmlcdata{content = Target}]}]},
                Target) -> ok;
expect_redirect(Stanza, Target) ->
    ct:fail("Invalid redirect stanza (target ~p): ~p", [Target, Stanza]).

bot_node(ID) ->
    <<"bot/", ID/binary>>.
