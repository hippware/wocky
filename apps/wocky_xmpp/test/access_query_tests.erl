%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for access_query.erl
-module(access_query_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").

-import(access_query, [run/3]).

-export([check_access/3]).

access_query_test_() -> {
  "access_query",
  setup, fun before_all/0, fun after_all/1, [
    {inorder,
      [
        test_run(),
        test_loop(),
        test_overflow(),
        test_timeout()
      ]
    }]
}.

before_all() ->
    _ = ?wocky_repo:delete_all(?wocky_user),
    Alice = ?wocky_factory:insert(user, #{id => ?ALICE,
                                          username => ?ALICE,
                                          handle => <<"alice">>}),
    Bob = ?wocky_factory:insert(user, #{id => ?BOB,
                                        username => ?BOB,
                                        handle => <<"bob">>}),
    Bot = ?wocky_factory:insert(bot, #{id => ?BOT, user => Alice}),

    ?wocky_bot:share(Bot, Bob, Alice),
    ?wocky_user:subscribe(Bob, Bot),

    setup_errors(),
    ok.

after_all(_) ->
    ok.

setup_errors() ->
    mod_wocky_access:register(<<"loop">>, ?MODULE),
    mod_wocky_access:register(<<"overflow">>, ?MODULE),
    mod_wocky_access:register(<<"timeout">>, ?MODULE).

check_access(<<"loop/1">>, _, _) ->
    {redirect, jid:make(<<>>, ?SERVER, <<"loop/2">>)};
check_access(<<"loop/2">>, _, _) ->
    {redirect, jid:make(<<>>, ?SERVER, <<"loop/1">>)};
check_access(<<"overflow/", I/binary>>, _, _) ->
    J = integer_to_binary(binary_to_integer(I) + 1),
    {redirect, jid:make(<<>>, ?SERVER, <<"overflow/", J/binary>>)};
check_access(<<"timeout">>, _, _) ->
    timer:sleep(5000).

test_run() -> {
  "run", [
    { "Forward query to a bot should return bot permissions", [
      ?_assertEqual(allow, run(?BOT_JID, ?ALICE_JID, view)),
      ?_assertEqual(allow, run(?BOT_JID, ?ALICE_JID, delete)),
      ?_assertEqual(allow, run(?BOT_JID, ?ALICE_JID, modify)),

      ?_assertEqual(allow, run(?BOT_JID, ?BOB_JID, view)),
      ?_assertEqual(deny, run(?BOT_JID, ?BOB_JID, delete)),
      ?_assertEqual(deny, run(?BOT_JID, ?BOB_JID, modify)),

      ?_assertEqual(deny, run(?BOT_JID, ?CAROL_JID, view)),
      ?_assertEqual(deny, run(?BOT_JID, ?CAROL_JID, delete)),
      ?_assertEqual(deny, run(?BOT_JID, ?CAROL_JID, modify))
    ]}
  ]}.

test_loop() -> {
  "run with redirect loop", [
     ?_assertEqual(deny, run(jid:make(<<>>, ?SERVER, <<"loop/1">>),
                             ?ALICE_JID, view))
  ]}.

test_overflow() -> {
  "run with redirect overflow", [
     ?_assertEqual(deny, run(jid:make(<<>>, ?SERVER, <<"overflow/1">>),
                             ?ALICE_JID, view))
  ]}.

test_timeout() -> {
  "run with timeout", [
     ?_assertEqual(deny, run(jid:make(<<>>, ?SERVER, <<"timeout">>),
                             ?ALICE_JID, view))
  ]}.
