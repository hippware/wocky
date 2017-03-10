%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_cli.erl
-module(cli_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").
-include("wocky.hrl").

-import(mod_wocky_cli, [befriend/2]).

cli_test_() -> {
  "mod_wocky_cli",
  setup, fun before_all/0, fun after_all/1,
  [
    test_befriend()
  ]}.

before_all() ->
    ok = ?wocky_user:update(?ALICE, ?SERVER, #{handle => <<"alice">>}),
    ok = ?wocky_user:update(?BOB, ?SERVER, #{handle => <<"bob">>}),
    ok = ?wocky_user:wait_for_user(?BOB),
    ok = wocky_db:truncate(shared, roster),
    ok.

after_all(_) ->
    ok = ?wocky_user:delete(?ALICE, ?SERVER),
    ok = ?wocky_user:delete(?BOB, ?SERVER),
    ok.

test_befriend() ->
  { "Makes two users friends",
    [
      { "Succeeds with valid users", inorder, [
        ?_assertNot(wocky_db_roster:is_friend(?ALICE_JID, ?BOB_JID)),
        ?_assertNot(wocky_db_roster:is_friend(?BOB_JID, ?ALICE_JID)),
        ?_assertEqual({ok, "Success"},
                      befriend(<<"alice">>, <<"bob">>)),
        ?_assert(wocky_db_roster:is_friend(?ALICE_JID, ?BOB_JID)),
        ?_assert(wocky_db_roster:is_friend(?BOB_JID, ?ALICE_JID))
      ]},
      { "Returns an error on invalid user(s)", inparallel, [
        ?_assertMatch({error, _}, befriend(<<"alice">>, <<"bobbb">>)),
        ?_assertMatch({error, _}, befriend(<<"alish">>, <<"bob">>)),
        ?_assertMatch({error, _}, befriend(<<"xxx">>, <<"ffff">>))
      ]}
    ]
  }.
