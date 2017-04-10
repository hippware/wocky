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
    _ = ?wocky_repo:delete_all(?wocky_user),
    _ = ?wocky_factory:insert(user, #{id => ?ALICE,
                                      username => ?ALICE,
                                      handle => <<"alice">>}),
    _ = ?wocky_factory:insert(user, #{id => ?BOB,
                                      username => ?BOB,
                                      handle => <<"bob">>}),
    ok = wocky_db:truncate(shared, roster),
    ok.

after_all(_) ->
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
