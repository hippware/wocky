%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_roster.erl
-module(wocky_db_roster_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_roster.hrl").
-include("wocky_db_seed.hrl").


-import(wocky_db_roster, [get_roster/2,
                          get_roster_version/2,
                          get_roster_updates/3,
                          delete_roster/2,
                          get_roster_item/3,
                          update_roster_item/4,
                          delete_roster_item/3,
                          has_contact/2
                         ]).

mod_roster_wocky_test_() -> {
  "wocky_db_roster",
  setup, fun before_all/0, fun after_all/1,
  [
    test_get_roster(),
    test_get_roster_version(),
    test_get_roster_updates(),
    test_delete_roster(),
    test_get_roster_item(),
    test_update_roster_item(),
    test_delete_roster_item(),
    test_has_contact()
  ]
}.

before_all() ->
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(shared, [user]),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [roster]),
    ok.

after_all(_) ->
    ok = wocky_app:stop(),
    ok.

before_each() ->
    {ok, _} = wocky_db_seed:seed_table(shared, user),
    {ok, Data} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, roster),
    Data.

after_each(_) ->
    ok = wocky_db_seed:clear_tables(shared, [user]),
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [roster]),
    ok.

make_jid(U) ->
    wocky_db_seed:sjid(U, ?SERVER).

test_get_roster() ->
  { "get_roster/2", foreach, fun before_each/0, fun after_each/1, [
    { "returns the roster for a known user", [
      ?_assertMatch([#roster{}|_], get_roster(?USER, ?SERVER)),
      ?_assertEqual(4, length(get_roster(?USER, ?SERVER)))
    ]},
    { "returns an empty list for an unknown user", [
      ?_assertEqual([], get_roster(?BADUSER, ?SERVER))
    ]}
  ]}.

test_get_roster_version() ->
  { "get_roster_version/2", foreach, fun before_each/0, fun after_each/1, [
    { "returns the roster version for a known user", [
      ?_assertEqual(<<"999">>, get_roster_version(?USER, ?SERVER))
    ]},
    { "returns a null version for an unknown user", [
      ?_assertEqual(<<"0">>, get_roster_version(?BADUSER, ?SERVER))
    ]}
  ]}.

test_get_roster_updates() ->
  { "get_roster_updates/3", foreach, fun before_each/0, fun after_each/1, [
    { "returns an empty roster when the versions match", [
      ?_assertEqual([], get_roster_updates(?USER, ?SERVER,
                                           get_roster_version(?USER, ?SERVER)))
    ]},
    { "returns an empty roster when the specified version is too large", [
      ?_assertEqual([], get_roster_updates(?USER, ?SERVER, <<"9999">>))
    ]},
    { "returns a full roster when the specified version is too small", [
      ?_assertMatch([#roster{}|_], get_roster_updates(?USER, ?SERVER, <<"0">>)),
      ?_assertEqual(4, length(get_roster_updates(?USER, ?SERVER, <<"0">>)))
    ]},
    { "returns a partial roster when the versions do not match", [
      ?_assertMatch([#roster{}], get_roster_updates(?USER, ?SERVER, <<"888">>)),
      ?_assertEqual(2, length(get_roster_updates(?USER, ?SERVER, <<"777">>)))
    ]},
    { "returns an empty roster for an unknown user", [
      ?_assertEqual([], get_roster_updates(?BADUSER, ?SERVER, <<"0">>))
    ]}
  ]}.

test_delete_roster() ->
  { "delete_roster/2", foreach, fun before_each/0, fun after_each/1, [
    { "deletes all roster items for a known user", [
      ?_assertEqual(ok, delete_roster(?USER, ?SERVER)),
      ?_assertEqual([], get_roster(?USER, ?SERVER))
    ]},
    { "returns ok for an unknown user", [
      ?_assertEqual(ok, delete_roster(?BADUSER, ?SERVER))
    ]}
  ]}.

test_get_roster_item() ->
  { "get_roster_item/3", setup, fun before_each/0, fun after_each/1, [
      { "returns the roster item for a known user and contact", [
        ?_assertMatch(#roster{name = <<"bobby">>},
                      get_roster_item(?USER, ?SERVER, make_jid(?BOB)))
      ]},
      { "returns a roster item with avatar pulled from the user record", [
        ?_assertMatch(#roster{avatar = ?AVATAR_ID},
                      get_roster_item(?USER, ?SERVER, make_jid(?KAREN)))
      ]},
      { "returns a roster item with natural_name pulled from the user record", [
        ?_assertMatch(#roster{natural_name = <<"Carol">>},
                      get_roster_item(?USER, ?SERVER, make_jid(?CAROL))),
        ?_assertMatch(#roster{natural_name = <<"Karen Kismet">>},
                      get_roster_item(?USER, ?SERVER, make_jid(?KAREN))),
        ?_assertMatch(#roster{natural_name = <<"Robert The Bruce">>},
                      get_roster_item(?USER, ?SERVER, make_jid(?ROBERT)))
      ]},
      { "returns an empty roster item for a known user and unknown contact", [
        ?_assertMatch(#roster{name = <<>>},
                      get_roster_item(?USER, ?SERVER, make_jid(?BADUSER)))
      ]},
      { "returns an empty roster item for an unknown user", [
        ?_assertMatch(#roster{name = <<>>},
                      get_roster_item(?BADUSER, ?SERVER, make_jid(?BOB)))
      ]}
  ]}.

test_update_roster_item() ->
  { "update_roster_item/4", setup, fun before_each/0, fun after_each/1, [
      { "inserts a new roster item if one does not exist", [
        ?_assertEqual(ok, update_roster_item(?USER, ?SERVER, make_jid(?TIM),
                                             #roster{name = <<"tim">>})),
        ?_assertMatch(#roster{name = <<"tim">>},
                      get_roster_item(?USER, ?SERVER, make_jid(?TIM)))
      ]},
      { "updates an existing roster item", [
        ?_assertEqual(ok, update_roster_item(?USER, ?SERVER, make_jid(?TIM),
                                             #roster{name = <<"dan">>})),
        ?_assertMatch(#roster{name = <<"dan">>},
                      get_roster_item(?USER, ?SERVER, make_jid(?TIM)))
      ]}
  ]}.

test_delete_roster_item() ->
  { "delete_roster_item/3", setup, fun before_each/0, fun after_each/1, [
      { "deletes an existing roster item", [
        ?_assertEqual(ok, delete_roster_item(?USER, ?SERVER, make_jid(?BOB))),
        ?_assertMatch(#roster{name = <<>>},
                      get_roster_item(?USER, ?SERVER, make_jid(?BOB)))
      ]},
      { "returns ok if the roster item doesn't exist", [
        ?_assertEqual(ok, delete_roster_item(?USER, ?SERVER,
                                             make_jid(?BADUSER)))
      ]}
  ]}.

test_has_contact() ->
  { "has_contact/2", setup, fun before_each/0, fun after_each/1, [
    { "returns true when user A has user B in their roster", [
      ?_assert(has_contact(?ALICE_JID, ?BOB_JID)),
      ?_assert(has_contact(?ALICE_JID, ?CAROL_JID))
    ]},
    { "returns false when users are not friends", [
      ?_assertNot(has_contact(?CAROL_JID, ?ALICE_JID)),
      ?_assertNot(has_contact(?KAREN_JID, ?CAROL_JID))
    ]}
  ]}.
