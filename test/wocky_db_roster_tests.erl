%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_roster.erl
-module(wocky_db_roster_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_roster.hrl").
-include("wocky_db_seed.hrl").


-import(wocky_db_roster, [get_roster/2,
                          get_roster_version/2,
                          delete_roster/2,
                          get_roster_item/3,
                          update_roster_item/4,
                          delete_roster_item/3,
                          has_contact/2,
                          is_friend/2,
                          users_with_contact/1,
                          bump_roster_version/2
                         ]).

mod_roster_wocky_test_() -> {
  "wocky_db_roster",
  setup, fun before_all/0, fun after_all/1,
  {foreach, fun before_each/0, fun after_each/1, [
    {inparallel, [
      test_get_roster(),
      test_get_roster_version(),
      test_get_roster_item(),
      test_has_contact(),
      test_is_friend(),
      test_users_with_contact()
    ]},
    test_bump_roster_version(),
    test_update_roster_item(),
    test_delete_roster_item(),
    test_delete_roster()
  ]}
}.

before_all() ->
    ok.

after_all(_) ->
    ok.

before_each() ->
    wocky_db_seed:seed_tables(shared, [user, roster]).

after_each(_) ->
    ok.

make_jid(U) ->
    jid:to_binary(jid:make(U, ?SERVER, <<>>)).

test_get_roster() ->
  { "get_roster/2", [
    { "returns the roster for a known user", [
      ?_assertMatch([#wocky_roster{}|_], get_roster(?USER, ?SERVER)),
      ?_assertEqual(4, length(get_roster(?USER, ?SERVER)))
    ]},
    { "returns an empty list for an unknown user", [
      ?_assertEqual([], get_roster(?BADUSER, ?SERVER))
    ]}
  ]}.

test_get_roster_version() ->
  { "get_roster_version/2", [
    { "returns the roster version for a known user", [
      ?_assertEqual(<<"999-4">>, get_roster_version(?USER, ?SERVER))
    ]},
    { "returns a null version for an unknown user", [
      ?_assertEqual(<<"0-0">>, get_roster_version(?BADUSER, ?SERVER))
    ]}
  ]}.

test_delete_roster() ->
  { "delete_roster/2", [
    { "deletes all roster items for a known user", [
      ?_assertEqual(ok, delete_roster(?USER, ?SERVER)),
      ?_assertEqual([], get_roster(?USER, ?SERVER))
    ]},
    { "returns ok for an unknown user", [
      ?_assertEqual(ok, delete_roster(?BADUSER, ?SERVER))
    ]}
  ]}.

test_get_roster_item() ->
  { "get_roster_item/3", [
      { "returns the roster item for a known user and contact", [
        ?_assertMatch(#wocky_roster{name = <<"bobby">>},
                      get_roster_item(?USER, ?SERVER, make_jid(?BOB)))
      ]},
      { "returns a roster item with avatar pulled from the user record", [
        ?_assertMatch(#wocky_roster{avatar = ?AVATAR_ID},
                      get_roster_item(?USER, ?SERVER, make_jid(?KAREN)))
      ]},
      { "returns a roster item with name pulled from the user record", [
        ?_assertMatch(#wocky_roster{first_name = <<"Carol">>, last_name = <<>>},
                      get_roster_item(?USER, ?SERVER, make_jid(?CAROL))),
        ?_assertMatch(#wocky_roster{first_name = <<"Karen">>,
                              last_name = <<"Kismet">>},
                      get_roster_item(?USER, ?SERVER, make_jid(?KAREN))),
        ?_assertMatch(#wocky_roster{first_name = <<>>,
                              last_name = <<"Robert The Bruce">>},
                      get_roster_item(?USER, ?SERVER, make_jid(?ROBERT)))
      ]},
      { "returns an empty roster item for a known user and unknown contact", [
        ?_assertMatch(#wocky_roster{name = <<>>},
                      get_roster_item(?USER, ?SERVER, make_jid(?BADUSER)))
      ]},
      { "returns an empty roster item for an unknown user", [
        ?_assertMatch(#wocky_roster{name = <<>>},
                      get_roster_item(?BADUSER, ?SERVER, make_jid(?BOB)))
      ]}
  ]}.

test_update_roster_item() ->
  { "update_roster_item/4", [
      { "inserts a new roster item if one does not exist", [
        ?_assertEqual(ok, update_roster_item(?USER, ?SERVER, make_jid(?TIM),
                                             #wocky_roster{name = <<"tim">>})),
        ?_assertMatch(#wocky_roster{name = <<"tim">>},
                      get_roster_item(?USER, ?SERVER, make_jid(?TIM)))
      ]},
      { "updates an existing roster item", [
        ?_assertEqual(ok, update_roster_item(?USER, ?SERVER, make_jid(?TIM),
                                             #wocky_roster{name = <<"dan">>})),
        ?_assertMatch(#wocky_roster{name = <<"dan">>},
                      get_roster_item(?USER, ?SERVER, make_jid(?TIM)))
      ]}
  ]}.

test_bump_roster_version() ->
  { "bump_roster_version/2", [
      { "sets the roster version for a contact to a new value", [
        ?_assertEqual(<<"999-4">>, get_roster_version(?USER, ?LOCAL_CONTEXT)),
        ?_assertEqual(ok, bump_roster_version(?USER, ?BOB_B_JID)),
        ?_assertNotEqual(<<"999-4">>, get_roster_version(?USER, ?LOCAL_CONTEXT))
      ]}
  ]}.

test_delete_roster_item() ->
  { "delete_roster_item/3", [
      { "deletes an existing roster item", [
        ?_assertEqual(ok, delete_roster_item(?USER, ?SERVER, make_jid(?BOB))),
        ?_assertMatch(#wocky_roster{name = <<>>},
                      get_roster_item(?USER, ?SERVER, make_jid(?BOB)))
      ]},
      { "returns ok if the roster item doesn't exist", [
        ?_assertEqual(ok, delete_roster_item(?USER, ?SERVER,
                                             make_jid(?BADUSER)))
      ]}
  ]}.

test_has_contact() ->
  { "has_contact/2", [
    { "returns true when user A has user B in their roster", [
      ?_assert(has_contact(?ALICE_JID, ?BOB_JID)),
      ?_assert(has_contact(?ALICE_JID, ?CAROL_JID))
    ]},
    { "returns false when A does not have B in their roster", [
      ?_assertNot(has_contact(?CAROL_JID, ?ALICE_JID)),
      ?_assertNot(has_contact(?KAREN_JID, ?CAROL_JID))
    ]}
  ]}.

test_is_friend() ->
  { "is_friend/2", [
    { "returns true when user A has user B as a friend", [
      ?_assert(is_friend(?ALICE_JID, ?BOB_JID)),
      ?_assert(is_friend(?ALICE_JID, ?CAROL_JID))
    ]},
    { "returns false when B is not a bi-directional subscriber of A", [
      ?_assertNot(is_friend(?CAROL_JID, ?TIM_JID))
    ]},
    { "returns false when B is in A's __blocked__ group", [
      ?_assertNot(is_friend(?KAREN_JID, ?TIM_JID))
    ]},
    { "returns false when B is not in A's roster", [
      ?_assertNot(is_friend(?CAROL_JID, ?ALICE_JID)),
      ?_assertNot(is_friend(?KAREN_JID, ?CAROL_JID))
    ]}
  ]}.

test_users_with_contact() ->
  { "users_with_contact/1", [
    { "returns a list of users who have the specified user as a contact", [
      ?_assertEqual(lists:sort([?CAROL_JID, ?KAREN_JID]),
                    lists:sort(users_with_contact(?TIM_JID))),
      ?_assertEqual([?ALICE_JID], users_with_contact(?BOB_JID))
    ]},
    { "returns an empty list for users who nobody has as a contact", [
      ?_assertEqual([], users_with_contact(
                          jid:make(
                            wocky_db:create_id(), ?LOCAL_CONTEXT, <<>>)))
    ]}
  ]}.
