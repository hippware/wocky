%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_roster.erl
-module(wocky_db_roster_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/mod_roster.hrl").

-import(wocky_db_roster, [get_roster/2,
                          get_roster_version/2,
                          get_roster_updates/3,
                          delete_roster/2,
                          get_roster_item/3,
                          update_roster_item/4,
                          delete_roster_item/3]).

-define(USER, <<"eaf84ab4-bbac-11e5-9912-ba0be0483c18">>).
-define(SERVER, <<"localhost">>).

-define(BADUSER, <<"97edd3c4-bbad-11e5-9912-ba0be0483c18">>).

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
    test_delete_roster_item()
  ]
}.

before_all() ->
    ok = wocky_app:start(),
    ok.

after_all(_) ->
    ok = wocky_app:stop(),
    ok.

make_jid() ->
    iolist_to_binary([wocky_db_user:create_id(), "@", ?SERVER]).

jid(Name, Items) ->
    element(2, lists:keyfind(Name, 1, Items)).

before_each() ->
    Items = [
       {<<"bob">>,    make_jid(), [],                             666},
       {<<"alicia">>, make_jid(), [<<"friends">>],                777},
       {<<"robert">>, make_jid(), [<<"friends">>, <<"starred">>], 888},
       {<<"karen">>,  make_jid(), [<<"starred">>],                999},
       {<<"alice">>,  make_jid(), [],                            1000}
    ],

    NewItems = {<<"tim">>, make_jid(), [], 1111},

    Values = [#{user => ?USER, server => ?SERVER, contact => C, nick => N,
                groups => G, version => V} || {N, C, G, V} <- Items],

    Q = "INSERT INTO roster (user, server, contact, nick, groups, version)"
        " VALUES (?, ?, ?, ?, ?, ?)",
    wocky_db:multi_query(?SERVER, Q, Values, quorum),
    [NewItems|Items].

after_each(_) ->
    {ok, void} = wocky_db:query(?SERVER, "TRUNCATE roster", quorum),
    ok.

test_get_roster() ->
  { "get_roster/2", foreach, fun before_each/0, fun after_each/1, [
    { "returns the roster for a known user", [
      ?_assertMatch([#roster{}|_], get_roster(?USER, ?SERVER)),
      ?_assertEqual(5, length(get_roster(?USER, ?SERVER)))
    ]},
    { "returns an empty list for an unknown user", [
      ?_assertEqual([], get_roster(?BADUSER, ?SERVER))
    ]}
  ]}.

test_get_roster_version() ->
  { "get_roster_version/2", foreach, fun before_each/0, fun after_each/1, [
    { "returns the roster version for a known user", [
      ?_assertEqual(<<"1000">>, get_roster_version(?USER, ?SERVER))
    ]},
    { "returns a not found error for an unknown user", [
      ?_assertEqual({error, not_found}, get_roster_version(?BADUSER, ?SERVER))
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
      ?_assertEqual(5, length(get_roster_updates(?USER, ?SERVER, <<"0">>)))
    ]},
    { "returns a partial roster when the versions do not match", [
      ?_assertMatch([#roster{}], get_roster_updates(?USER, ?SERVER, <<"999">>)),
      ?_assertEqual(3, length(get_roster_updates(?USER, ?SERVER, <<"777">>)))
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
  { "get_roster_item/3", setup, fun before_each/0, fun after_each/1,
    fun (Items) ->
    [
      { "returns the roster item for a known user and contact", [
        ?_assertMatch(#roster{name = <<"bob">>},
                      get_roster_item(?USER, ?SERVER, jid(<<"bob">>, Items)))
      ]},
      { "returns an empty roster item for a known user and unknown contact", [
        ?_assertMatch(#roster{name = <<>>},
                      get_roster_item(?USER, ?SERVER, make_jid()))
      ]},
      { "returns an empty roster item for an unknown user", [
        ?_assertMatch(#roster{name = <<>>},
                      get_roster_item(?BADUSER, ?SERVER, jid(<<"bob">>, Items)))
      ]}
    ]
    end
  }.

test_update_roster_item() ->
  { "update_roster_item/4", setup, fun before_each/0, fun after_each/1,
    fun (Items) ->
    [
      { "inserts a new roster item if one does not exist", [
        ?_assertEqual(ok, update_roster_item(?USER, ?SERVER,
                                             jid(<<"tim">>, Items),
                                             #roster{name = <<"tim">>})),
        ?_assertMatch(#roster{name = <<"tim">>},
                      get_roster_item(?USER, ?SERVER, jid(<<"tim">>, Items)))
      ]},
      { "updates an existing roster item", [
        ?_assertEqual(ok, update_roster_item(?USER, ?SERVER,
                                             jid(<<"bob">>, Items),
                                             #roster{name = <<"dan">>})),
        ?_assertMatch(#roster{name = <<"dan">>},
                      get_roster_item(?USER, ?SERVER, jid(<<"bob">>, Items)))
      ]}
    ]
    end
  }.

test_delete_roster_item() ->
  { "delete_roster_item/3", setup, fun before_each/0, fun after_each/1,
    fun (Items) ->
    [
      { "deletes an existing roster item", [
        ?_assertEqual(ok, delete_roster_item(?USER, ?SERVER,
                                             jid(<<"bob">>, Items))),
        ?_assertMatch(#roster{name = <<>>},
                      get_roster_item(?USER, ?SERVER, jid(<<"bob">>, Items)))
      ]},
      { "returns ok if the roster item doesn't exist", [
        ?_assertEqual(ok, delete_roster_item(?USER, ?SERVER, make_jid()))
      ]}
    ]
    end
  }.
