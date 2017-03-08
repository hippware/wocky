%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for wocky_db_home_stream.erl
-module(wocky_db_home_stream_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").

-import(wocky_db_home_stream,
        [
         publish/5,
         delete/3,
         get/2,
         get/3,
         get_catchup/3,
         current_version/2
        ]).

wocky_db_home_stream_test_() -> {
  "wocky_db_home_stream",
  setup, fun before_all/0, fun after_all/1,
  [ {inorder, [
      {inparallel, [
        test_get(),
        test_get_catchup(),
        test_current_version()
      ]},
      {inorder, [
        test_publish(),
        test_delete()
      ]}
    ]}
  ]}.

local_tables() -> [
                   home_stream
                  ].

before_all() ->
    ok = wocky_db_seed:seed_tables(?LOCAL_CONTEXT, local_tables()).

after_all(_) ->
    ok.

test_get() ->
    { "get", inparallel, [
      { "gets the list of home stream items in chronological order", [
        ?_assertMatch([#{id := ?BOT},
                       #{id := ?ITEM},
                       #{id := ?ITEM2}],
                      get(?ALICE, ?LOCAL_CONTEXT))
      ]},
      { "gets a single item by id", [
        ?_assertMatch(#{id := ?BOT},
                       get(?ALICE, ?LOCAL_CONTEXT, ?BOT))
      ]},
      { "gets an empty list for non-existent users", [
        ?_assertEqual([], get(?wocky_id:new(), ?LOCAL_CONTEXT))
      ]},
      { "gets not_found for non-existent items", [
        ?_assertEqual(not_found, get(?ALICE, ?LOCAL_CONTEXT, <<"non-item">>))
      ]},
      { "gets all items in a very large set", [
        ?_assertEqual(250, length(get(?BOB, ?LOCAL_CONTEXT)))
      ]}
    ]}.

test_publish() ->
    ID = ?wocky_id:new(),
    AliceJID = ?ALICE_JID,
    BotJID = ?BOT_JID,
    Stanza = #xmlel{name = <<"item">>},
    Stanza2 = #xmlel{name = <<"new">>},
    { "publish", [
      { "adds a new item to the end of the user's home stream", [
        ?_assertMatch(#{id := ID,
                        from := AliceJID,
                        stanza := [Stanza],
                        deleted := false
                       },
                      publish(?ALICE, ?LOCAL_CONTEXT, ID,
                              Stanza, ?ALICE_JID)),
        ?_assertMatch(#{id := ID},
                      lists:last(get(?ALICE, ?LOCAL_CONTEXT)))
      ]},
      { "replaces an existing item and moves it to the end of the stream", [
        ?_assertMatch(#{id := ?BOT,
                        from := BotJID,
                        stanza := [Stanza2],
                        deleted := false
                       },
                      publish(?ALICE, ?LOCAL_CONTEXT, ?BOT,
                              Stanza2, ?BOT_JID)),
        ?_assertMatch(#{id := ?BOT, stanza := [Stanza2]},
                      lists:last(get(?ALICE, ?LOCAL_CONTEXT))),
        ?_assertEqual(4, length(get(?ALICE, ?LOCAL_CONTEXT)))
      ]}
    ]}.

test_delete() ->
    { "delete", [
      { "deletes an item with the specified ID by writing a tombstone entry", [
        ?_assertMatch(#{id := ?BOT,
                        deleted := true},
                      delete(?ALICE, ?LOCAL_CONTEXT, ?BOT)),
        ?_assertMatch(#{id := ?BOT,
                        deleted := true},
                      lists:last(get(?ALICE, ?LOCAL_CONTEXT))),
        ?_assertEqual(4, length(get(?ALICE, ?LOCAL_CONTEXT)))
      ]}
    ]}.

test_get_catchup() ->
    { "get_catchup", [
      { "gets items added or changed since the specified version", [
        ?_assertMatch(
           [#{id := ?ITEM}, #{id := ?ITEM2}],
           get_catchup(?ALICE, ?LOCAL_CONTEXT, ?HS_V_1))
      ]}
    ]}.

test_current_version() ->
    { "current_version", [
      { "gets the newest version timestamp", [
        ?_assertEqual(?HS_V_3, current_version(?ALICE, ?LOCAL_CONTEXT)),
        ?_assertEqual(not_found, current_version(?CAROL, ?LOCAL_CONTEXT))
      ]}
    ]}.
