%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_geoloc.erl
-module(mod_wocky_geoloc_tests).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").
-include("wocky.hrl").

-import(mod_wocky_geoloc, [
                           handle_pep/2
                          ]).

mod_wocky_mam_test_() ->
 {
  "mod_wocky_geoloc",
  setup, fun before_all/0, fun after_all/1,
  [
   test_pep_hook()
  ]
 }.

before_all() ->
    ok = wocky_db:prepare_tables(?LOCAL_CONTEXT, [location]),
    ok.

after_all(_) ->
    ok.

test_pep_hook() ->
    Item = #xmlel{name = <<"geoloc">>,
                  attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
                  children = [cdata_item(<<"lat">>, <<"1.234">>),
                              cdata_item(<<"lon">>, <<"-5.678">>),
                              cdata_item(<<"accuracy">>, <<"1.2">>)]},
    EndItem = #xmlel{name = <<"geoloc">>,
                     attrs = [{<<"xmlns">>, ?NS_GEOLOC}]},
    NonGLEntry = Item#xmlel{name = <<"somethingelse">>},
    CountMatch = #{user => ?ALICE, server => ?LOCAL_CONTEXT},
    { "handle_pep", [
       { "should add an entry to the location table", [
          ?_assertEqual(Item, handle_pep(?ALICE_JID, Item)),
          ?_assertEqual(1, wocky_db:count(?LOCAL_CONTEXT, location, CountMatch))
       ]},
       { "should continue to add more items even if data is unchanged", [
          ?_assertEqual(Item, handle_pep(?ALICE_JID, Item)),
          ?_assertEqual(2, wocky_db:count(?LOCAL_CONTEXT, location, CountMatch))
       ]},
       { "should not add an incomplete entry", [
          ?_assertEqual(undefined,
                        handle_pep(?ALICE_JID,
                                   Item#xmlel{children =
                                              tl(Item#xmlel.children)})),
          ?_assertEqual(2, wocky_db:count(?LOCAL_CONTEXT, location, CountMatch))
       ]},
       { "should not add, but not drop a geoloc-end entry", [
          ?_assertEqual(EndItem, handle_pep(?ALICE_JID, EndItem)),
          ?_assertEqual(2, wocky_db:count(?LOCAL_CONTEXT, location, CountMatch))
       ]},
       { "should ignore a non-geoloc entry", [
          ?_assertEqual(NonGLEntry, handle_pep(?ALICE_JID, NonGLEntry)),
          ?_assertEqual(2, wocky_db:count(?LOCAL_CONTEXT, location, CountMatch))
       ]}
    ]}.

cdata_item(Name, Val) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content = Val}]}.
