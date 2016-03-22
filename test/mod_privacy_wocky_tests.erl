%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_privacy_wocky.erl
-module(mod_privacy_wocky_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/mod_privacy.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").

-import(mod_privacy_wocky, [get_default_list/2, get_list_names/2,
                            get_privacy_list/3, forget_default_list/2,
                            set_default_list/3, remove_privacy_list/3,
                            replace_privacy_list/4, remove_user/2
                           ]).

mod_privacy_wocky_test_() -> {
  "mod_privacy_wocky",
  setup, fun before_all/0, fun after_all/1,
  [
    test_get_default_list(),
    test_get_list_names(),
    test_get_privacy_list(),
    test_forget_default_list(),
    test_set_default_list(),
    test_remove_privacy_list(),
    test_replace_privacy_list(),
    test_remove_user()
  ]
 }.

privacy_tables() -> [privacy, privacy_item].

before_all() ->
    {ok, _} = application:ensure_all_started(p1_stringprep),
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, privacy_tables()),
    ok = wocky_db_seed:seed_tables(?LOCAL_CONTEXT, privacy_tables()),
    ok.

after_all(_) ->
    ok.

test_get_default_list() -> {
  "get_default_list", [
    { "gets a user's default list", [
      ?_assertMatch({ok, {?PRIVACY_LIST1,
                          [#listitem{value = {?BOB, ?LOCAL_CONTEXT, <<>>}},
                           #listitem{value = {?BOB, ?LOCAL_CONTEXT, <<>>}}]}},
                    get_default_list(?ALICE, ?LOCAL_CONTEXT))
    ]},
    { "gets an error if no default list is set", [
      ?_assertEqual({error, not_found},
                    get_default_list(?BOB, ?LOCAL_CONTEXT))
    ]},
    { "gets an error if there is no privacy table entry for the user", [
      ?_assertEqual({error, not_found},
                    get_default_list(?CAROL, ?LOCAL_CONTEXT))
    ]}
  ]}.

test_get_list_names() -> {
  "get_list_names", [
    { "gets a user's lists and default list", [
      ?_assertEqual({ok, {?PRIVACY_LIST1,
                          [?PRIVACY_LIST1, ?PRIVACY_LIST2]}},
                    get_list_names(?ALICE, ?LOCAL_CONTEXT))
    ]},
    { "gets nothing if no list exist", [
      ?_assertEqual({ok, {<<"">>, []}},
                    get_list_names(?BOB, ?LOCAL_CONTEXT))
    ]},
    { "gets an error if there is no privacy table entry for the user", [
      ?_assertEqual({error, not_found},
                    get_list_names(?CAROL, ?LOCAL_CONTEXT))
    ]}
  ]}.

test_get_privacy_list() -> {
  "get_privacy_list", [
    { "gets the items in a list by name", [
      ?_assertMatch({ok, [#listitem{value = {?BOB, ?LOCAL_CONTEXT, <<>>}},
                          #listitem{value = {?BOB, ?LOCAL_CONTEXT, <<>>}}]},
                    get_privacy_list(?ALICE, ?LOCAL_CONTEXT, ?PRIVACY_LIST1))
    ]},
    { "gets the items in a list by name", [
      ?_assertMatch({ok, [#listitem{type = subscription, value = both}]},
                    get_privacy_list(?ALICE, ?LOCAL_CONTEXT, ?PRIVACY_LIST2))
    ]},
    { "gets an error if no such list exists", [
      ?_assertEqual({error, not_found},
                    get_privacy_list(?ALICE, ?LOCAL_CONTEXT, <<"not a list">>))
    ]},
    { "gets an error if the user's list of privacy lists is empty", [
      ?_assertEqual({error, not_found},
                    get_privacy_list(?BOB, ?LOCAL_CONTEXT, ?PRIVACY_LIST1))
    ]},
    { "gets an error if there is no privacy table entry for the user", [
      ?_assertEqual({error, not_found},
                    get_privacy_list(?CAROL, ?LOCAL_CONTEXT, ?PRIVACY_LIST1))
    ]}
  ]}.

test_forget_default_list() -> {
  "forget_default_list", [
    { "Clear's a user's default list", [
      ?_assertEqual(ok, forget_default_list(?ALICE, ?LOCAL_CONTEXT)),
      ?_assertEqual({error, not_found},
                    get_default_list(?ALICE, ?LOCAL_CONTEXT))
    ]},
    { "Does not fail when a user doesn't have a default list", [
      ?_assertEqual(ok, forget_default_list(?BOB, ?LOCAL_CONTEXT)),
      ?_assertEqual(ok, forget_default_list(?CAROL, ?LOCAL_CONTEXT))
    ]}
  ]}.

test_set_default_list() -> {
  "set_default_list", [
    { "sets a default on a user with no default list", [
      ?_assertEqual({error, not_found},
                    get_default_list(?ALICE, ?LOCAL_CONTEXT)),
      ?_assertEqual(ok, set_default_list(?ALICE, ?LOCAL_CONTEXT,
                                         ?PRIVACY_LIST1)),
      ?_assertMatch({ok, {?PRIVACY_LIST1,
                          [#listitem{value = {?BOB, ?LOCAL_CONTEXT, <<>>}},
                           #listitem{value = {?BOB, ?LOCAL_CONTEXT, <<>>}}]}},
                    get_default_list(?ALICE, ?LOCAL_CONTEXT))
    ]},
    { "fails for non-existant lists", [
      ?_assertEqual({error, not_found},
                    set_default_list(?BOB, ?LOCAL_CONTEXT, ?PRIVACY_LIST1)),
      ?_assertEqual({error, not_found},
                     set_default_list(?CAROL, ?LOCAL_CONTEXT, ?PRIVACY_LIST1))
    ]}
  ]}.

test_remove_privacy_list() -> {
  "remove_privacy_list", [
    { "removes a list by name", [
      ?_assertEqual(ok, remove_privacy_list(?ALICE, ?LOCAL_CONTEXT,
                                            ?PRIVACY_LIST2)),
      ?_assertEqual({ok, {?PRIVACY_LIST1, [?PRIVACY_LIST1]}},
                    get_list_names(?ALICE, ?LOCAL_CONTEXT))
    ]},
    { "refuses to remove a default list", [
      ?_assertEqual({error, conflict},
                    remove_privacy_list(?ALICE, ?LOCAL_CONTEXT,
                                        ?PRIVACY_LIST1)),
      ?_assertEqual({ok, {?PRIVACY_LIST1, [?PRIVACY_LIST1]}},
                    get_list_names(?ALICE, ?LOCAL_CONTEXT))
    ]},
    { "succeeds even if the list alredy doesn't exist for that user", [
      ?_assertEqual(ok,
                    remove_privacy_list(?ALICE, ?LOCAL_CONTEXT,
                                        ?PRIVACY_LIST2)),
      ?_assertEqual(ok,
                    remove_privacy_list(?BOB, ?LOCAL_CONTEXT,
                                        ?PRIVACY_LIST2)),
      ?_assertEqual(ok,
                    remove_privacy_list(?CAROL, ?LOCAL_CONTEXT,
                                        ?PRIVACY_LIST2))
    ]}
  ]}.

test_replace_privacy_list() -> {
  "replace_privacy_list", [
    { "creates a new list if it doesn't exist", [
      ?_assertEqual(ok, replace_privacy_list(?BOB, ?LOCAL_CONTEXT,
                                             <<"new list">>,
                                             [new_item()])),
      ?_assertEqual({ok, [new_item()]},
                    get_privacy_list(?BOB, ?LOCAL_CONTEXT,
                                     <<"new list">>))
    ]},
    { "replaces an existing list", [
      ?_assertEqual(ok, replace_privacy_list(?ALICE, ?LOCAL_CONTEXT,
                                             ?PRIVACY_LIST1,
                                             [new_item()])),
      ?_assertEqual({ok, [new_item()]},
                    get_privacy_list(?ALICE, ?LOCAL_CONTEXT,
                                     ?PRIVACY_LIST1))
    ]}
  ]}.

test_remove_user() -> {
  "remove_user", [
    { "replaces an existing list", [
      ?_assertEqual(ok, remove_user(?ALICE, ?LOCAL_CONTEXT)),
      ?_assertEqual({error, not_found},
                    get_privacy_list(?ALICE, ?LOCAL_CONTEXT,
                                     ?PRIVACY_LIST1)),
      ?_assertEqual({error, not_found},
                    get_default_list(?ALICE, ?LOCAL_CONTEXT))
    ]},
    { "does not fail on repeated calls with non-existant users", [
      ?_assertEqual(ok, remove_user(?CAROL, ?LOCAL_CONTEXT)),
      ?_assertEqual(ok, remove_user(?CAROL, ?LOCAL_CONTEXT)),
      ?_assertEqual(ok, remove_user(wocky_db_user:create_id(), ?LOCAL_CONTEXT))
    ]}
  ]}.

new_item() ->
    #listitem{type = subscription,
              value = both,
              action = deny,
              order = 1,
              match_all = true}.
