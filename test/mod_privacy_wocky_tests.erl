%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_privacy_wocky.erl
-module(mod_privacy_wocky_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/mod_privacy.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").

-define(DEFAULT_LIST, <<"default">>).
-define(BACKEND, mod_privacy_riak).

-import(mod_privacy_wocky, [get_default_list/2, get_list_names/2,
                            get_privacy_list/3, forget_default_list/2,
                            set_default_list/3, remove_privacy_list/3,
                            replace_privacy_list/4, remove_user/2,
                            default_list_items/2
                           ]).

mod_privacy_wocky_test_() -> {
  "mod_privacy_wocky", inorder, {
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
  }
 }.

before_all() ->
    seed_data(),
    ok.

after_all(_) ->
    ok.

seed_data() ->
    ?BACKEND:replace_privacy_list(
       ?CAROL, ?LOCAL_CONTEXT, ?PRIVACY_LIST1,
       [#listitem{
           type = jid,
           value = jid:to_lower(jid:make(?KAREN, ?LOCAL_CONTEXT, <<>>)),
           action = false,
           order = 1,
           match_all = true},
        #listitem{
           type = jid,
           value = jid:to_lower(jid:make(?KAREN, ?LOCAL_CONTEXT, <<>>)),
           action = false,
           order = 2,
           match_iq = true}]),
    ?BACKEND:replace_privacy_list(
       ?CAROL, ?LOCAL_CONTEXT, ?PRIVACY_LIST2,
       [#listitem{
           type = subscription,
           value = both,
           action = false,
           order = 1,
           match_message = true}]),
    ?BACKEND:remove_user(?KAREN, ?LOCAL_CONTEXT).

test_get_default_list() ->
  DefaultListItems = default_list_items(?CAROL, ?LOCAL_CONTEXT),
  {
  "get_default_list", [
    { "gets a user's default list", [
      ?_assertMatch({ok, {?DEFAULT_LIST, DefaultListItems}},
                    get_default_list(?CAROL, ?LOCAL_CONTEXT))
    ]}
  ]}.

test_get_list_names() -> {
  "get_list_names", [
    { "gets a user's lists and default list", [
      ?_assertEqual({ok, {?DEFAULT_LIST,
                          [?DEFAULT_LIST, ?PRIVACY_LIST1, ?PRIVACY_LIST2]}},
                    get_list_names(?CAROL, ?LOCAL_CONTEXT))
    ]},
    { "gets only default if no other lists exist", [
      ?_assertEqual({ok, {?DEFAULT_LIST, [?DEFAULT_LIST]}},
                    get_list_names(?KAREN, ?LOCAL_CONTEXT))
    ]}
  ]}.

test_get_privacy_list() -> {
  "get_privacy_list", [
    { "gets the items in a list by name", [
      ?_assertMatch({ok, [#listitem{value = {?KAREN, ?LOCAL_CONTEXT, <<>>}},
                          #listitem{value = {?KAREN, ?LOCAL_CONTEXT, <<>>}}]},
                    get_privacy_list(?CAROL, ?LOCAL_CONTEXT, ?PRIVACY_LIST1))
    ]},
    { "gets the items in a list by name", [
      ?_assertMatch({ok, [#listitem{type = subscription, value = both}]},
                    get_privacy_list(?CAROL, ?LOCAL_CONTEXT, ?PRIVACY_LIST2))
    ]},
    { "gets an error if no such list exists", [
      ?_assertEqual({error, not_found},
                    get_privacy_list(?CAROL, ?LOCAL_CONTEXT, <<"not a list">>))
    ]},
    { "gets an error if the user's list of privacy lists is empty", [
      ?_assertEqual({error, not_found},
                    get_privacy_list(?KAREN, ?LOCAL_CONTEXT, ?PRIVACY_LIST1))
    ]},
    { "gets an error if there is no privacy table entry for the user", [
      ?_assertEqual({error, not_found},
                    get_privacy_list(?ALICE, ?LOCAL_CONTEXT, ?PRIVACY_LIST1))
    ]}
  ]}.

test_forget_default_list() -> {
  "forget_default_list", [
    { "Should always return an error", [
      ?_assertEqual({error, not_found},
                    forget_default_list(?CAROL, ?LOCAL_CONTEXT))
    ]}
  ]}.

test_set_default_list() -> {
  "set_default_list", [
    { "Should always return an error", [
      ?_assertEqual({error, not_found},
                    set_default_list(?CAROL, ?LOCAL_CONTEXT,
                                     ?PRIVACY_LIST1))
    ]}
  ]}.

test_remove_privacy_list() -> {
  "remove_privacy_list", [
    { "removes a list by name", [
      ?_assertEqual(ok, remove_privacy_list(?CAROL, ?LOCAL_CONTEXT,
                                            ?PRIVACY_LIST2)),
      ?_assertEqual({ok, {?DEFAULT_LIST, [?DEFAULT_LIST, ?PRIVACY_LIST1]}},
                    get_list_names(?CAROL, ?LOCAL_CONTEXT))
    ]},
    { "refuses to remove a default list", [
      ?_assertEqual({error, conflict},
                    remove_privacy_list(?KAREN, ?LOCAL_CONTEXT,
                                        ?DEFAULT_LIST)),
      ?_assertEqual({ok, {?DEFAULT_LIST, [?DEFAULT_LIST]}},
                    get_list_names(?KAREN, ?LOCAL_CONTEXT))
    ]}
  ]}.

test_replace_privacy_list() -> {
  "replace_privacy_list", [
    { "creates a new list if it doesn't exist", [
      ?_assertEqual(ok, replace_privacy_list(?KAREN, ?LOCAL_CONTEXT,
                                             <<"new list">>,
                                             [new_item()])),
      ?_assertEqual({ok, [new_item()]},
                    get_privacy_list(?KAREN, ?LOCAL_CONTEXT,
                                     <<"new list">>))
    ]},
    { "replaces an existing list", [
      ?_assertEqual(ok, replace_privacy_list(?CAROL, ?LOCAL_CONTEXT,
                                             ?PRIVACY_LIST1,
                                             [new_item()])),
      ?_assertEqual({ok, [new_item()]},
                    get_privacy_list(?CAROL, ?LOCAL_CONTEXT,
                                     ?PRIVACY_LIST1))
    ]}
  ]}.

test_remove_user() -> {
  "remove_user", [
    { "replaces an existing list", [
      ?_assertEqual(ok, remove_user(?CAROL, ?LOCAL_CONTEXT)),
      ?_assertEqual({error, not_found},
                    get_privacy_list(?CAROL, ?LOCAL_CONTEXT,
                                     ?PRIVACY_LIST1))
    ]},
    { "does not fail on repeated calls with non-existant users", [
      ?_assertEqual(ok, remove_user(?CAROL, ?LOCAL_CONTEXT)),
      ?_assertEqual(ok, remove_user(?CAROL, ?LOCAL_CONTEXT)),
      ?_assertEqual(ok, remove_user(?wocky_id:create(), ?LOCAL_CONTEXT))
    ]}
  ]}.

new_item() ->
    #listitem{type = subscription,
              value = both,
              action = deny,
              order = 1,
              match_all = true}.
