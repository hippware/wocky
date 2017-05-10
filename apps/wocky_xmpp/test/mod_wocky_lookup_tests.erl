%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_lookup.erl
-module(mod_wocky_lookup_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").

-import(mod_wocky_lookup, [handle_phone_iq/3, handle_handle_iq/3]).


mod_wocky_lookup_phone_test_() -> {
  "mod_wocky_lookup",
  setup, fun before_all/0, fun after_all/1,
  [
    test_phone_iq_get_request(),
    test_phone_iq_get_results(),
    test_phone_iq_set()
  ]
}.

mod_wocky_lookup_handle_test_() -> {
  "mod_wocky_lookup",
  setup, fun before_all/0, fun after_all/1,
  [
    test_handle_iq_get_request(),
    test_handle_iq_get_results(),
    test_handle_iq_set()
  ]
}.

before_all() ->
    _ = ?wocky_repo:delete_all(?wocky_user),
    _User = ?wocky_factory:insert(user, #{id => ?ALICE,
                                          username => ?ALICE,
                                          server => ?SERVER,
                                          phone_number => ?PHONE_NUMBER,
                                          handle => <<"alice">>,
                                          first_name => <<"Alice">>,
                                          last_name => <<"Wonderland">>,
                                          avatar => ?AVATAR_ID}),
    ok.

after_all(_) ->
    ok.

-define(FROM, #jid{luser = ?ALICE, lserver = ?SERVER}).
-define(TO, #jid{lserver = ?SERVER}).

-define(RESULT_IQ(Content),
        #iq{type = result,
            sub_el = [#xmlel{children = Content}]}).

make_iq(Items) ->
    iq_get([item_el(I) || I <- Items]).

iq_get(Items) ->
    #iq{type = get,
        sub_el = #xmlel{name = <<"lookup">>,
                        children = Items}}.

item_el(I) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, iolist_to_binary(I)}]}.

test_phone_iq_get_request() ->
  { "handle_phone_iq with type get", [
    { "returns an empty result when the JID is not a UUID", [
      ?_assertMatch(?RESULT_IQ([]),
                    handle_phone_iq(#jid{luser = <<"foo">>,
                                         lserver = ?LOCAL_CONTEXT},
                                    ?TO, make_iq([])))
    ]},
    { "returns an empty result IQ if there are no item elements", [
      ?_assertMatch(?RESULT_IQ([]), handle_phone_iq(?FROM, ?TO, make_iq([])))
    ]},
    { "ignores any item elements without an id", [
      ?_assertMatch(?RESULT_IQ([]),
                    handle_phone_iq(?FROM, ?TO,
                                    iq_get([#xmlel{name = <<"item">>}])))
    ]},
    { "returns a result iq when the request is properly formatted", [
      ?_assertMatch(?RESULT_IQ([#xmlel{name = <<"item">>}]),
                    handle_phone_iq(?FROM, ?TO, make_iq([?PHONE_NUMBER])))
    ]}
  ]}.

setup_phone_request() ->
    Numbers = [<<"+1234">>, <<"4567">>, <<"+5555">>, <<"+6666">>, <<"+9999">>],
    ResultIQ = handle_phone_iq(?FROM, ?TO, make_iq(Numbers)),
    #iq{type = result,
        sub_el = [#xmlel{children = Els}]} = ResultIQ,
    Els.

after_each(_) ->
    ok.

-define(FIRST(X), element(3, hd(X))).
-define(LAST(X), element(3, hd(lists:reverse(X)))).

test_phone_iq_get_results() ->
  { "handle_phone_iq with type get",
    setup,
    fun setup_phone_request/0,
    fun after_each/1,
    fun (Els) -> [
      { "returns a result item for each item in the lookup list", [
        ?_assertEqual(5, length(Els))
      ]},
      { "returns item-not-found for an unrecognized phone number", [
        ?_assertEqual(<<"item-not-found">>,
                      proplists:get_value(<<"error">>, ?LAST(Els)))
      ]},
      { "returns the proper user information for a phone number", [
        ?_assertEqual(<<"+1234">>,
                      proplists:get_value(<<"id">>, ?FIRST(Els))),
        ?_assertEqual(<<?ALICE/binary, "@", ?LOCAL_CONTEXT/binary>>,
                      proplists:get_value(<<"jid">>, ?FIRST(Els))),
        ?_assertEqual(<<"alice">>,
                      proplists:get_value(<<"handle">>, ?FIRST(Els))),
        ?_assertMatch(<<"tros:", _/binary>>,
                      proplists:get_value(<<"avatar">>, ?FIRST(Els)))
      ]}
    ] end
  }.


test_phone_iq_set() ->
  ErrorIQ = #iq{type = error, sub_el = [?ERR_NOT_ALLOWED]},
  { "handle_phone_iq returns a error IQ when the IQ type is set", [
      ?_assertMatch(ErrorIQ, handle_phone_iq(?FROM, ?TO, #iq{type = set}))
  ]}.


test_handle_iq_get_request() ->
  { "handle_handle_iq with type get", [
    { "returns an empty result IQ if there are no item elements", [
      ?_assertMatch(?RESULT_IQ([]), handle_handle_iq(?FROM, ?TO, make_iq([])))
    ]},
    { "ignores any item elements without an id", [
      ?_assertMatch(?RESULT_IQ([]),
                    handle_handle_iq(?FROM, ?TO,
                                     iq_get([#xmlel{name = <<"item">>}])))
    ]},
    { "returns a result iq when the request is properly formatted", [
      ?_assertMatch(?RESULT_IQ([#xmlel{name = <<"item">>}]),
                    handle_handle_iq(?FROM, ?TO, make_iq([<<"alice">>])))
    ]}
  ]}.

setup_handle_request() ->
    Handles = [<<"alice">>, <<"carol">>, <<"bob">>, <<"karen">>, <<"duke">>],
    ResultIQ = handle_handle_iq(?FROM, ?TO, make_iq(Handles)),
    #iq{type = result,
        sub_el = [#xmlel{children = Els}]} = ResultIQ,
    Els.

test_handle_iq_get_results() ->
  { "handle_handle_iq with type get",
    setup,
    fun setup_handle_request/0,
    fun after_each/1,
    fun (Els) -> [
      { "returns a result item for each item in the lookup list", [
        ?_assertEqual(5, length(Els))
      ]},
      { "returns item-not-found for an unrecognized handle", [
        ?_assertEqual(<<"item-not-found">>,
                      proplists:get_value(<<"error">>, ?LAST(Els)))
      ]},
      { "returns the proper user information for a handle", [
        ?_assertEqual(<<"alice">>,
                      proplists:get_value(<<"id">>, ?FIRST(Els))),
        ?_assertEqual(<<?ALICE/binary, "@", ?LOCAL_CONTEXT/binary>>,
                      proplists:get_value(<<"jid">>, ?FIRST(Els)))
      ]}
    ] end
  }.

test_handle_iq_set() ->
  ErrorIQ = #iq{type = error, sub_el = [?ERR_NOT_ALLOWED]},
  { "handle_handle_iq returns a error IQ when the IQ type is set", [
      ?_assertMatch(ErrorIQ, handle_handle_iq(?FROM, ?TO, #iq{type = set}))
  ]}.
