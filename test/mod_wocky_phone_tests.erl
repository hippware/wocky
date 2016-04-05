%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_phone.erl
-module(mod_wocky_phone_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").

-import(mod_wocky_phone, [handle_iq/3]).


mod_wocky_phone_test_() -> {
  "mod_wocky_phone",
  setup, fun before_all/0, fun after_all/1,
  [
    test_iq_get_request(),
    test_iq_get_results(),
    test_iq_get_limits(),
    test_iq_set()
  ]
}.

before_all() ->
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(shared, [user, phone_number_to_user]),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [phone_lookup_count]),
    {ok, _} = wocky_db_seed:seed_table(shared, phone_number_to_user),
    {ok, _} = wocky_db_seed:seed_table(shared, user),
    ok.

after_all(_) ->
    ok = wocky_db_seed:clear_tables(shared, [user, phone_number_to_user]),
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [phone_lookup_count]),
    ok = wocky_app:stop().

-define(FROM, #jid{luser = ?ALICE, lserver = ?SERVER}).
-define(TO, #jid{lserver = ?SERVER}).

-define(RESULT_IQ(Content),
        #iq{type = result,
            sub_el = [#xmlel{children = Content}]}).

make_iq(Numbers) ->
    iq_get([item_el(N) || N <- Numbers]).

iq_get(Items) ->
    #iq{type = get,
        sub_el = #xmlel{name = <<"lookup">>,
                        children = Items}}.

item_el(N) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, iolist_to_binary(N)}]}.

test_iq_get_request() ->
  ErrorIQ = #iq{type = error, sub_el = [?ERR_JID_MALFORMED]},
  { "handle_iq with type get", [
    { "returns an error IQ when the JID is not a UUID", [
      ?_assertMatch(ErrorIQ, handle_iq(#jid{luser = "foo"}, ?TO, make_iq([])))
    ]},
    { "returns an empty result IQ if there are no item elements", [
      ?_assertMatch(?RESULT_IQ([]), handle_iq(?FROM, ?TO, make_iq([])))
    ]},
    { "ignores any item elements without an id", [
      ?_assertMatch(?RESULT_IQ([]),
                    handle_iq(?FROM, ?TO, iq_get([#xmlel{name = <<"item">>}])))
    ]},
    { "returns a result iq when the request is properly formatted", [
      ?_assertMatch(?RESULT_IQ([#xmlel{name = <<"item">>}]),
                    handle_iq(?FROM, ?TO, make_iq([?PHONE_NUMBER])))
    ]}
  ]}.

setup_request() ->
    Numbers = [<<"+1234">>, <<"4567">>, <<"+5555">>, <<"+6666">>, <<"+9999">>],
    ResultIQ = handle_iq(?FROM, ?TO, make_iq(Numbers)),
    #iq{type = result,
        sub_el = [#xmlel{children = Els}]} = ResultIQ,
    Els.

after_each(_) ->
    ok.

-define(FIRST(X), element(3, hd(X))).
-define(LAST(X), element(3, hd(lists:reverse(X)))).

test_iq_get_results() ->
  { "handle_iq with type get", setup, fun setup_request/0, fun after_each/1,
    fun (Els) -> [
      { "returns a result item for each item in the lookup list", [
        ?_assertEqual(5, length(Els))
      ]},
      { "returns item-not-found for an unrecognized phone number", [
        ?_assertEqual(<<"item-not-found">>,
                      proplists:get_value(<<"error">>, ?LAST(Els)))
      ]},
      { "returns the proper user information for a phone number", [
        ?_assertMatch(<<"+1234">>,
                      proplists:get_value(<<"id">>, ?FIRST(Els))),
        ?_assertMatch(<<"043e8c96-ba30-11e5-9912-ba0be0483c18@localhost">>,
                      proplists:get_value(<<"jid">>, ?FIRST(Els))),
        ?_assertMatch(<<"alice">>,
                      proplists:get_value(<<"handle">>, ?FIRST(Els)))
      ]}
    ] end
  }.


setup_limits() ->
    ok = mod_wocky_phone:save_reductions(?SERVER, ?ALICE, 3),
    setup_request().

test_iq_get_limits() ->
  { "handle_iq with type get", setup, fun setup_limits/0, fun after_each/1,
    fun (Els) -> [
      { "saves reductions to the database", [
        ?_assertEqual(0, mod_wocky_phone:lookup_reductions(?SERVER, ?ALICE))
      ]},
      { "returns not-acceptable once the limit is reached", [
        ?_assertEqual(<<"not-acceptable">>,
                      proplists:get_value(<<"error">>, ?FIRST(Els)))
      ]}
    ] end
  }.

test_iq_set() ->
  ErrorIQ = #iq{type = error, sub_el = [?ERR_NOT_ALLOWED]},
  { "handle_iq returns a error IQ when the IQ type is set", [
      ?_assertMatch(ErrorIQ, handle_iq(?FROM, ?TO, #iq{type = set}))
  ]}.
