%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_token.erl
-module(mod_wocky_token_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/ejabberd_config.hrl").
-include("wocky_db_seed.hrl").

-import(mod_wocky_token, [handle_iq/3]).

-define(ISO8601_REGEX,
        "\\d\\d\\d\\d-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d\\+\\d\\d:\\d\\d").


mod_wocky_token_test_() -> {
  "mod_wocky_token",
  setup, fun before_all/0, fun after_all/1,
  [
    test_non_local_domain(),
    test_iq_get(),
    test_iq_set()
  ]
}.

before_all() ->
    ets:new(config, [named_table, set, public, {keypos, 2}]),
    ets:insert(config, #config{key = hosts, value = [?SERVER]}),
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(shared, [user, handle_to_user]),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [auth_token]),
    {ok, _} = wocky_db_seed:seed_table(shared, handle_to_user),
    {ok, _} = wocky_db_seed:seed_table(shared, user),
    {ok, _} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, auth_token),
    ok.

after_all(_) ->
    ets:delete(config),
    ok = wocky_app:stop(),
    ok = wocky_db_seed:clear_tables(shared, [user, handle_to_user]),
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [auth_token]),
    ok.


jid() ->
    wocky_db_seed:jid(?USER, ?SERVER, ?RESOURCE).

-define(RESULT_IQ(Content),
        #iq{type = result,
            sub_el = [#xmlel{children = Content}]}).

test_non_local_domain() ->
  { "handle_iq returns an error IQ when JID domainpart is not local", [
      ?_assertMatch(#iq{type = error},
                    handle_iq(#jid{lserver = <<"remote">>}, #jid{}, #iq{}))
  ]}.

before() ->
    handle_iq(jid(), #jid{}, #iq{type = get}).

test_iq_get() ->
  { "handle_iq with type get", setup, fun before/0, fun (IQ) -> [
    { "returns a result IQ", [
      ?_assertMatch(#iq{type = result}, IQ)
    ]},
    { "returns an IQ with a token", [
      ?_assertMatch(?RESULT_IQ([#xmlcdata{content = <<"$T$", _/binary>>}]), IQ)
    ]},
    { "returns an IQ with an expiry", [
      ?_test(
       begin
         #iq{sub_el = [#xmlel{attrs = [{<<"xmlns">>, _},
                                      {<<"expiry">>, Expiry}]}]} = IQ,
         ?assertMatch({match, _}, re:run(Expiry, ?ISO8601_REGEX))
       end)
    ]}
  ] end}.

test_iq_set() ->
  { "handle_iq returns an empty result IQ when the IQ type is set", [
      ?_assertMatch(?RESULT_IQ([]), handle_iq(jid(), #jid{}, #iq{type = set}))
  ]}.
