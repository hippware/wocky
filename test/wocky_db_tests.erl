%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for ejabberd_auth_cassandra.erl
-module(wocky_db_tests).

-include_lib("eunit/include/eunit.hrl").


wocky_db_configure_from_env_test_() -> {
  setup,
  fun _Before() ->
    meck:new(wocky_db_seestar),
    meck:expect(wocky_db_seestar, clear, fun() -> ok end),
    meck:expect(wocky_db_seestar, configure,
                fun(env_host, env_params) -> env_configured;
                   (par_host, par_params) -> par_configured end),

    meck:new(application, [unstick]),
    meck:expect(application, get_env,
                fun(wocky, host) -> {ok, env_host};
                   (wocky, wocky_db) -> {ok, env_params} end)
  end,
  fun _After(_) ->
    meck:unload(wocky_db_seestar),
    meck:unload(application)
  end,
  [
    { "maybe_configure/0 should call backend with settings from environment", [
      ?_assertMatch(env_configured, wocky_db:maybe_configure()),
      ?_assert(meck:validate(wocky_db_seestar)),
      ?_assert(meck:validate(application))
    ]},
    { "configure/0 should call backend with settings from environment", [
      ?_assertMatch(env_configured, wocky_db:configure()),
      ?_assert(meck:validate(wocky_db_seestar)),
      ?_assert(meck:validate(application))
    ]},
    { "configure/2 should call backend with supplied params", [
      ?_assertMatch(par_configured, wocky_db:configure(par_host, par_params)),
      ?_assert(meck:validate(wocky_db_seestar)),
      ?_assert(meck:validate(application))
    ]},
    { "clear/0 should call clear on the backend", [
      ?_assertMatch(ok, wocky_db:clear()),
      ?_assert(meck:validate(wocky_db_seestar))
    ]}
  ]
}.

wocky_db_configure_no_env_test_() -> {
  setup,
  fun _Before() ->
    meck:new(wocky_db_seestar),
    meck:expect(wocky_db_seestar, configure, fun(_, _) -> throw(badcall) end),

    meck:new(application, [unstick]),
    meck:expect(application, get_env,
                fun(wocky, host) -> undefined;
                   (wocky, wocky_db) -> undefined end)
  end,
  fun _After(_) ->
    meck:unload(wocky_db_seestar),
    meck:unload(application)
  end,
  [
    { "maybe_configure/0 should return ok if no settings in environment", [
      ?_assertMatch(ok, wocky_db:maybe_configure()),
      ?_assert(meck:validate(wocky_db_seestar)),
      ?_assert(meck:validate(application))
    ]},
    { "configure/0 should visibly fail if no settings in environment", [
      ?_assertMatch({error, no_config}, wocky_db:configure()),
      ?_assert(meck:validate(wocky_db_seestar)),
      ?_assert(meck:validate(application))
    ]}
  ]
}.

-define(LONG_STRING, <<"Lorem ipsum dolor sit amet, consectetur cras amet.">>).

wocky_db_to_keyspace_test_() -> {
  "to_keyspace/1", [
    { "should replace non-alphanumeric characters with underscores", [
      ?_assertEqual(<<"abc123___">>, wocky_db:to_keyspace("abc123$!@"))
    ]},
    { "should truncate strings at 48 characters", [
      ?_assert(byte_size(?LONG_STRING) > 48),
      ?_assertEqual(48, byte_size(wocky_db:to_keyspace(?LONG_STRING)))
    ]}
]}.


%% This is a very simple test to verify that basic communication with
%% Cassandra works. It probably isn't worth testing this functionality more
%% thoroughly since at that point we would essentially be testing the
%% Cassandra driver.

wocky_db_api_smoke_test() ->
    ok = wocky_app:start(),

    Q1 = <<"INSERT INTO username_to_user (id, domain, username) VALUES (?, ?, ?)">>,
    Queries = [
      {Q1, [ossp_uuid:make(v1, binary), <<"localhost">>, <<"alice">>]},
      {Q1, [ossp_uuid:make(v1, binary), <<"localhost">>, <<"bob">>]},
      {Q1, [ossp_uuid:make(v1, binary), <<"localhost">>, <<"charlie">>]}
    ],
    {ok, _} = wocky_db:batch_query(shared, Queries, unlogged, quorum),

    Q2 = <<"SELECT username FROM username_to_user">>,
    {ok, Result} = wocky_db:query(shared, Q2, quorum),
    ?assertEqual(3, length(wocky_db:rows(Result))),

    Q3 = <<"TRUNCATE username_to_user">>,
    wocky_db:query_async(shared, Q3, quorum),
    ok.
