%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for ejabberd_auth_cassandra.erl
-module(wocky_db_tests).

-include_lib("eunit/include/eunit.hrl").


wocky_db_start_backend_test_() -> {
  "wocky_db:start_backend",
  foreach,
  fun _Before() ->
    code:purge(cassandra_backend),
    code:delete(cassandra_backend),
    ok = wocky_db:start_backend(seestar)
  end,
  fun _After(_) ->
    ok
  end,
  [
    { "should generate and load cassandra_backend", [
      ?_assertMatch({file, _}, code:is_loaded(cassandra_backend))
    ]},
    { "should make cassandra_backend return seestar", [
      ?_assertMatch(wocky_db_seestar, cassandra_backend:backend())
    ]}
  ]
}.

wocky_db_configure_test_() -> {
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
                   (wocky, wocky_db_seestar) -> {ok, env_params} end)
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
                   (wocky, wocky_db_seestar) -> undefined end)
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

wocky_db_test_() -> {
  "wocky_db",
  setup, fun before_all/0, fun after_all/1,
  [
    test_module_is_loaded()
  ]
}.

before_all() ->
    ok = wocky_app:start(),
    ok.

after_all(_) ->
    ok = wocky_app:stop(),
    ok.

before_each() ->
    ok.

after_each(_) ->
    ok.

%% Simple placeholder test. Delete and replace with something more meaningful.
test_module_is_loaded() ->
  { "module", foreach, fun before_each/0, fun after_each/1, [
    { "is loaded", [
      ?_assertMatch({file, _}, code:is_loaded(wocky_db))
    ]}
  ]}.
