%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for ejabberd_auth_cassandra.erl
-module(ejabberd_auth_wocky_tests).

-include_lib("eunit/include/eunit.hrl").


ejabberd_auth_wocky_test_() -> {
  "ejabberd_auth_wocky",
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
      ?_assertMatch({file, _}, code:is_loaded(ejabberd_auth_wocky))
    ]}
  ]}.
