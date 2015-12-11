%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_roster_cassandra.erl
-module(mod_roster_wocky_tests).

-include_lib("eunit/include/eunit.hrl").


mod_roster_wocky_test_() -> {
  "mod_roster_wocky",
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
      ?_assertMatch({file, _}, code:is_loaded(mod_roster_wocky))
    ]}
  ]}.
