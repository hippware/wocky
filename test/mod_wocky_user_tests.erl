%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_user.erl
-module(mod_wocky_user_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").

mod_privacy_wocky_test_() -> {
  "mod_privacy_wocky",
  setup, fun before_all/0, fun after_all/1,
  [
  %  test_get_default_list(),
  ]
 }.

privacy_tables() -> [privacy, privacy_item].

before_all() ->
    {ok, _} = application:ensure_all_started(p1_stringprep),
    ok = wocky_app:start(),
    ok = wocky_db_seed:clear_user_tables(?LOCAL_CONTEXT),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, privacy_tables()),
    ok = wocky_db_seed:seed_tables(?LOCAL_CONTEXT, privacy_tables()),
    ok.

after_all(_) ->
    ok.
