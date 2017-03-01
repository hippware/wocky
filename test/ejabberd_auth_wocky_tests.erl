%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for ejabberd_auth_wocky.erl
-module(ejabberd_auth_wocky_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").

-import(ejabberd_auth_wocky, [check_password/3, try_register/3, remove_user/2]).


ejabberd_auth_wocky_test_() -> {
  "ejabberd_auth_wocky",
  foreach, fun before_each/0, fun after_each/1, [
    test_check_password_with_token()
  ]
}.


before_each() ->
    ok = remove_user(?USER, ?SERVER),
    ok = try_register(?USER, ?SERVER, ?PASS),
    {ok, _} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, auth_token),
    ok.

after_each(_) ->
    ok.

test_check_password_with_token() ->
  { "check_password/3", [
    { "returns true when user exists and token matches", [
      ?_assert(check_password(?USER, ?SERVER, ?TOKEN))
    ]},
    { "returns false when user exists but token does not match", [
      ?_assertNot(check_password(?USER, ?SERVER,
                                 wocky_db_user:generate_token()))
    ]},
    { "returns false when user does not exist", [
      ?_assertNot(check_password(?BADUSER, ?SERVER, ?PASS))
    ]}
  ]}.
