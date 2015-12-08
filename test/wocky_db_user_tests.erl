%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for wocky_db_user.erl
-module(wocky_db_user_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DOMAIN, <<"localhost">>).
-define(USER, <<"bob">>).
-define(PASS, <<"password">>).


wocky_db_user_test_() -> {
  "wocky_db_user",
  setup, fun before_all/0, fun after_all/1,
  [
    test_create_user(),
    test_get_password(),
    test_set_password(),
    test_remove_user()
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
    {ok, _} = wocky_db:pquery(shared, <<"TRUNCATE username_to_user">>, [], quorum),
    {ok, _} = wocky_db:pquery(?DOMAIN, <<"TRUNCATE user">>, [], quorum),
    ok.

test_create_user() ->
  { "create_user", foreach, fun before_each/0, fun after_each/1, [
    { "creates a user if none exists", [
      ?_assertNot(wocky_db_user:does_user_exist(?DOMAIN, ?USER)),
      ?_assertMatch(ok, wocky_db_user:create_user(?DOMAIN, ?USER, ?PASS)),
      ?_assert(wocky_db_user:does_user_exist(?DOMAIN, ?USER))
    ]},
    { "fails if user already exists", [
      ?_assertMatch(ok, wocky_db_user:create_user(?DOMAIN, ?USER, ?PASS)),
      ?_assert(wocky_db_user:does_user_exist(?DOMAIN, ?USER)),
      ?_assertMatch({error, exists},
                    wocky_db_user:create_user(?DOMAIN, ?USER, ?PASS))
    ]}
  ]}.

test_get_password() ->
  { "get_password", foreach, fun before_each/0, fun after_each/1, [
    { "returns password if user exists", [
      ?_assertMatch(ok, wocky_db_user:create_user(?DOMAIN, ?USER, ?PASS)),
      ?_assertMatch(?PASS, wocky_db_user:get_password(?DOMAIN, ?USER))
    ]},
    { "returns {error, not_found} if user does not exist", [
      ?_assertNot(wocky_db_user:does_user_exist(?DOMAIN, ?USER)),
      ?_assertMatch({error, not_found},
                    wocky_db_user:get_password(?DOMAIN, ?USER))
    ]}
  ]}.

test_set_password() ->
  { "set_password", foreach, fun before_each/0, fun after_each/1, [
    { "sets password if user exists", [
      ?_assertMatch(ok, wocky_db_user:create_user(?DOMAIN, ?USER, ?PASS)),
      ?_assertMatch(ok, wocky_db_user:set_password(?DOMAIN, ?USER, <<"newpass">>)),
      ?_assertMatch(<<"newpass">>, wocky_db_user:get_password(?DOMAIN, ?USER))
    ]},
    { "returns {error, not_found} if user does not exist", [
      ?_assertNot(wocky_db_user:does_user_exist(?DOMAIN, ?USER)),
      ?_assertMatch({error, not_found},
                    wocky_db_user:set_password(?DOMAIN, ?USER, <<"newpass">>))
    ]}
  ]}.

test_remove_user() ->
  { "remove_user", foreach, fun before_each/0, fun after_each/1, [
    { "removes user if user exists", [
      ?_assertMatch(ok, wocky_db_user:create_user(?DOMAIN, ?USER, ?PASS)),
      ?_assert(wocky_db_user:does_user_exist(?DOMAIN, ?USER)),
      ?_assertMatch(ok, wocky_db_user:remove_user(?DOMAIN, ?USER)),
      ?_assertNot(wocky_db_user:does_user_exist(?DOMAIN, ?USER))
    ]},
    { "succeeds if user does not exist", [
      ?_assertNot(wocky_db_user:does_user_exist(?DOMAIN, ?USER)),
      ?_assertMatch(ok, wocky_db_user:remove_user(?DOMAIN, ?USER))
    ]}
  ]}.
