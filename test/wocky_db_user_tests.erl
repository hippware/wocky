%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for wocky_db_user.erl
-module(wocky_db_user_tests).

-include_lib("eunit/include/eunit.hrl").

-define(USER,    <<"043e8c96-ba30-11e5-9912-ba0be0483c18">>).
-define(DOMAIN,  <<"localhost">>).
-define(NAME,    <<"bob">>).
-define(PASS,    <<"password">>).

-define(BADUSER, <<"d51f92c8-ba40-11e5-9912-ba0be0483c18">>).
-define(NEWUSER, <<"9d7acab4-ba30-11e5-9912-ba0be0483c18">>).


wocky_db_user_test_() -> {
  "wocky_db_user",
  setup, fun before_all/0, fun after_all/1,
  [
    test_does_user_exist(),
    test_create_user_without_id(),
    test_create_user_with_id(),
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
    Query1 = "INSERT INTO username_to_user (id, domain, username)" ++
             " VALUES (?, ?, ?)",
    Values1 = [{id, ?USER}, {domain, ?DOMAIN}, {username, ?NAME}],
    {ok, _} = wocky_db:query(shared, Query1, Values1, quorum),

    Query2 = "INSERT INTO user (id, domain, username, password)" ++
             " VALUES (?, ?, ?, ?)",
    Values2 = [{id, ?USER}, {domain, ?DOMAIN},
               {username, ?NAME}, {password, ?PASS}],
    {ok, _} = wocky_db:query(?DOMAIN, Query2, Values2, quorum),
    ok.

after_each(_) ->
    {ok, _} = wocky_db:query(shared, <<"TRUNCATE username_to_user">>, quorum),
    {ok, _} = wocky_db:query(?DOMAIN, <<"TRUNCATE user">>, quorum),
    ok.

test_does_user_exist() ->
  { "does_user_exist", setup, fun before_each/0, fun after_each/1, [
    { "returns true if the user exists", [
      ?_assert(wocky_db_user:does_user_exist(?USER, ?DOMAIN))
    ]},
    { "returns false if the user does not exist", [
      ?_assertNot(wocky_db_user:does_user_exist(?BADUSER, ?DOMAIN))
    ]}
  ]}.

test_create_user_without_id() ->
  { "create_user", setup, fun before_each/0, fun after_each/1, [
    { "creates a user if none exists", [
      ?_assertMatch({ok, _}, wocky_db_user:create_user(
                               ?DOMAIN, <<"alice">>, ?PASS))
    ]},
    { "fails if user already exists", [
      ?_assertMatch({error, exists},
                    wocky_db_user:create_user(?DOMAIN, ?NAME, ?PASS))
    ]}
  ]}.

test_create_user_with_id() ->
  { "create_user", setup, fun before_each/0, fun after_each/1, [
    { "creates a user if none exists", [
      ?_assertMatch(ok, wocky_db_user:create_user(?NEWUSER, ?DOMAIN,
                                                  <<"alice">>, ?PASS)),
      ?_assert(wocky_db_user:does_user_exist(?NEWUSER, ?DOMAIN))
    ]},
    { "fails if user already exists", [
      ?_assertMatch({error, exists},
                    wocky_db_user:create_user(?USER, ?DOMAIN, ?NAME, ?PASS))
    ]}
  ]}.

test_get_password() ->
  { "get_password", setup, fun before_each/0, fun after_each/1, [
    { "returns password if user exists", [
      ?_assertMatch(?PASS, wocky_db_user:get_password(?USER, ?DOMAIN))
    ]},
    { "returns {error, not_found} if user does not exist", [
      ?_assertMatch({error, not_found},
                    wocky_db_user:get_password(?BADUSER, ?DOMAIN))
    ]}
  ]}.

test_set_password() ->
  { "set_password", setup, fun before_each/0, fun after_each/1, [
    { "sets password if user exists", [
      ?_assertMatch(ok, wocky_db_user:set_password(?USER, ?DOMAIN,
                                                   <<"newpass">>)),

      ?_assertMatch(<<"newpass">>, wocky_db_user:get_password(?USER, ?DOMAIN))
    ]},
    { "returns {error, not_found} if user does not exist", [
      ?_assertMatch({error, not_found},
                    wocky_db_user:set_password(?BADUSER, ?DOMAIN, ?PASS))
    ]}
  ]}.

test_remove_user() ->
  { "remove_user", setup, fun before_each/0, fun after_each/1, [
    { "removes user if user exists", [
      ?_assertMatch(ok, wocky_db_user:remove_user(?USER, ?DOMAIN)),
      ?_assertNot(wocky_db_user:does_user_exist(?USER, ?DOMAIN))
    ]},
    { "succeeds if user does not exist", [
      ?_assertMatch(ok, wocky_db_user:remove_user(?BADUSER, ?DOMAIN))
    ]}
  ]}.
