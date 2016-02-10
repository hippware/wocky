%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for wocky_db_user.erl
-module(wocky_db_user_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").

-import(wocky_db_user, [is_valid_id/1, does_user_exist/2, create_user/3,
                        create_user/4, remove_user/2, get_password/2,
                        set_password/3]).


wocky_db_user_test_() -> {
  "wocky_db_user",
  setup, fun before_all/0, fun after_all/1,
  [
    test_is_valid_id(),
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
    ok = wocky_db_seed:prepare_tables(shared, [handle_to_user]),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [user]),
    ok.

after_all(_) ->
    ok = wocky_app:stop(),
    ok.

before_each() ->
    {ok, _} = wocky_db_seed:seed_table(shared, handle_to_user),
    {ok, _} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, user),
    ok.

after_each(_) ->
    ok = wocky_db_seed:clear_tables(shared, [handle_to_user]),
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [user]),
    ok.

test_is_valid_id() ->
  { "is_valid_id", [
    { "returns true if the user ID is a valid UUID", [
      ?_assert(is_valid_id(?USER)),
      ?_assert(is_valid_id(ossp_uuid:make(v1, text))),
      ?_assert(is_valid_id(ossp_uuid:make(v1, binary))),
      ?_assert(is_valid_id(ossp_uuid:make(v4, text))),
      ?_assert(is_valid_id(ossp_uuid:make(v4, binary)))
    ]},
    { "returns false if the user ID is not a valid UUID", [
      ?_assertNot(is_valid_id(<<"alice">>))
    ]}
  ]}.

test_does_user_exist() ->
  { "does_user_exist", setup, fun before_each/0, fun after_each/1, [
    { "returns true if the user exists", [
      ?_assert(does_user_exist(?USER, ?SERVER))
    ]},
    { "returns false if the user does not exist", [
      ?_assertNot(does_user_exist(?BADUSER, ?SERVER)),
      ?_assertNot(does_user_exist(<<"alice">>, ?SERVER))
    ]}
  ]}.

test_create_user_without_id() ->
  { "create_user", setup, fun before_each/0, fun after_each/1, [
    { "creates a user if none exists", [
      ?_assertMatch({ok, _}, create_user(?SERVER, <<"nosuchuser">>, ?PASS))
    ]},
    { "fails if user already exists", [
      ?_assertMatch({error, exists}, create_user(?SERVER, ?HANDLE, ?PASS))
    ]}
  ]}.

test_create_user_with_id() ->
  { "create_user", setup, fun before_each/0, fun after_each/1, [
    { "creates a user if none exists", [
      ?_assertMatch(ok, create_user(?NEWUSER, ?SERVER,
                                    <<"nosuchuser">>, ?PASS)),
      ?_assert(does_user_exist(?NEWUSER, ?SERVER))
    ]},
    { "returns {error, exists} if user already exists", [
      ?_assertMatch({error, exists},
                    create_user(?USER, ?SERVER, ?HANDLE, ?PASS))
    ]},
    { "returns {error, invalid_id} if user ID is not a valid UUID", [
      ?_assertMatch({error, invalid_id},
                    create_user(<<"alice">>, ?SERVER, ?HANDLE, ?PASS))
    ]}
  ]}.

test_get_password() ->
  { "get_password", setup, fun before_each/0, fun after_each/1, [
    { "returns password if user exists", [
      ?_assertMatch(?SCRAM, get_password(?USER, ?SERVER))
    ]},
    { "returns {error, not_found} if user does not exist", [
      ?_assertMatch({error, not_found}, get_password(?BADUSER, ?SERVER))
    ]},
    { "returns {error, not_found} if user ID is not a valid UUID", [
      ?_assertMatch({error, not_found}, get_password(<<"alice">>, ?SERVER))
    ]}
  ]}.

test_set_password() ->
  { "set_password", setup, fun before_each/0, fun after_each/1, [
    { "sets password if user exists", [
      ?_assertMatch(ok, set_password(?USER, ?SERVER, <<"newpass">>)),
      ?_assertMatch(<<"newpass">>, get_password(?USER, ?SERVER))
    ]},
    { "returns {error, not_found} if user does not exist", [
      ?_assertMatch({error, not_found}, set_password(?BADUSER, ?SERVER, ?PASS))
    ]},
    { "returns {error, not_found} if user ID is not a valid UUID", [
      ?_assertMatch({error, not_found},
                    set_password(<<"alice">>, ?SERVER, ?PASS))
    ]}
  ]}.

test_remove_user() ->
  { "remove_user", setup, fun before_each/0, fun after_each/1, [
    { "removes user if user exists", [
      ?_assertMatch(ok, remove_user(?USER, ?SERVER)),
      ?_assertNot(does_user_exist(?USER, ?SERVER))
    ]},
    { "succeeds if user does not exist", [
      ?_assertMatch(ok, remove_user(?BADUSER, ?SERVER))
    ]},
    { "returns ok if user ID is not a valid UUID", [
      ?_assertMatch(ok, remove_user(<<"alice">>, ?SERVER))
    ]}
  ]}.
