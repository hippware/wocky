%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for wocky_db_user.erl
-module(wocky_db_user_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").

-import(wocky_db_user,
        [is_valid_id/1, does_user_exist/2, create_user_with_handle/3,
         create_user_with_handle/4, remove_user/2, get_password/2,
         set_password/3, generate_token/0, assign_token/3,
         release_token/3, get_tokens/2, check_token/4,
         maybe_set_handle/3,
         get_handle/2, get_by_handle/1,
         set_phone_number/3,
         get_phone_number/2, get_by_phone_number/1
        ]).


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
    test_remove_user(),
    test_generate_token(),
    test_assign_token(),
    test_release_token(),
    test_get_tokens(),
    test_check_token(),
    test_maybe_set_handle(),
    test_set_phone_number()
  ]
}.

before_all() ->
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(shared, [handle_to_user,
                                               phone_number_to_user]),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [user, auth_token]),
    ok.

after_all(_) ->
    ok = wocky_app:stop(),
    ok.

before_each() ->
    ok = wocky_db_seed:seed_tables(shared, [handle_to_user,
                                            phone_number_to_user]),
    {ok, _} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, user),
    ok.

after_each(_) ->
    ok = wocky_db_seed:clear_tables(shared, [handle_to_user,
                                             phone_number_to_user]),
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
  { "create_user_with_handle", setup, fun before_each/0, fun after_each/1, [
    { "creates a user if none exists", [
      ?_assertMatch({ok, _}, create_user_with_handle(?SERVER,
                                                     <<"nosuchuser">>, ?PASS))
    ]},
    { "fails if user already exists", [
      ?_assertMatch({error, exists}, create_user_with_handle(?SERVER,
                                                             ?HANDLE, ?PASS))
    ]}
  ]}.

test_create_user_with_id() ->
  { "create_user_with_handle", setup, fun before_each/0, fun after_each/1, [
    { "creates a user if none exists", [
      ?_assertMatch(ok, create_user_with_handle(?NEWUSER, ?SERVER,
                                    <<"nosuchuser">>, ?PASS)),
      ?_assert(does_user_exist(?NEWUSER, ?SERVER))
    ]},
    { "returns {error, exists} if user already exists", [
      ?_assertMatch({error, exists},
                    create_user_with_handle(?USER, ?SERVER, ?HANDLE, ?PASS))
    ]},
    { "returns {error, invalid_id} if user ID is not a valid UUID", [
      ?_assertMatch({error, invalid_id},
                    create_user_with_handle(<<"alice">>, ?SERVER,
                                            ?HANDLE, ?PASS))
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

test_generate_token() ->
  { "generate_token", [
    { "creates a token that starts with '$T$'", [
      ?_assertMatch(<<"$T$", _/binary>>, generate_token())
    ]}
  ]}.

token_setup() ->
    {ok, Token} = assign_token(?USER, ?SERVER, ?RESOURCE),
    Token.

token_cleanup(_) ->
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [auth_token]).

test_assign_token() ->
  { "assign_token", setup, fun before_each/0, fun after_each/1, [
    { "generates and stores a token for the user",
      setup, fun token_setup/0, fun token_cleanup/1, fun (Token) -> [
        ?_assertMatch(<<"$T$", _/binary>>, Token),
        ?_assertEqual([Token], get_tokens(?USER, ?SERVER))
    ] end},
    { "overwrites tokens when there are multiple requests", [
      ?_assertMatch({ok, _}, assign_token(?USER, ?SERVER, ?RESOURCE)),
      ?_assertMatch({ok, _}, assign_token(?USER, ?SERVER, ?RESOURCE)),
      ?_assertMatch({ok, _}, assign_token(?USER, ?SERVER, ?RESOURCE)),
      ?_assertEqual(1, length(get_tokens(?USER, ?SERVER)))
    ]},
    { "returns {error, not_found} if user does not exist", [
      ?_assertEqual({error, not_found},
                    assign_token(?BADUSER, ?SERVER, ?RESOURCE))
    ]},
    { "returns {error, not_found} if user ID is not a valid UUID", [
      ?_assertEqual({error, not_found},
                    assign_token(<<"alice">>, ?SERVER, ?RESOURCE))
    ]}
  ]}.

test_release_token() ->
  { "release_token", setup, fun before_each/0, fun after_each/1, [
    { "destroys a stored token",
      setup, fun token_setup/0, fun token_cleanup/1, [
        ?_assertEqual(ok, release_token(?USER, ?SERVER, ?RESOURCE)),
        ?_assertEqual([], get_tokens(?USER, ?SERVER))
    ]},
    { "returns ok if there is no token associated with the user or resource",
      setup, fun token_setup/0, fun token_cleanup/1, [
        ?_assertEqual(ok, release_token(?BOB, ?SERVER, ?RESOURCE)),
        ?_assertEqual(ok, release_token(?USER, ?SERVER, <<"nosuchresource">>))
    ]},
    { "returns ok if user does not exist", [
      ?_assertEqual(ok, release_token(?BADUSER, ?SERVER, ?RESOURCE))
    ]},
    { "returns ok if user ID is not a valid UUID", [
      ?_assertEqual(ok, release_token(<<"alice">>, ?SERVER, ?RESOURCE))
    ]}
  ]}.

test_get_tokens() ->
  { "get_tokens", setup, fun before_each/0, fun after_each/1, [
    { "returns all tokens for a user",
      setup, fun token_setup/0, fun token_cleanup/1, fun (Token) -> [
        ?_assertMatch({ok, _}, assign_token(?USER, ?SERVER, <<"test1">>)),
        ?_assertMatch({ok, _}, assign_token(?USER, ?SERVER, <<"test2">>)),
        ?_assertEqual(3, length(get_tokens(?USER, ?SERVER))),
        ?_assert(lists:member(Token, get_tokens(?USER, ?SERVER)))
    ] end},
    { "returns [] if there are no tokens associated with the user", [
        ?_assertEqual([], get_tokens(?BOB, ?SERVER))
    ]},
    { "returns [] if user does not exist", [
      ?_assertEqual([], get_tokens(?BADUSER, ?SERVER))
    ]},
    { "returns [] if user ID is not a valid UUID", [
      ?_assertEqual([], get_tokens(<<"alice">>, ?SERVER))
    ]}
  ]}.

test_check_token() ->
  { "check_token", setup, fun before_each/0, fun after_each/1, [
    { "accepts a valid token",
      setup, fun token_setup/0, fun token_cleanup/1, fun (Token) -> [
      ?_assert(check_token(?USER, ?SERVER, ?RESOURCE, Token))
    ] end},
    { "denies any other token",
      setup, fun token_setup/0, fun token_cleanup/1, [
      ?_assertNot(check_token(?USER, ?SERVER, ?RESOURCE, <<"badtoken">>))
    ]},
    { "denies tokens with a bad user or resource",
      setup, fun token_setup/0, fun token_cleanup/1, fun (Token) -> [
      ?_assertNot(check_token(wocky_db_user:create_id(),
                              ?SERVER, ?RESOURCE, Token)),
      ?_assertNot(check_token(?USER, ?SERVER, <<"badresource">>, Token))
    ] end}
   ]}.

test_maybe_set_handle() ->
  { "maybe_set_handle", foreach, fun before_each/0, fun after_each/1, [
    { "sets a handle on a user", [
        ?_assert(maybe_set_handle(?ALICE, ?SERVER, <<"shinynewhandle">>)),
        ?_assertEqual(<<"shinynewhandle">>, get_handle(?ALICE, ?SERVER)),
        ?_assertEqual({?ALICE, ?SERVER}, get_by_handle(<<"shinynewhandle">>)),
        ?_assertEqual(not_found, get_by_handle(?HANDLE))
    ]},
    { "won't set a handle that's already in use", [
        ?_assertNot(maybe_set_handle(?ALICE, ?SERVER, <<"carol">>)),
        ?_assertEqual(?HANDLE, get_handle(?ALICE, ?SERVER)),
        ?_assertEqual({?ALICE, ?SERVER}, get_by_handle(?HANDLE))
    ]},
    { "will fail and remain valid for the same value", [
        ?_assertNot(maybe_set_handle(?ALICE, ?SERVER, ?HANDLE)),
        ?_assertEqual(?HANDLE, get_handle(?ALICE, ?SERVER)),
        ?_assertEqual({?ALICE, ?SERVER}, get_by_handle(?HANDLE))
    ]}
  ]}.

test_set_phone_number() ->
  { "set_phone_number", foreach, fun before_each/0, fun after_each/1, [
    { "sets a phone number on a user", [
        ?_assert(set_phone_number(?ALICE, ?SERVER, <<"+614444">>)),
        ?_assertEqual(<<"+614444">>, get_phone_number(?ALICE, ?SERVER)),
        ?_assertEqual({?ALICE, ?SERVER}, get_by_phone_number(<<"+614444">>)),
        ?_assertEqual(not_found, get_by_phone_number(?PHONE_NUMBER))
    ]},
    { "will set a phone number that's already in use,"
      " and remove from the currently holding user", [
        ?_assert(set_phone_number(?ALICE, ?SERVER, <<"+4567">>)),
        ?_assertEqual(<<"+4567">>, get_phone_number(?ALICE, ?SERVER)),
        ?_assertEqual({?ALICE, ?SERVER}, get_by_phone_number(<<"+4567">>)),
        ?_assertEqual(null, get_phone_number(?CAROL, ?SERVER))
    ]},
    { "will succeed and remain valid for the same value", [
        ?_assert(set_phone_number(?ALICE, ?SERVER, ?PHONE_NUMBER)),
        ?_assertEqual(?PHONE_NUMBER, get_phone_number(?ALICE, ?SERVER)),
        ?_assertEqual({?ALICE, ?SERVER}, get_by_phone_number(?PHONE_NUMBER))
    ]}
  ]}.
