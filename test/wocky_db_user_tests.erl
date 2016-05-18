%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for wocky_db_user.erl
-module(wocky_db_user_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").

-import(wocky_db_user,
        [does_user_exist/2, create_user/1, create_user/4,
         update_user/1, remove_user/2, get_password/2,
         set_password/3, generate_token/0, assign_token/3,
         release_token/3, get_tokens/2, check_token/3,
         maybe_set_handle/3,
         get_handle/2, get_user_by_handle/1,
         set_phone_number/3,
         get_phone_number/2, get_user_by_phone_number/1,
         get_user_data/2,
         get_user_by_external_id/2
        ]).

wocky_db_user_test_() ->
    {ok, _} = application:ensure_all_started(stringprep),
    {"wocky_db_user",
     setup, fun before_all/0, fun after_all/1, [
     {foreach, fun before_each/0, fun after_each/1, [
       [
         test_does_user_exist(),
         test_get_password(),
         test_get_user_data(),
         test_external_id(),
         test_generate_token(),
         test_get_tokens(),
         test_assign_token(),
         test_release_token(),
         test_check_token()
       ],
       test_create_user_with_id(),
       test_set_password(),
       test_remove_user(),
       test_set_phone_number(),
       test_update_user_from_map(),
       test_set_avatar()
     ]},
     test_create_user_from_map(),
     test_maybe_set_handle()
    ]}.

before_all() ->
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(shared, [user,
                                               handle_to_user,
                                               phone_number_to_user]),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [auth_token,
                                                       media,
                                                       media_data]),
    ok.

after_all(_) ->
    ok = wocky_app:stop(),
    ok.

before_each() ->
    ok = wocky_db_seed:seed_tables(shared, [user,
                                            handle_to_user,
                                            phone_number_to_user]),
    ok.

after_each(_) ->
    ok = wocky_db_seed:clear_tables(shared, [user,
                                             handle_to_user,
                                             phone_number_to_user]),
    ok.

before_avatar() ->
    ok = wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [media, media_data]),
    ok.

after_avatar(_) ->
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [media, media_data]).

test_does_user_exist() ->
  { "does_user_exist", [
    { "returns true if the user exists", [
      ?_assert(does_user_exist(?USER, ?SERVER))
    ]},
    { "returns false if the user does not exist", [
      ?_assertNot(does_user_exist(?BADUSER, ?SERVER)),
      ?_assertNot(does_user_exist(<<"alice">>, ?SERVER))
    ]}
  ]}.

test_create_user_with_id() ->
  { "create_user", [
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
  { "get_password", [
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
  { "set_password", [
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
  { "remove_user", [
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
    {ok, Token, Expiry} = assign_token(?USER, ?SERVER, ?RESOURCE),
    {Token, Expiry}.

token_cleanup(_) ->
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [auth_token]).

test_assign_token() ->
  { "assign_token", [
    { "generates and stores a token for the user",
      setup, fun token_setup/0, fun token_cleanup/1, fun ({Token, Expiry}) ->
      {inorder, [
        ?_assertMatch(<<"$T$", _/binary>>, Token),
        ?_assertEqual([Token], get_tokens(?USER, ?SERVER)),
        ?_assert(is_integer(Expiry))
      ]} end},
    { "overwrites tokens when there are multiple requests", {inorder, [
      ?_assertMatch({ok, _, _}, assign_token(?USER, ?SERVER, ?RESOURCE)),
      ?_assertMatch({ok, _, _}, assign_token(?USER, ?SERVER, ?RESOURCE)),
      ?_assertMatch({ok, _, _}, assign_token(?USER, ?SERVER, ?RESOURCE)),
      ?_assertEqual(1, length(get_tokens(?USER, ?SERVER)))
    ]}},
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
  { "release_token", [
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
  { "get_tokens", [
    { "returns all tokens for a user",
      setup, fun token_setup/0, fun token_cleanup/1, fun ({Token, _Expiry}) ->
      {inorder, [
        ?_assertMatch({ok, _, _}, assign_token(?USER, ?SERVER, <<"test1">>)),
        ?_assertMatch({ok, _, _}, assign_token(?USER, ?SERVER, <<"test2">>)),
        ?_assertEqual(3, length(get_tokens(?USER, ?SERVER))),
        ?_assert(lists:member(Token, get_tokens(?USER, ?SERVER)))
    ]} end},
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
  { "check_token", setup, fun token_setup/0, fun token_cleanup/1,
    fun ({Token, _Expiry}) -> [
      { "accepts a valid token", [
        ?_assert(check_token(?USER, ?SERVER, Token))
      ]},
      { "denies any other token", [
        ?_assertNot(check_token(?USER, ?SERVER, <<"badtoken">>))
      ]},
      { "denies tokens with a bad user", [
        ?_assertNot(check_token(wocky_db:create_id(), ?SERVER, Token))
      ]}
   ] end}.

test_maybe_set_handle() ->
  { "maybe_set_handle", foreach, fun before_each/0, fun after_each/1, [
    { "sets a handle on a user", [
        ?_assert(maybe_set_handle(?ALICE, ?SERVER, <<"shinynewhandle">>)),
        ?_assertEqual(<<"shinynewhandle">>, get_handle(?ALICE, ?SERVER)),
        ?_assertEqual({?ALICE, ?SERVER},
                      get_user_by_handle(<<"shinynewhandle">>)),
        ?_assertEqual(not_found, get_user_by_handle(?HANDLE))
    ]},
    { "won't set a handle that's already in use", [
        ?_assertNot(maybe_set_handle(?ALICE, ?SERVER, <<"carol">>)),
        ?_assertEqual(?HANDLE, get_handle(?ALICE, ?SERVER)),
        ?_assertEqual({?ALICE, ?SERVER}, get_user_by_handle(?HANDLE))
    ]},
    { "will fail and remain valid for the same value", [
        ?_assertNot(maybe_set_handle(?ALICE, ?SERVER, ?HANDLE)),
        ?_assertEqual(?HANDLE, get_handle(?ALICE, ?SERVER)),
        ?_assertEqual({?ALICE, ?SERVER}, get_user_by_handle(?HANDLE))
    ]}
  ]}.

test_set_phone_number() ->
  { "set_phone_number", [
    { "sets a phone number on a user", [
        ?_assertEqual(ok, set_phone_number(?ALICE, ?SERVER, <<"+614444">>)),
        ?_assertEqual(<<"+614444">>, get_phone_number(?ALICE, ?SERVER)),
        ?_assertEqual({?ALICE, ?SERVER},
                      get_user_by_phone_number(<<"+614444">>)),
        ?_assertEqual(not_found, get_user_by_phone_number(?PHONE_NUMBER))
    ]},
    { "will set a phone number that's already in use,"
      " and remove from the currently holding user", [
        ?_assertEqual(ok, set_phone_number(?ALICE, ?SERVER, <<"+4567">>)),
        ?_assertEqual(<<"+4567">>, get_phone_number(?ALICE, ?SERVER)),
        ?_assertEqual({?ALICE, ?SERVER}, get_user_by_phone_number(<<"+4567">>)),
        ?_assertEqual({error, not_found}, get_phone_number(?CAROL, ?SERVER))
    ]},
    { "will succeed and remain valid for the same value", [
        ?_assertEqual(ok, set_phone_number(?ALICE, ?SERVER, ?PHONE_NUMBER)),
        ?_assertEqual(?PHONE_NUMBER, get_phone_number(?ALICE, ?SERVER)),
        ?_assertEqual({?ALICE, ?SERVER},
                      get_user_by_phone_number(?PHONE_NUMBER))
    ]}
  ]}.

test_get_user_data() ->
  { "get_user_data", [
    { "gets an existing user's data", [
        ?_assertMatch(#{user := ?ALICE,
                        handle := ?HANDLE,
                        phone_number := ?PHONE_NUMBER,
                        external_id := ?EXTERNAL_ID},
                      get_user_data(?ALICE, ?LOCAL_CONTEXT))
    ]},
    { "returns not_found for non-existant users", [
        ?_assertEqual(not_found,
                      get_user_data(wocky_db:create_id(), ?LOCAL_CONTEXT))
    ]}
  ]}.

test_external_id() ->
  { "external_id", [
    { "gets an existing user from the external_id", [
        ?_assertEqual(?ALICE,
                      get_user_by_external_id(?LOCAL_CONTEXT, ?EXTERNAL_ID))
    ]},
    { "gets not_found for non-existant external_id", [
        ?_assertEqual(not_found,
                      get_user_by_external_id(?LOCAL_CONTEXT, <<"3413212312">>))
    ]}
  ]}.

test_create_user_from_map() ->
  Fields = #{server => ?LOCAL_CONTEXT,
             handle => ?HANDLE,
             phone_number => ?PHONE_NUMBER,
             first_name => <<"Alice">>,
             last_name => <<"Bobson">>,
             email => <<"alice@bob.com">>,
             external_id => ?EXTERNAL_ID
            },
  { "create_user", [
    { "creates a user excluding handle and phone_number fields", [
      ?_assert(is_binary(create_user(Fields))),
      ?_assertMatch(#{server := ?LOCAL_CONTEXT,
                      first_name := <<"Alice">>,
                      last_name := <<"Bobson">>,
                      email := <<"alice@bob.com">>,
                      external_id := ?EXTERNAL_ID,
                      phone_number := null,
                      handle := null
                     },
                    wocky_db:select_row(shared, user, all, #{}))
    ]}
  ]}.

test_update_user_from_map() ->
  Fields = #{user => ?ALICE,
             server => ?LOCAL_CONTEXT,
             handle => <<"NewHandle">>,
             phone_number => <<"+9999">>,
             first_name => <<"Olaf">>
            },
  { "update_user", [
    { "updates a user excluding handle and phone_number fields", [
      ?_assertEqual(ok, update_user(Fields)),
      ?_assertMatch(#{user := ?ALICE,
                      handle := ?HANDLE,
                      phone_number := ?PHONE_NUMBER,
                      first_name := <<"Olaf">>},
                    wocky_db_user:get_user_data(?ALICE, ?LOCAL_CONTEXT))
    ]}
  ]}.

test_set_avatar() ->
  AvatarURL = tros:make_url(?LOCAL_CONTEXT, ?AVATAR_FILE2),
  Fields = #{user => ?ALICE,
             server => ?LOCAL_CONTEXT,
             avatar => AvatarURL
            },
  NonExistantURL = tros:make_url(?LOCAL_CONTEXT, mod_tros:make_file_id()),
  MediaURL = tros:make_url(?LOCAL_CONTEXT, ?MEDIA_FILE),

  { "set user's avatar", setup, fun before_avatar/0, fun after_avatar/1, [
    { "succesfully set avatar and delete the old one", [
      ?_assertEqual(ok, update_user(Fields)),
      ?_assertEqual(AvatarURL,
                    maps:get(avatar,
                             wocky_db_user:get_user_data(?ALICE,
                                                         ?LOCAL_CONTEXT))),
      ?_assertEqual(not_found,
                    wocky_db:select_one(?LOCAL_CONTEXT, media,
                                        id, #{id => ?AVATAR_FILE}))
    ]},
    { "fail to set avatar due to wrong owner", [
      ?_assertEqual({error, not_file_owner},
                        update_user(Fields#{user => ?BOB})),
      ?_assertEqual(null,
                    maps:get(avatar,
                             wocky_db_user:get_user_data(?BOB,
                                                         ?LOCAL_CONTEXT)))
    ]},
    { "fail to set avatar due to non-existant file", [
      ?_assertEqual({error, not_found},
                        update_user(Fields#{avatar => NonExistantURL})),
      ?_assertEqual(AvatarURL,
                    maps:get(avatar,
                             wocky_db_user:get_user_data(?ALICE,
                                                         ?LOCAL_CONTEXT)))
    ]},
    { "fail to set avatar due to non-avatar type file", [
      ?_assertEqual({error, not_avatar_file},
                        update_user(Fields#{avatar => MediaURL})),
      ?_assertEqual(AvatarURL,
                    maps:get(avatar,
                             wocky_db_user:get_user_data(?ALICE,
                                                         ?LOCAL_CONTEXT)))
    ]}
  ]}.
