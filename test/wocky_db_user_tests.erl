%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for wocky_db_user.erl
-module(wocky_db_user_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").

-import(wocky_db_user,
        [register_user/3, register_user/2, update_user/3, remove_user/2,
         does_user_exist/2, find_user/2, find_user_by/2,
         %% TODO get_handle/2, get_phone_number/2,
         get_password/2, set_password/3, set_location/4,
         assign_token/3, release_token/3, check_token/3,
         generate_token/0, get_tokens/2]).

wocky_db_user_test_() ->
    {ok, _} = application:ensure_all_started(stringprep),
    {"wocky_db_user",
     setup, fun before_all/0, fun after_all/1, [
     {foreach, fun before_each/0, fun after_each/1, [
       [
         test_does_user_exist(),
         test_get_password(),
         test_find_user(),
         test_find_user_by(),
         test_generate_token(),
         test_get_tokens(),
         test_assign_token(),
         test_release_token(),
         test_check_token()
       ],
       test_register_user_with_id(),
       test_update_user(),
       test_update_user_with_avatar(),
       test_set_password(),
       test_remove_user()
     ]},
     test_register_user_with_external_id(),
     test_set_location()
    ]}.

before_all() ->
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(shared, [user,
                                               handle_to_user,
                                               phone_number_to_user]),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [auth_token,
                                                       media,
                                                       media_data,
                                                       location]),
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

test_register_user_with_id() ->
  { "register_user/3", [
    { "creates a user if none exists", [
      ?_assertMatch(ok, register_user(?NEWUSER, ?SERVER, ?PASS)),
      ?_assert(does_user_exist(?NEWUSER, ?SERVER))
    ]},
    { "updates the user if it already exists", [
      ?_assertMatch(ok, register_user(?USER, ?SERVER, ?PASS))
    ]}
  ]}.

test_get_password() ->
  { "get_password", [
    { "returns password if user exists", [
      ?_assertMatch(?SCRAM, get_password(?USER, ?SERVER))
    ]},
    { "returns not_found if user does not exist", [
      ?_assertMatch(not_found, get_password(?BADUSER, ?SERVER))
    ]},
    { "returns not_found if user ID is not a valid UUID", [
      ?_assertMatch(not_found, get_password(<<"alice">>, ?SERVER))
    ]}
  ]}.

test_set_password() ->
  { "set_password", [
    { "sets password if user exists", [
      ?_assertMatch(ok, set_password(?USER, ?SERVER, <<"newpass">>)),
      ?_assertMatch(<<"newpass">>, get_password(?USER, ?SERVER))
    ]},
    { "returns not_found if user does not exist", [
      ?_assertMatch(not_found, set_password(?BADUSER, ?SERVER, ?PASS))
    ]},
    { "returns not_found if user ID is not a valid UUID", [
      ?_assertMatch(not_found, set_password(<<"alice">>, ?SERVER, ?PASS))
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
    { "returns not_found if user does not exist", [
      ?_assertEqual(not_found, assign_token(?BADUSER, ?SERVER, ?RESOURCE))
    ]},
    { "returns not_found if user ID is not a valid UUID", [
      ?_assertEqual(not_found, assign_token(<<"alice">>, ?SERVER, ?RESOURCE))
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

test_find_user() ->
  { "find_user", [
    { "gets an existing user's data", [
        ?_assertMatch(#{user := ?ALICE,
                        handle := ?HANDLE,
                        external_id := ?EXTERNAL_ID},
                      find_user(?ALICE, ?LOCAL_CONTEXT))
    ]},
    { "returns not_found for non-existant users", [
        ?_assertEqual(not_found,
                      find_user(wocky_db:create_id(), ?LOCAL_CONTEXT))
    ]}
  ]}.

test_find_user_by() ->
  { "find_user_by", [
    { "gets an existing user from the external_id", [
        ?_assertMatch(#{user := ?ALICE},
                      find_user_by(external_id, ?EXTERNAL_ID))
    ]},
    { "gets not_found for non-existant external_id", [
        ?_assertEqual(not_found, find_user_by(external_id, <<"3413212312">>))
    ]},
    { "gets an existing user from the handle", [
        ?_assertMatch(#{user := ?ALICE},
                      find_user_by(handle, ?HANDLE))
    ]},
    { "gets not_found for non-existant handle", [
        ?_assertEqual(not_found, find_user_by(handle, <<"nosuchhandle">>))
    ]},
    { "gets an existing user from the phone number", [
        ?_assertMatch(#{user := ?ALICE},
                      find_user_by(phone_number, ?PHONE_NUMBER))
    ]},
    { "gets not_found for non-existant phone number", [
        ?_assertEqual(not_found, find_user_by(phone_number, <<"+1555bogus">>))
    ]}
  ]}.

test_register_user_with_external_id() ->
  { "create_user", [
    { "creates a user excluding handle and phone_number fields", [
      ?_assertMatch({ok, _}, register_user(?EXTERNAL_ID, ?PHONE_NUMBER)),
      ?_assertMatch(#{server := ?SERVER,
                      external_id := ?EXTERNAL_ID,
                      handle := null},
                    wocky_db:select_row(shared, user, all, #{}))
    ]}
  ]}.

test_update_user() ->
  Fields = #{handle => <<"NewHandle">>,
             first_name => <<"Olaf">>},
  { "update_user", [
    { "updates a user excluding handle and phone_number fields", [
      ?_assertEqual(ok, update_user(?ALICE, ?SERVER, Fields)),
      ?_assertMatch(#{user := ?ALICE,
                      handle := <<"NewHandle">>,
                      first_name := <<"Olaf">>},
                    wocky_db_user:find_user(?ALICE, ?LOCAL_CONTEXT))
    ]}
  ]}.

test_update_user_with_avatar() ->
  AvatarURL = tros:make_url(?LOCAL_CONTEXT, ?AVATAR_FILE2),
  Fields = #{avatar => AvatarURL},
  NonExistantURL = tros:make_url(?LOCAL_CONTEXT, mod_tros:make_file_id()),
  MediaURL = tros:make_url(?LOCAL_CONTEXT, ?MEDIA_FILE),

  { "set user's avatar", setup, fun before_avatar/0, fun after_avatar/1, [
    { "succesfully set avatar and delete the old one", [
      ?_assertEqual(ok, update_user(?ALICE, ?SERVER, Fields)),
      ?_assertEqual(AvatarURL,
                    maps:get(avatar,
                             wocky_db_user:find_user(?ALICE, ?SERVER))),
      ?_assertEqual(not_found,
                    wocky_db:select_one(?LOCAL_CONTEXT, media,
                                        id, #{id => ?AVATAR_FILE}))
    ]},
    { "fail to set avatar due to wrong owner", [
      ?_assertEqual({error, not_file_owner},
                        update_user(?BOB, ?SERVER, Fields)),
      ?_assertEqual(null,
                    maps:get(avatar,
                             wocky_db_user:find_user(?BOB, ?SERVER)))
    ]},
    { "fail to set avatar due to non-existant file", [
      ?_assertEqual({error, not_found},
                        update_user(?ALICE, ?SERVER,
                                    #{avatar => NonExistantURL})),
      ?_assertEqual(AvatarURL,
                    maps:get(avatar,
                             wocky_db_user:find_user(?ALICE, ?SERVER)))
    ]},
    { "fail to set avatar due to non-avatar type file", [
      ?_assertEqual({error, not_avatar_file},
                        update_user(?ALICE, ?SERVER, #{avatar => MediaURL})),
      ?_assertEqual(AvatarURL,
                    maps:get(avatar,
                             wocky_db_user:find_user(?ALICE, ?SERVER)))
    ]}
  ]}.

test_set_location() ->
    AliceJID = jid:make(?ALICE, ?LOCAL_CONTEXT, ?RESOURCE),
    CountMatch = #{user => ?ALICE, server => ?LOCAL_CONTEXT},
    { "set a user's location", [
      { "set a user's location without overwriting previous ones", [
        ?_assertEqual(ok, set_location(AliceJID, 1, 2, 3)),
        ?_assertEqual(1, wocky_db:count(?LOCAL_CONTEXT, location,
                                        CountMatch)),
        ?_assertEqual(ok, set_location(AliceJID, 4.0, 5.0, 6.0)),
        ?_assertEqual(2, wocky_db:count(?LOCAL_CONTEXT, location,
                                        CountMatch)),
        ?_assertEqual(ok, set_location(AliceJID, 6.6, 7.7, 8.8)),
        ?_assertEqual(3, wocky_db:count(?LOCAL_CONTEXT, location,
                                        CountMatch))
      ]},
      { "first location result should be most recently set location", [
        ?_assertEqual(#{lat => 6.6, lon => 7.7, accuracy => 8.8},
                      wocky_db:select_row(?LOCAL_CONTEXT, location,
                                          [lat, lon, accuracy],
                                          CountMatch))
      ]}
    ]}.
