%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for wocky_db_user.erl
-module(wocky_db_user_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").

-import(wocky_db_user,
        [register_user/2, update_user/3, remove_user/2,
         find_user/2, find_user_by/2, get_handle/2, get_phone_number/2,
         set_location/6, assign_token/3, release_token/3, check_token/3,
         generate_token/0, get_tokens/2,
         add_roster_viewer/3, remove_roster_viewer/3, get_roster_viewers/2]).

register_user(User, Server, Pass) ->
    case ejabberd_auth_wocky:try_register(User, Server, Pass) of
        ok -> ok;
        {error, exists} -> ok;
        Error -> Error
    end.

wocky_db_user_test_() ->
    {"wocky_db_user",
     setup, fun before_all/0, fun after_all/1, [
     {foreach, fun before_each/0, fun after_each/1, [
       [
         test_get_handle(),
         test_get_phone_number(),
         test_find_user(),
         test_find_user_by(),
         test_generate_token(),
         test_get_tokens(),
         test_assign_token(),
         test_release_token(),
         test_check_token(),
         test_get_roster_viewers()
       ],
       test_add_roster_viewer(),
       test_remove_roster_viewer(),
       test_register_user_with_external_id(),
       test_update_user(),
       test_update_user_with_avatar(),
       test_remove_user()
     ]},
     test_set_location()
    ]}.

before_all() ->
    ok = wocky_db:clear_tables(?LOCAL_CONTEXT, [auth_token, location]),
    wocky_db_seed:maybe_seed_s3_file(?ALICE_JID, ?AVATAR_FILE),
    wocky_db_seed:maybe_seed_s3_file(?ALICE_JID, ?AVATAR_FILE2),
    ok.

after_all(_) ->
    ok.

before_each() ->
    ok = wocky_db_seed:seed_tables(shared, [user, handle_to_user,
                                            phone_number_to_user]),
    ok.

after_each(_) ->
    ok.

before_avatar() ->
    ok = wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [media, media_data]),
    ok.

after_avatar(_) ->
    ok.

test_get_handle() ->
  { "get_handle", [
    { "returns the user's handle", [
      ?_assertEqual(?HANDLE, get_handle(?USER, ?SERVER))
    ]},
    { "returns not_found if the user does not exist", [
      ?_assertEqual(not_found, get_handle(?BADUSER, ?SERVER))
    ]},
    { "returns not_found if the user does not have a handle", [
      ?_assertEqual(ok, register_user(?NEWUSER, ?SERVER, ?PASS)),
      ?_assertEqual(not_found, get_handle(?NEWUSER, ?SERVER))
    ]}
  ]}.

test_get_phone_number() ->
  { "get_phone_number", [
    { "returns the user's phone number", [
      ?_assertEqual(?PHONE_NUMBER, get_phone_number(?USER, ?SERVER))
    ]},
    { "returns not_found if the user does not exist", [
      ?_assertEqual(not_found, get_phone_number(?BADUSER, ?SERVER))
    ]},
    { "returns not_found if the user does not have a phone_number", [
      ?_assertEqual(ok, register_user(?NEWUSER, ?SERVER, ?PASS)),
      ?_assertEqual(not_found, get_phone_number(?NEWUSER, ?SERVER))
    ]}
  ]}.

test_remove_user() ->
  { "remove_user", [
    { "removes user if user exists", [
      ?_assertEqual(ok, remove_user(?USER, ?SERVER)),
      ?_assertEqual(not_found, find_user(?USER, ?SERVER))
    ]},
    { "succeeds if user does not exist", [
      ?_assertEqual(ok, remove_user(?BADUSER, ?SERVER))
    ]},
    { "returns ok if user ID is not a valid UUID", [
      ?_assertEqual(ok, remove_user(<<"alice">>, ?SERVER))
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
    ok = wocky_db:clear_tables(?LOCAL_CONTEXT, [auth_token]).

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
        ?_assertNot(check_token(?wocky_id:create(), ?SERVER, Token))
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
                      find_user(?wocky_id:create(), ?LOCAL_CONTEXT))
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
    { "creates a user when none exists", [
      ?_assertMatch({ok, {_, ?SERVER, true}},
                    register_user(<<"987654321">>, <<"+15559876543">>))
    ]},
    { "updates an existing user when one exists", [
      ?_assertMatch({ok, {?ALICE, ?SERVER, false}},
                    register_user(?EXTERNAL_ID, ?PHONE_NUMBER))
    ]}
  ]}.

test_update_user() ->
  NullHandleUser = ?wocky_id:create(),
  { "update_user", [
    { "updates a user's handle if it is unique", [
      ?_assertEqual(ok, update_user(?ALICE, ?SERVER,
                                    #{handle => <<"NewHandle">>})),
      ?_assertMatch(#{user := ?ALICE, handle := <<"NewHandle">>},
                    wocky_db_user:find_user(?ALICE, ?SERVER))
    ]},
    { "returns ok if the new handle is the same as the old handle", [
      ?_assertEqual(ok, update_user(?ALICE, ?SERVER,
                                    #{handle => ?HANDLE})),
      ?_assertMatch(#{user := ?ALICE, handle := ?HANDLE},
                    wocky_db_user:find_user(?ALICE, ?SERVER))
    ]},
    { "updates a user's handle when the old handle is null", [
      ?_assertEqual(not_found, find_user(NullHandleUser, ?SERVER)),
      ?_assertEqual(ok, register_user(NullHandleUser, ?SERVER, ?PASS)),
      ?_assertEqual(ok, update_user(NullHandleUser, ?SERVER,
                                    #{handle => <<"NewHandle">>})),
      ?_assertMatch(#{user := NullHandleUser, handle := <<"NewHandle">>},
                    wocky_db_user:find_user(NullHandleUser, ?SERVER))
    ]},
    { "returns {error, duplicate_handle} if the new handle already exists", [
      ?_assertEqual({error, duplicate_handle},
                    update_user(?ALICE, ?SERVER, #{handle => <<"carol">>}))
    ]},
    { "updates a user's email", [
      ?_assertEqual(ok, update_user(?ALICE, ?SERVER,
                                    #{email => <<"alice@wonderland.lit">>})),
      ?_assertMatch(#{user := ?ALICE, email := <<"alice@wonderland.lit">>},
                    wocky_db_user:find_user(?ALICE, ?SERVER))
    ]},
    { "updates a user's first and last name", [
      ?_assertEqual(ok, update_user(?ALICE, ?SERVER,
                                    #{first_name => <<"Svein">>,
                                      last_name => <<"Forkbeard">>})),
      ?_assertMatch(#{user := ?ALICE,
                      first_name := <<"Svein">>,
                      last_name := <<"Forkbeard">>},
                    wocky_db_user:find_user(?ALICE, ?SERVER))
    ]},
    { "returns ok if passed an empty list of fields", [
      ?_assertEqual(ok, update_user(?ALICE, ?SERVER, #{}))
    ]},
    { "won't update a user's server or external id", [
      ?_assertEqual(ok, update_user(?ALICE, ?SERVER,
                                    #{server => <<"neverwhere">>,
                                      external_id => <<"987654321">>})),
      ?_assertMatch(#{user := ?ALICE,
                      server := ?SERVER,
                      external_id := ?EXTERNAL_ID},
                    wocky_db_user:find_user(?ALICE, ?SERVER))
    ]},
    { "will 'upsert' a user that doesn't already exist", [
      ?_assertEqual(not_found, find_user(?NEWUSER, ?SERVER)),
      ?_assertEqual(ok, update_user(?NEWUSER, ?SERVER,
                                    #{handle => <<"auniquehandle">>})),
      ?_assertMatch(#{user := ?NEWUSER, handle := <<"auniquehandle">>},
                    wocky_db_user:find_user(?NEWUSER, ?SERVER))
    ]}
  ]}.

test_update_user_with_avatar() ->
  AvatarURL = tros:make_url(?SERVER, ?AVATAR_FILE2),
  Fields = #{avatar => AvatarURL},
  NonExistantURL = tros:make_url(?SERVER, mod_wocky_tros:make_file_id()),
  NonLocalURL = tros:make_url(<<"nonlocal.com">>,
                              mod_wocky_tros:make_file_id()),

  { "set user's avatar", setup, fun before_avatar/0, fun after_avatar/1, [
    { "succesfully set avatar and delete the old one", [
      ?_assertEqual(ok, update_user(?ALICE, ?SERVER, Fields)),
      ?_assertEqual(AvatarURL,
                    maps:get(avatar,
                             wocky_db_user:find_user(?ALICE, ?SERVER))),
      ?_assertEqual({error, not_found},
                    tros:get_metadata(?LOCAL_CONTEXT, ?AVATAR_FILE))
    ]},
    { "succesfully set avatar when the existing avatar is bogus", [
      ?_assertEqual(ok, wocky_db:update(shared, user,
                                        #{avatar => <<"bogus">>},
                                        #{user => ?ALICE, server => ?SERVER})),
      ?_assertEqual(ok, update_user(?ALICE, ?SERVER, Fields)),
      ?_assertEqual(AvatarURL,
                    maps:get(avatar,
                             wocky_db_user:find_user(?ALICE, ?SERVER)))
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
    { "fail to set avatar due to non-local file", [
      ?_assertEqual({error, not_local_file},
                        update_user(?ALICE, ?SERVER,
                                    #{avatar => NonLocalURL})),
      ?_assertEqual(AvatarURL,
                    maps:get(avatar,
                             wocky_db_user:find_user(?ALICE, ?SERVER)))
    ]}
  ]}.

test_set_location() ->
    CountMatch = #{user => ?ALICE, server => ?SERVER},
    { "set a user's location", [
      { "set a user's location without overwriting previous ones", [
        ?_assertEqual(ok, set_location(?ALICE, ?SERVER, ?RESOURCE, 1, 2, 3)),
        ?_assertEqual(1, wocky_db:count(?LOCAL_CONTEXT, location, CountMatch)),
        ?_assertEqual(ok, set_location(?ALICE, ?SERVER, ?RESOURCE,
                                       4.0, 5.0, 6.0)),
        ?_assertEqual(2, wocky_db:count(?LOCAL_CONTEXT, location, CountMatch)),
        ?_assertEqual(ok, set_location(?ALICE, ?SERVER, ?RESOURCE,
                                       6.6, 7.7, 8.8)),
        ?_assertEqual(3, wocky_db:count(?LOCAL_CONTEXT, location, CountMatch))
      ]},
      { "first location result should be most recently set location", [
        ?_assertEqual(#{lat => 6.6, lon => 7.7, accuracy => 8.8},
                      wocky_db:select_row(?LOCAL_CONTEXT, location,
                                          [lat, lon, accuracy],
                                          CountMatch))
      ]}
    ]}.

test_add_roster_viewer() ->
  { "add_roster_viewer", [
    { "Adds a roster viewer to a user", [
      ?_assertEqual(ok, add_roster_viewer(?ALICE, ?LOCAL_CONTEXT, ?CAROL_JID)),
      ?_assertEqual(lists:sort([?CAROL_B_JID | ?ROSTER_VIEWERS]),
                    lists:sort(get_roster_viewers(?ALICE, ?LOCAL_CONTEXT)))
    ]},
    { "Succeeds but makes no change for an existing viewer", [
      ?_assertEqual(lists:sort([?CAROL_B_JID | ?ROSTER_VIEWERS]),
                    lists:sort(get_roster_viewers(?ALICE, ?LOCAL_CONTEXT))),
      ?_assertEqual(ok, add_roster_viewer(?ALICE, ?LOCAL_CONTEXT, ?CAROL_JID)),
      ?_assertEqual(lists:sort([?CAROL_B_JID | ?ROSTER_VIEWERS]),
                    lists:sort(get_roster_viewers(?ALICE, ?LOCAL_CONTEXT)))
    ]},
    { "Returns not_found for a non-existant user", [
      ?_assertEqual(not_found, get_roster_viewers(?wocky_id:create(),
                                                  ?LOCAL_CONTEXT))
    ]}
  ]}.

test_remove_roster_viewer() ->
  { "remove_roster_viewer", [
    { "Removes a roster viewer from a user", [
      ?_assertEqual(ok, remove_roster_viewer(?ALICE, ?LOCAL_CONTEXT, ?BOB_JID)),
      ?_assertEqual([], get_roster_viewers(?ALICE, ?LOCAL_CONTEXT))
    ]},
    { "Succeeds but makes no change for a non-viewer", [
      ?_assertEqual(ok, remove_roster_viewer(
                          ?ALICE, ?LOCAL_CONTEXT, ?KAREN_JID)),
      ?_assertEqual([], get_roster_viewers(?ALICE, ?LOCAL_CONTEXT))
    ]},
    { "Returns not_found for a non-existant user", [
      ?_assertEqual(not_found, remove_roster_viewer(?wocky_id:create(),
                                           ?LOCAL_CONTEXT, ?CAROL_JID))
    ]}
  ]}.

test_get_roster_viewers() ->
  { "get_roster_viewers", [
    { "Get the list of roster viewers", [
      ?_assertEqual(lists:sort(?ROSTER_VIEWERS),
                    lists:sort(get_roster_viewers(?ALICE, ?LOCAL_CONTEXT)))
    ]},
    { "Returns not_found for non-existant users", [
      ?_assertEqual(not_found, get_roster_viewers(?wocky_id:create(),
                                               ?LOCAL_CONTEXT))
    ]},
    { "Returns empty list for users with no viewers", [
      ?_assertEqual([], get_roster_viewers(?BOB, ?LOCAL_CONTEXT))
    ]}
  ]}.
