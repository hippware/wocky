%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for ejabberd_auth_wocky.erl
-module(ejabberd_auth_wocky_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").

-import(ejabberd_auth_wocky, [
    login/2, plain_password_required/0, dirty_get_registered_users/0,
    get_vh_registered_users/1, get_vh_registered_users/2,
    get_vh_registered_users_number/1, get_vh_registered_users_number/2,
    store_type/1, check_password/5, check_password/3, set_password/3,
    get_password/2, get_password/3, get_password_s/2, does_user_exist/2,
    remove_user/2, remove_user/3, try_register/3
]).


encode_password(Password) ->
    do_encode_password(Password, scram:enabled(?SERVER)).

do_encode_password(P, false) -> P;
do_encode_password(P, true) ->
    scram:serialize(scram:password_to_scram(P, scram:iterations(?SERVER))).

mk_digest(User, Password) ->
    do_mk_digest(User, Password, scram:enabled(?SERVER)).

do_mk_digest(_U, P, false) -> P;
do_mk_digest(U, _P, true) ->
    SerializedScram = wocky_db_user:get_password(U, ?SERVER),
    {ok, {scram, Key, _, _, _}} = scram:deserialize(SerializedScram),
    base64:decode(Key).


ejabberd_auth_wocky_with_scram_test_() -> {
  "ejabberd_auth_wocky with scram",
  setup,
  fun() -> before_all(scram) end,
  fun after_all/1,
  [
    test_store_type_with_scram(),
    test_check_password_with_digest(),
    test_check_password(),
    test_set_password(),
    test_get_password_with_default(),
    test_get_password_with_scram(),
    test_get_password_s_with_scram(),
    test_does_user_exist(),
    test_remove_user(),
    test_remove_user_with_password(),
    test_try_register()
  ]
}.

ejabberd_auth_wocky_without_scram_test_() -> {
  "ejabberd_auth_wocky without scram",
  setup,
  fun() -> before_all(plain) end,
  fun after_all/1,
  [
    test_store_type_without_scram(),
    test_check_password_with_digest(),
    test_check_password(),
    test_set_password(),
    test_get_password_with_default(),
    test_get_password_without_scram(),
    test_get_password_s_without_scram(),
    test_does_user_exist(),
    test_remove_user(),
    test_remove_user_with_password(),
    test_try_register()
  ]
}.

ejabberd_auth_wocky_sanity_test_() -> {
  "sanity check stubbed module functions",
  setup,
  fun() -> before_all(scram) end,
  fun after_all/1,
  [
    ?_assertError(not_implemented, login(user, server)),
    ?_assertNot(plain_password_required()),

    ?_assertEqual([], dirty_get_registered_users()),
    ?_assertEqual([], get_vh_registered_users(?SERVER)),
    ?_assertEqual([], get_vh_registered_users(?SERVER, [])),
    ?_assertEqual(0, get_vh_registered_users_number(?SERVER)),
    ?_assertEqual(0, get_vh_registered_users_number(?SERVER, [])),

    ?_assert(meck:validate(ejabberd_config))
  ]
}.

before_all(PasswordFormat) ->
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_vh_by_auth_method,
                fun(_) -> [?SERVER] end),
    meck:expect(ejabberd_config, get_local_option,
                fun(auth_opts, _Host) ->
                    [{password_format, PasswordFormat}]
                end),

    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(shared, [handle_to_user]),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [user, auth_token]),
    ok = ejabberd_auth_wocky:start(?SERVER),
    ok.

after_all(_) ->
    ok = ejabberd_auth_wocky:stop(?SERVER),
    ok = wocky_app:stop(),

    meck:unload(),
    ok.

before_each() ->
    ok = wocky_db_user:create_user(?USER, ?SERVER, ?HANDLE,
                                   encode_password(?PASS)),
    {ok, _} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, auth_token),
    ok.

after_each(_) ->
    ok = wocky_db_seed:clear_tables(shared, [handle_to_user]),
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [user, auth_token]),
    ok.


test_store_type_with_scram() ->
  { "store_type/1 with scram",
    foreach, fun before_each/0, fun after_each/1, [
    { "returns 'scram'", [
      ?_assertEqual(scram, store_type(?SERVER))
    ]}
  ]}.

test_store_type_without_scram() ->
  { "store_type/1 without scram",
    foreach, fun before_each/0, fun after_each/1, [
    { "returns 'plain'", [
      ?_assertEqual(plain, store_type(?SERVER))
    ]}
  ]}.

test_check_password_with_digest() ->
  DigestGen = fun(X) -> X end,
  { "check_password/5", foreach, fun before_each/0, fun after_each/1, [
    { "returns true when user exists and password digest matches", [
      ?_assert(check_password(?USER, ?SERVER, ?PASS,
                              mk_digest(?USER, ?PASS), DigestGen))
    ]},
    { "returns false when user exists but password digest does not match", [
      ?_assertNot(check_password(?USER, ?SERVER, <<"niemakota">>,
                                 <<"readers">>, DigestGen))
    ]},
    { "returns false when user does not exist", [
      ?_assertNot(check_password(?BADUSER, ?SERVER, ?PASS,
                                 <<"readers">>, DigestGen))
    ]}
  ]}.

test_check_password() ->
  { "check_password/3", foreach, fun before_each/0, fun after_each/1, [
    { "returns true when user exists and password matches", [
      ?_assert(check_password(?USER, ?SERVER, ?PASS))
    ]},
    { "returns true when user exists and token matches", [
      ?_assert(check_password(?USER, ?SERVER, ?TOKEN))
    ]},
    { "returns false when user exists but password does not match", [
      ?_assertNot(check_password(?USER, ?SERVER, <<"niemakota">>))
    ]},
    { "returns false when user exists but token does not match", [
      ?_assertNot(check_password(?USER, ?SERVER,
                                 wocky_db_user:generate_token()))
    ]},
    { "returns false when user does not exist", [
      ?_assertNot(check_password(?BADUSER, ?SERVER, ?PASS))
    ]}
  ]}.

test_set_password() ->
  { "set_password/3", foreach, fun before_each/0, fun after_each/1, [
    { "changes the user's password", [
      ?_assertEqual(ok, set_password(?USER, ?SERVER, <<"mialakota">>)),
      ?_assert(check_password(?USER, ?SERVER, <<"mialakota">>))
    ]},
    { "returns {error, invalid_jid} if the user doesn't exist", [
      ?_assertEqual({error, invalid_jid},
                    set_password(?BADUSER, ?SERVER, <<"ticktock">>))
    ]}
  ]}.

test_get_password_with_default() ->
  { "get_password/3", foreach, fun before_each/0, fun after_each/1, [
    { "returns the same value as get_password/2", [
      ?_assertEqual(get_password(?USER, ?SERVER),
                    get_password(?USER, ?SERVER, [])),
      ?_assertEqual(get_password(?BADUSER, ?SERVER),
                    get_password(?BADUSER, ?SERVER, []))
    ]}
  ]}.

test_get_password_with_scram() ->
  { "get_password/2 with scram",
    foreach, fun before_each/0, fun after_each/1, [
    { "returns a tuple with scram data when the user exists", [
      ?_assertMatch({_, _, _, 4096},
                    get_password(?USER, ?SERVER))
    ]},
    { "returns false when the user doesn't exist", [
      ?_assertEqual(false, get_password(?BADUSER, ?SERVER))
    ]}
  ]}.

test_get_password_without_scram() ->
  { "get_password/2 without scram",
    foreach, fun before_each/0, fun after_each/1, [
    { "returns the stored password", [
      ?_assertMatch(?PASS, get_password(?USER, ?SERVER))
    ]},
    { "returns false when the user doesn't exist", [
      ?_assertEqual(false, get_password(?BADUSER, ?SERVER))
    ]}
  ]}.

test_get_password_s_with_scram() ->
  { "get_password_s/2 with scram",
    foreach, fun before_each/0, fun after_each/1, [
    { "returns an empty binary regardless of whether the user exists", [
      ?_assertEqual(<<>>, get_password_s(?USER, ?SERVER)),
      ?_assertEqual(<<>>, get_password_s(?BADUSER, ?SERVER))
    ]}
  ]}.

test_get_password_s_without_scram() ->
  { "get_password_s/2 without scram",
    foreach, fun before_each/0, fun after_each/1, [
    { "returns the stored password if the user exists", [
      ?_assertEqual(?PASS, get_password_s(?USER, ?SERVER))
    ]},
    { "returns an empty binary if the user does not exist", [
      ?_assertEqual(<<>>, get_password_s(?BADUSER, ?SERVER))
    ]}
  ]}.

test_does_user_exist() ->
  { "does_user_exist/2", foreach, fun before_each/0, fun after_each/1, [
    { "returns true if the user exists", [
      ?_assert(does_user_exist(?USER, ?SERVER))
    ]},
    { "returns false if the user does not exist", [
      ?_assertNot(does_user_exist(?BADUSER, ?SERVER))
    ]}
  ]}.

test_remove_user() ->
  { "remove_user/2", foreach, fun before_each/0, fun after_each/1, [
    { "removes the user if the user exists", [
      ?_assertEqual(ok, remove_user(?USER, ?SERVER)),
      ?_assertNot(does_user_exist(?USER, ?SERVER))
    ]},
    { "returns ok if the user doesn't exist", [
      ?_assertEqual(ok, remove_user(?BADUSER, ?SERVER))
    ]}
  ]}.

test_remove_user_with_password() ->
  { "remove_user/3", foreach, fun before_each/0, fun after_each/1, [
    { "removes the user if the user exists and the password matches", [
      ?_assertEqual(ok, remove_user(?USER, ?SERVER, ?PASS)),
      ?_assertNot(does_user_exist(?USER, ?SERVER))
    ]},
    { "returns {error, not_allowed} if the password does not match", [
      ?_assertEqual({error, not_allowed},
                    remove_user(?USER, ?SERVER, <<"niemakota">>)),
      ?_assert(does_user_exist(?USER, ?SERVER))
    ]},
    { "returns ok if the user doesn't exist", [
      ?_assertEqual(ok, remove_user(?BADUSER, ?SERVER, <<"ticktock">>))
    ]}
  ]}.

test_try_register() ->
  { "try_register/3", foreach, fun before_each/0, fun after_each/1, [
    { "creates the user if it does not already exist", [
      ?_assertEqual(ok, try_register(?NEWUSER, ?SERVER, ?PASS)),
      ?_assert(does_user_exist(?NEWUSER, ?SERVER))
    ]},
    { "returns {error, exists} if the user already exists", [
      ?_assertEqual(ok, try_register(?NEWUSER, ?SERVER, ?PASS)),
      ?_assertEqual({error, exists}, try_register(?NEWUSER, ?SERVER, ?PASS))
    ]}
  ]}.
