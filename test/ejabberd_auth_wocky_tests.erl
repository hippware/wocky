%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for ejabberd_auth_wocky.erl
-module(ejabberd_auth_wocky_tests).

-include_lib("eunit/include/eunit.hrl").

-define(USER,    <<"043e8c96-ba30-11e5-9912-ba0be0483c18">>).
-define(DOMAIN,  <<"localhost">>).
-define(NAME,    <<"bob">>).
-define(PASS,    <<"password">>).

-define(BADUSER, <<"d51f92c8-ba40-11e5-9912-ba0be0483c18">>).
-define(NEWUSER, <<"9d7acab4-ba30-11e5-9912-ba0be0483c18">>).

encode_password(Password) ->
    do_encode_password(Password, scram:enabled(?DOMAIN)).

do_encode_password(P, false) -> P;
do_encode_password(P, true) ->
    scram:serialize(scram:password_to_scram(P, scram:iterations(?DOMAIN))).

mk_digest(User, Password) ->
    do_mk_digest(User, Password, scram:enabled(?DOMAIN)).

do_mk_digest(_U, P, false) -> P;
do_mk_digest(U, _P, true) ->
    SerializedScram = wocky_db_user:get_password(U, ?DOMAIN),
    {ok, {scram, Key, _, _, _}} = scram:deserialize(SerializedScram),
    base64:decode(Key).


ejabberd_auth_wocky_with_scram_test_() -> {
  "ejabberd_auth_wocky with scram",
  setup,
  fun() -> before_all(scram) end,
  fun after_all/1,
  [
    test_store_type_with_scram(),
    test_check_password_with_options(),
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
    test_check_password_with_options(),
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
    ?_assertError(not_implemented, ejabberd_auth_wocky:login(user, server)),
    ?_assertNot(ejabberd_auth_wocky:plain_password_required()),

    ?_assertEqual([], ejabberd_auth_wocky:dirty_get_registered_users()),
    ?_assertEqual([], ejabberd_auth_wocky:get_vh_registered_users(?DOMAIN)),
    ?_assertEqual([], ejabberd_auth_wocky:get_vh_registered_users(?DOMAIN, [])),
    ?_assertEqual(0, ejabberd_auth_wocky:get_vh_registered_users_number(
                       ?DOMAIN)),
    ?_assertEqual(0, ejabberd_auth_wocky:get_vh_registered_users_number(
                       ?DOMAIN, [])),

    ?_assert(meck:validate(ejabberd_config))
  ]
}.

before_all(PasswordFormat) ->
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_vh_by_auth_method,
                fun(_) -> [?DOMAIN] end),
    meck:expect(ejabberd_config, get_local_option,
                fun(auth_opts, _Host) ->
                    [{password_format, PasswordFormat}]
                end),

    application:ensure_started(p1_stringprep),
    ok = wocky_app:start(),
    ok = ejabberd_auth_wocky:start(?DOMAIN),
    ok.

after_all(_) ->
    ok = ejabberd_auth_wocky:stop(?DOMAIN),
    ok = wocky_app:stop(),
    application:stop(p1_stringprep),
    application:unload(p1_stringprep),
    code:delete(p1_stringprep),
    code:purge(p1_stringprep),

    meck:unload(),
    ok.

before_each() ->
    ok = wocky_db_user:create_user(?USER, ?DOMAIN, ?NAME,
                                   encode_password(?PASS)),
    ok.

after_each(_) ->
    {ok, _} = wocky_db:query(shared, <<"TRUNCATE username_to_user">>, quorum),
    {ok, _} = wocky_db:query(?DOMAIN, <<"TRUNCATE user">>, quorum),
    ok.


test_store_type_with_scram() ->
  { "store_type/1 with scram",
    foreach, fun before_each/0, fun after_each/1, [
    { "returns 'scram'", [
      ?_assertEqual(scram, ejabberd_auth_wocky:store_type(?DOMAIN))
    ]}
  ]}.

test_store_type_without_scram() ->
  { "store_type/1 without scram",
    foreach, fun before_each/0, fun after_each/1, [
    { "returns 'plain'", [
      ?_assertEqual(plain, ejabberd_auth_wocky:store_type(?DOMAIN))
    ]}
  ]}.

test_check_password_with_options() ->
  DigestGen = fun(X) -> X end,
  { "check_password/5", foreach, fun before_each/0, fun after_each/1, [
    { "returns true when user exists and password digest matches", [
      ?_assert(ejabberd_auth_wocky:check_password(?USER,
                                                  ?DOMAIN,
                                                  ?PASS,
                                                  mk_digest(?USER, ?PASS),
                                                  DigestGen))
    ]},
    { "returns false when user exists but password digest does not match", [
      ?_assertNot(ejabberd_auth_wocky:check_password(?USER,
                                                     ?DOMAIN,
                                                     <<"niemakota">>,
                                                     <<"readers">>,
                                                     DigestGen))
    ]},
    { "returns false when user does not exist", [
      ?_assertNot(ejabberd_auth_wocky:check_password(?BADUSER,
                                                     ?DOMAIN,
                                                     ?PASS,
                                                     <<"readers">>,
                                                     DigestGen))
    ]}
  ]}.

test_check_password() ->
  { "check_password/3", foreach, fun before_each/0, fun after_each/1, [
    { "returns true when user exists and password matches", [
      ?_assert(ejabberd_auth_wocky:check_password(?USER, ?DOMAIN, ?PASS))
    ]},
    { "returns false when user exists but password does not match", [
      ?_assertNot(ejabberd_auth_wocky:check_password(
                    ?USER, ?DOMAIN, <<"niemakota">>))
    ]},
    { "returns false when user does not exist", [
      ?_assertNot(ejabberd_auth_wocky:check_password(?BADUSER, ?DOMAIN, ?PASS))
    ]}
  ]}.

test_set_password() ->
  { "set_password/3", foreach, fun before_each/0, fun after_each/1, [
    { "changes the user's password", [
      ?_assertEqual(ok, ejabberd_auth_wocky:set_password(
                          ?USER, ?DOMAIN, <<"mialakota">>)),
      ?_assert(ejabberd_auth_wocky:check_password(
                 ?USER, ?DOMAIN, <<"mialakota">>))
    ]},
    { "returns {error, invalid_jid} if the user doesn't exist", [
      ?_assertEqual({error, invalid_jid},
                    ejabberd_auth_wocky:set_password(
                      ?BADUSER, ?DOMAIN, <<"ticktock">>))
    ]}
  ]}.

test_get_password_with_default() ->
  { "get_password/3", foreach, fun before_each/0, fun after_each/1, [
    { "returns the same value as get_password/2", [
      ?_assertEqual(ejabberd_auth_wocky:get_password(?USER, ?DOMAIN),
                    ejabberd_auth_wocky:get_password(?USER, ?DOMAIN, [])),
      ?_assertEqual(ejabberd_auth_wocky:get_password(?BADUSER, ?DOMAIN),
                    ejabberd_auth_wocky:get_password(?BADUSER, ?DOMAIN, []))
    ]}
  ]}.

test_get_password_with_scram() ->
  { "get_password/2 with scram",
    foreach, fun before_each/0, fun after_each/1, [
    { "returns a tuple with scram data when the user exists", [
      ?_assertMatch({_, _, _, 4096},
                    ejabberd_auth_wocky:get_password(?USER, ?DOMAIN))
    ]},
    { "returns false when the user doesn't exist", [
      ?_assertEqual(false, ejabberd_auth_wocky:get_password(?BADUSER, ?DOMAIN))
    ]}
  ]}.

test_get_password_without_scram() ->
  { "get_password/2 without scram",
    foreach, fun before_each/0, fun after_each/1, [
    { "returns the stored password", [
      ?_assertMatch(?PASS, ejabberd_auth_wocky:get_password(?USER, ?DOMAIN))
    ]},
    { "returns false when the user doesn't exist", [
      ?_assertEqual(false, ejabberd_auth_wocky:get_password(?BADUSER, ?DOMAIN))
    ]}
  ]}.

test_get_password_s_with_scram() ->
  { "get_password_s/2 with scram",
    foreach, fun before_each/0, fun after_each/1, [
    { "returns an empty binary regardless of whether the user exists", [
      ?_assertEqual(<<>>, ejabberd_auth_wocky:get_password_s(?USER, ?DOMAIN)),
      ?_assertEqual(<<>>, ejabberd_auth_wocky:get_password_s(?BADUSER, ?DOMAIN))
    ]}
  ]}.

test_get_password_s_without_scram() ->
  { "get_password_s/2 without scram",
    foreach, fun before_each/0, fun after_each/1, [
    { "returns the stored password if the user exists", [
      ?_assertEqual(?PASS, ejabberd_auth_wocky:get_password_s(?USER, ?DOMAIN))
    ]},
    { "returns an empty binary if the user does not exist", [
      ?_assertEqual(<<>>, ejabberd_auth_wocky:get_password_s(?BADUSER, ?DOMAIN))
    ]}
  ]}.

test_does_user_exist() ->
  { "does_user_exist/2", foreach, fun before_each/0, fun after_each/1, [
    { "returns true if the user exists", [
      ?_assert(ejabberd_auth_wocky:does_user_exist(?USER, ?DOMAIN))
    ]},
    { "returns false if the user does not exist", [
      ?_assertNot(ejabberd_auth_wocky:does_user_exist(?BADUSER, ?DOMAIN))
    ]}
  ]}.

test_remove_user() ->
  { "remove_user/2", foreach, fun before_each/0, fun after_each/1, [
    { "removes the user if the user exists", [
      ?_assertEqual(ok, ejabberd_auth_wocky:remove_user(?USER, ?DOMAIN)),
      ?_assertNot(ejabberd_auth_wocky:does_user_exist(?USER, ?DOMAIN))
    ]},
    { "returns ok if the user doesn't exist", [
      ?_assertEqual(ok, ejabberd_auth_wocky:remove_user(?BADUSER, ?DOMAIN))
    ]}
  ]}.

test_remove_user_with_password() ->
  { "remove_user/3", foreach, fun before_each/0, fun after_each/1, [
    { "removes the user if the user exists and the password matches", [
      ?_assertEqual(ok, ejabberd_auth_wocky:remove_user(?USER, ?DOMAIN, ?PASS)),
      ?_assertNot(ejabberd_auth_wocky:does_user_exist(?USER, ?DOMAIN))
    ]},
    { "returns {error, not_allowed} if the password does not match", [
      ?_assertEqual({error, not_allowed},
                    ejabberd_auth_wocky:remove_user(
                      ?USER, ?DOMAIN, <<"niemakota">>)),
      ?_assert(ejabberd_auth_wocky:does_user_exist(?USER, ?DOMAIN))
    ]},
    { "returns {error, not_exists} if the user doesn't exist", [
      ?_assertEqual({error, not_exists},
                    ejabberd_auth_wocky:remove_user(
                      ?BADUSER, ?DOMAIN, <<"ticktock">>))
    ]}
  ]}.

test_try_register() ->
  { "try_register/3", foreach, fun before_each/0, fun after_each/1, [
    { "creates the user if it does not already exist", [
      ?_assertEqual(ok, ejabberd_auth_wocky:try_register(
                          <<"madhatter">>, ?DOMAIN, <<"ticktock">>))
    ]},
    { "returns {error, exists} if the user already exists", [
      ?_assertEqual({error, exists},
                    ejabberd_auth_wocky:try_register(?NAME, ?DOMAIN, ?PASS))
    ]}
  ]}.
