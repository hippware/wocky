%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for extensions to the SASL PLAIN mechanism
%%% including `mod_wocky_token`
-module(sasl_plain_SUITE).

-compile(export_all).
-compile({parse_transform, fun_chain}).
-compile({parse_transform, cut}).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").

-define(key_manager, 'Elixir.Wocky.Account.FirebaseKeyManager').
-define(joken, 'Elixir.Joken').
-define(jwk, 'Elixir.JOSE.JWK').
-define(key_id, <<"c947c408c8dd053f7e13117c4e00f0b2b16dc789">>).
-define(iss, <<"https://securetoken.google.com/">>).
-define(PREPOP_USER, <<"__new_user_hs_archive__">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, new},
     {group, existing},
     {group, token}
    ].

groups() ->
    [
     {new, [], new_cases()},
     {existing, [], existing_cases()},
     {token, [], token_cases()}
    ].

new_cases() ->
    [
     register_with_firebase,
     register_with_digits_bypass
    ].

existing_cases() ->
    [
     login_with_invalid_json,
     login_with_missing_provider,
     login_with_invalid_provider,
     login_with_digits,
     login_with_digits_bypass,
     login_with_firebase,
     login_with_firebase_no_changes,
     login_with_firebase_no_token,
     login_with_firebase_invalid_provider_data,
     login_with_firebase_bad_jwt
    ].

token_cases() ->
    [
     acquire_token,
     release_token,
     login_with_token,
     login_with_bad_token
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    ?wocky_repo:delete_all(?wocky_user),

    InitialContacts = [setup_initial_contacts(T) ||
                       T <- [followee, follower, friend]],

    PrepopItems = setup_hs_prepop(),

    meck:new(?key_manager, [passthrough, no_link]),
    meck:expect(?key_manager, get_key,
                fun(?key_id) -> {ok, cert()};
                   (_) -> {error, no_key}
                end),
    Config1 = [{initial_contacts, InitialContacts},
               {prepop_items, PrepopItems}
               | Config],
    escalus:init_per_suite(Config1).

setup_initial_contacts(Type) ->
    Users = ?wocky_factory:insert_list(3, user),
    lists:foreach(
      fun(#{id := ID}) ->
              ?wocky_factory:insert(initial_contact, [{user_id, ID},
                                                      {type, Type}])
      end,
      Users),
    Users.

setup_hs_prepop() ->
    #{id := UserID} = test_helper:insert_system_users(),

    OldTS = ?timex:subtract(?datetime:utc_now(), ?duration:from_weeks(6)),

    ?wocky_factory:insert_list(15, home_stream_item, #{user_id => UserID,
                                                       created_at => OldTS,
                                                       updated_at => OldTS}),
    ?wocky_factory:insert_list(15, home_stream_item, #{user_id => UserID}).

end_per_suite(Config) ->
    meck:unload(),
    escalus:end_per_suite(Config).

init_per_group(token, Config) ->
    test_helper:setup_users(Config, [alice]);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% new user tests
%%--------------------------------------------------------------------

register_with_firebase(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(request_data(firebase, provider_data(firebase))),
    register_common(Config, Client, Stanza),
    ?assert(meck:validate(?key_manager)).

register_with_digits_bypass(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(request_data(digits, provider_data(digits_bypass))),
    register_common(Config, Client, Stanza).

register_common(Config, Client, Stanza) ->
    Result = escalus:send_and_wait(Client, Stanza),
    {UserID, Token} = assert_is_redirect(Result, true, true),
    NewConfig = [{escalus_users,
                  [{thedude,
                    [{username, UserID},
                     {server, ?SERVER},
                     {password, Token}]}]}
                 | Config],

    escalus:story(NewConfig, [{thedude, 1}], fun(Alice) ->
        %% Verify that initial followees have been added
        InitialContacts
        = [InitialFollowees, InitialFollowers, InitialFriends]
        = proplists:get_value(initial_contacts, NewConfig),
        JIDs = lists:map(fun(#{id := ID}) ->
                                 jid:to_binary(jid:make(ID, ?SERVER, <<>>))
                         end, lists:flatten(InitialContacts)),

        escalus:send(Alice, escalus_stanza:roster_get()),
        Stanza2 = escalus:wait_for_stanza(Alice),
        escalus_assert:is_roster_result(Stanza2),

        escalus:assert(count_roster_items, [length(JIDs)], Stanza2),
        lists:foreach(fun(JID) ->
                              escalus:assert(roster_contains, [JID], Stanza2)
                      end, JIDs),
        lists:foreach(
          fun({Users, SubType}) ->
                  lists:foreach(check_contact(_, SubType, Stanza2), Users)
          end,
          [{InitialFollowees, <<"to">>},
           {InitialFollowers, <<"from">>},
           {InitialFriends, <<"both">>}]),

        %% Verify that initial HS items have been added
        Stanza3 = test_helper:expect_iq_success_u(
                    test_helper:get_hs_stanza(), Alice, Alice),
        test_helper:check_hs_result(Stanza3, 15)
    end).


%%--------------------------------------------------------------------
%% existing user tests
%%--------------------------------------------------------------------

login_with_invalid_json(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(<<"Not at all valid JSON">>),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_malformed_error(Result).

login_with_missing_provider(Config) ->
    Client = start_client(Config),
    Data = request_data(firebase, provider_data(firebase)),
    BrokenData = proplists:delete(provider, Data),
    BrokenStanza = request_stanza(BrokenData),
    Result = escalus:send_and_wait(Client, BrokenStanza),
    assert_is_malformed_error(Result).

login_with_invalid_provider(Config) ->
    Client = start_client(Config),
    Data = request_data(firebase, provider_data(firebase)),
    BrokenData = [{provider, <<"NOTFirebase">>} |
                  proplists:delete(provider, Data)],
    BrokenStanza = request_stanza(BrokenData),
    Result = escalus:send_and_wait(Client, BrokenStanza),
    assert_is_not_authorized(Result).

login_with_digits(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(request_data(digits, provider_data(digits))),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_not_authorized(Result).

login_with_digits_bypass(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(request_data(digits, provider_data(digits_bypass))),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_redirect(Result, false, true).

login_with_firebase(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(request_data(firebase, provider_data(firebase))),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_redirect(Result, false, true).

login_with_firebase_no_changes(Config) ->
    login_with_firebase(Config).

login_with_firebase_no_token(Config) ->
    Client = start_client(Config),
    Data = request_data(firebase, provider_data(firebase)),
    NoTokenData = [{token, false} |
                   proplists:delete(token, Data)],
    NoTokenStanza = request_stanza(NoTokenData),
    Result = escalus:send_and_wait(Client, NoTokenStanza),
    assert_is_redirect(Result, false, false).

login_with_firebase_invalid_provider_data(Config) ->
    Client = start_client(Config),
    Data = request_data(firebase, provider_data(firebase)),
    BrokenData = [{provider_data, <<"NOTFirebaseData">>} |
                  proplists:delete(provider_data, Data)],
    BrokenStanza = request_stanza(BrokenData),
    Result = escalus:send_and_wait(Client, BrokenStanza),
    assert_is_malformed_error(Result).

login_with_firebase_bad_jwt(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(
               request_data(firebase, provider_data(firebase_bad_jwt))),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_not_authorized(Result).


%%--------------------------------------------------------------------
%% token management tests
%%--------------------------------------------------------------------

acquire_token(Config) ->
    escalus:story(Config, [{alice, 1}], fun (Alice) ->
        Reply = test_helper:expect_iq_success_u(token_stanza(<<"get">>), Alice),
        <<"$T$", _/binary>> =
            exml_query:path(Reply, [{element, <<"query">>}, cdata])
    end).

release_token(Config) ->
    escalus:story(Config, [{alice, 1}], fun (Alice) ->
        test_helper:expect_iq_success_u(token_stanza(<<"set">>), Alice)
    end).

login_with_token(Config) ->
    {ok, {Token, _}} = escalus_ejabberd:rpc(?wocky_account, assign_token,
                                            [?ALICE, <<"res1">>]),
    Config2 = escalus_users:update_userspec(Config, alice, password, Token),
    escalus:story(Config2, [{alice, 1}], fun (Alice) ->
        escalus_client:send(Alice, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>],
                       escalus_client:wait_for_stanza(Alice))
    end).

%% Test that our authorisation system is returning the correct
%% failure message
login_with_bad_token(Config) ->
    UserSpec = escalus_users:get_userspec(Config, alice),
    ConnSteps = [start_stream, stream_features],
    {ok, ClientConnection, _Features} =
        escalus_connection:start(UserSpec, ConnSteps),

    try
        escalus_auth:auth_plain(ClientConnection, [{username, ?ALICE},
                                                   {password, <<"$T$blah">>}]),
        ct:fail("Auth should not have succeeded")
    catch throw:E ->
        ?assertMatch({auth_failed, ?ALICE,
                      #xmlel{name = <<"failure">>,
                             attrs = [{<<"xmlns">>, ?NS_SASL}],
                             children = [#xmlel{name = <<"not-authorized">>}]}},
                     E)
    end.


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_client(Config) ->
    AliceSpec = escalus_users:get_options(Config, alice),
    {ok, Client, _} = escalus_connection:start(AliceSpec,
                                               [start_stream,
                                                stream_features]),
    Client.

assert_is_malformed_error(Result) ->
    assert_is_failure(Result, <<"malformed-request">>).

assert_is_not_authorized(Result) ->
    assert_is_failure(Result, <<"not-authorized">>).

assert_is_temp_failure(Result) ->
    assert_is_failure(Result, <<"temporary-auth-failure">>).

assert_is_failure(#xmlel{name = Name, children = Children}, Type) ->
    ?assertEqual(<<"failure">>, Name),
    ?assertNotEqual(false, lists:keyfind(Type, #xmlel.name, Children)).

assert_is_redirect(#xmlel{name = Name, children = Children}, IsNew, HasToken) ->
    ?assertEqual(<<"failure">>, Name),
    assert_has_redirect_children(Children, IsNew, HasToken).

assert_has_redirect_children(Children, IsNew, HasToken) ->
    ?assert(lists:keymember(<<"redirect">>, #xmlel.name, Children)),
    Text = lists:keyfind(<<"text">>, #xmlel.name, Children),
    ?assertNotEqual(false, Text),
    [#xmlcdata{content = JSON}] = Text#xmlel.children,
    assert_has_redirect_data(JSON, IsNew, HasToken).

assert_has_redirect_data(JSON, IsNew, HasToken) ->
    {struct, Fields} = mochijson2:decode(JSON),
    RequiredFields = [<<"user">>, <<"server">>, <<"handle">>,
                      <<"provider">>, <<"external_id">>, <<"is_new">>],
    lists:foreach(fun(F) -> ?assert(proplists:is_defined(F, Fields)) end,
                  RequiredFields),
    ?assertEqual(IsNew, proplists:get_value(<<"is_new">>, Fields)),
    {proplists:get_value(<<"user">>, Fields), check_token(Fields, HasToken)}.

check_contact(#{id := ID}, SubType, Stanza2) ->
    Query = exml_query:subelement(Stanza2, <<"query">>),
    JID = jid:to_binary(jid:make(ID, ?SERVER, <<>>)),
    Item = lists:filter(
             fun(E) ->
                     xml:get_attr(<<"jid">>, E#xmlel.attrs)
                         =:= {value, JID}
             end,
             Query#xmlel.children),
    case Item of
        [I] ->
            ?assertEqual({value, SubType},
                         xml:get_attr(<<"subscription">>, I#xmlel.attrs)),
            ?assert(lists:member(
                      <<"__new__">>,
                      exml_query:paths(I, [{element, <<"group">>}, cdata])));
        X ->
            ct:fail("Could not find item or __new__ group for jid ~p (~p)",
                    [JID, X])
    end.

check_token(Fields, HasToken) ->
    lists:foreach(fun(F) ->
                          ?assertEqual(HasToken,
                                       proplists:is_defined(F, Fields))
                  end,
                  [<<"token">>, <<"token_expiry">>]),
    proplists:get_value(<<"token">>, Fields).

token_stanza(Type) ->
    escalus_stanza:iq(Type, [escalus_stanza:query_el(?NS_TOKEN, [])]).

request_data(Provider, ProviderData) ->
    [{provider, atom_to_binary(Provider, utf8)},
     {resource, <<"test_resource">>},
     {token, true},
     {provider_data, {struct, ProviderData}}].

provider_data(digits) ->
    [{authTokenSecret, <<"vViH56F2f1sNi6RYZZeztDo8NoQMWxhGMDKAL0wCFcIUH">>},
     {authToken, <<"701990807448920064-JxNX4i57y5Wp6xBDVjNwKB4ZYUcC8FK">>},
     {'X-Auth-Service-Provider', <<"http://doesnt.really.matter/">>},
     {'X-Verify-Credentials-Authorization',
      <<"OAuth oauth_signature=\"%2FeT%2FOC%2F78Rij8QPEd3ghy%2FUOIbI%3D\""
        ",oauth_nonce=\"944F1D89-161C-47E4-8730-41BD43BB164F\","
        "oauth_timestamp=\"1456701445\",oauth_consumer_key="
        "\"e527IQiWSXZ5WHNxROUZk87uV\",oauth_token="
        "\"701990807448920064-JxNX4i57y5Wp6xBDVjNwKB4ZYUcC8FK\""
        ",oauth_version=\"1.0\",oauth_signature_method=\"HMAC-SHA1\"">>
     }];

provider_data(digits_bypass) ->
    [{userID, ?EXTERNAL_ID}, {phoneNumber, <<"+15551234567">>} |
     provider_data(digits)];

provider_data(firebase) ->
    Project = ?confex:get_env(wocky, firebase_project_id),

    [{jwt,
      fun_chain:first(
        ?joken:token(),
        ?joken:with_header_arg(<<"kid">>, ?key_id),
        ?joken:with_exp(os:system_time(seconds) + 3600),
        ?joken:with_iat(os:system_time(seconds)),
        ?joken:with_aud(Project),
        ?joken:with_iss(<<?iss/binary, Project/binary>>),
        ?joken:with_sub(integer_to_binary(rand:uniform(10000))),
        ?joken:with_claim(<<"phone_number">>, <<"+15551231234">>),
        ?joken:with_signer(?joken:rs256(?jwk:from_pem(private_key()))),
        ?joken:sign(),
        ?joken:get_compact()
       )
     }];

provider_data(firebase_bad_jwt) ->
    [{jwt,
      fun_chain:first(
        ?joken:token(),
        ?joken:with_header_arg(<<"kid">>, ?key_id),
        ?joken:with_signer(?joken:rs256(?jwk:from_pem(private_key()))),
        ?joken:sign(),
        ?joken:get_compact()
       )
     }].

request_stanza(Data) when is_list(Data) ->
    BinData = iolist_to_binary(mochijson2:encode({struct, Data})),
    request_stanza(BinData);

request_stanza(BinData) when is_binary(BinData) ->
    Payload = <<"gibberish", 0:8, "register", 0:8, "$J$", BinData/binary>>,
    Stanza = escalus_stanza:auth(<<"PLAIN">>, [base64_cdata(Payload)]),
    ct:log("Auth Stanza: ~p", [Stanza]),
    Stanza.

base64_cdata(Payload) ->
    #xmlcdata{content = base64:encode(Payload)}.

private_key() ->
<<"
-----BEGIN PRIVATE KEY-----
MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC3bQRyIwh5Ybw0
Bd1kNeWh57qSpUKa867rPS3Uwqd42LGRDmC0ZJNBB3mN6J7WNaAO+kwJ1HpGLkQ5
OtQcKzBmKppr19zm6fAFOFs59fDMHY5aBMYfGaEdqnpGv7+bjbFDEctSwXiOzQ++
br8871KUPXNE4/8Hjel+HYQO7rSWHuMq3hUBsprfbAUKFoz1VaD7HLBZxg4iEA+p
jGZ9ykfEfQtIYZSNGjNmZP/XmEzGYF+fGuKgigjiZ6Vvd703r30sFbjUhaaImIOX
Yk3bt0RYYxBx0CROI+x1aTsEcezQSwKsVeEp/BpemE6MomgEph8rNF3NSP73S3Wu
QP0jCX3dAgMBAAECggEAad+fJVJbXdSwEUchVupVNXLQGj0RiOcHG/kgLyJ8ECDj
vVqTLwyugmaSHvsaU4J4dKy8nx/pxACImJAARpIXSaFlqMHcW9zEEf9JiNcQuoCE
3ijLQsBEYx83nQaozlym6JOozIen0qVCZST/dWiePbqKgkUnu3CKSaU3yHa3/b2z
eM1tZ/0aCxxTiRn/E0snBBGWMs3XuFcp/nLgw3Nbd1XG6skgOegxqfmCtdNyywzl
PksfbLepk73axPUbTkkz8WFUOnfrTED4cGo++3VrfaMBLjM1Sm4jiQr9FIB3NmfF
j4b/Ui3ZNoG1xbjK0xDsuKsl6tLGzNVaFk23PUwkAQKBgQDpyjkrQLlU0FLxg8AS
k/0lwV7JjMbUQadDr49kQDGOkz/ljlZgStv8jQFsGYpvNIKZi5OXWxNPB3mWQwU3
z7dF7e47gXCMzvmIrdr1Gs2YPxTYjv9wB3om2D6mLyBs/4yCaGo8hi0SPVYZi2iC
uQLv3g2UTOKaX9rCmQ0jDQetHQKBgQDI2fA04+drvSQvIFrqk4Skz2g8sN7+7j/n
yx/aYMJyCNCpvih5L2014xXosTVbNhn5pmFUlZ+4qwwQEzElQgqN2VZdHNxE0bYe
xTyjSuKkJY0jAcfUJz0neiXbWpKvfj3yQupFNeWjpOE2tsstFfWkH89xL6w/1VIs
CI/u6M33wQKBgEyHevFSrZg63Xvboesy8GIEi4+0en2OxD8e3/R0IwTF5NuzHUlG
F/7y9W06axt99+ZlTznzgT2Ud9OdOr8LSrYkbaCi/YHKWtrH9m3XiUd2Fs/Q94Ln
n6/Jh7CEqrujZ45kuan4ThazZ1TTUrG/+FsmuBE8nczk5cpfqXI42LNtAoGBAJ1/
gMQIvs0WWUx3I7P0j7wpRATrcUIZE5WxC75Tx8ZiMTYZ/mThEtOByglZBI0MxJum
o4YPelr2DhSA6DXeLqaC+h0z52ozxIsmgWFO9KBhLeZ4m/k599OADjWPNZ1V8j+J
x2kUVYnYXh5ogrRNFv1nUGTiTEEWB0SuRifC+NhBAoGAcXiz/l+vO1xa0+qpWFO/
ZomXwqs+Do40vXqWQg7C7fJ3C3Wgf1RO5MRncVpcawqPkN9jvskFtA18B4lXN6f+
MRcUPLMy+tbWNleZ/KktLk5GdGeY8GaScFb82EwaWb2NIs1MF/ONyGF5sTZvToK7
hHku1vi9fpU/eNt0FXgecd8=
-----END PRIVATE KEY-----
">>.

cert() ->
<<"
-----BEGIN CERTIFICATE-----
MIIDBjCCAe4CCQCgEOA/+QkMRTANBgkqhkiG9w0BAQsFADBFMQswCQYDVQQGEwJB
VTETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UECgwYSW50ZXJuZXQgV2lkZ2l0
cyBQdHkgTHRkMB4XDTE3MDgyMjA3NDE1OFoXDTQ1MDEwNjA3NDE1OFowRTELMAkG
A1UEBhMCQVUxEzARBgNVBAgMClNvbWUtU3RhdGUxITAfBgNVBAoMGEludGVybmV0
IFdpZGdpdHMgUHR5IEx0ZDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB
ALdtBHIjCHlhvDQF3WQ15aHnupKlQprzrus9LdTCp3jYsZEOYLRkk0EHeY3ontY1
oA76TAnUekYuRDk61BwrMGYqmmvX3Obp8AU4Wzn18MwdjloExh8ZoR2qeka/v5uN
sUMRy1LBeI7ND75uvzzvUpQ9c0Tj/weN6X4dhA7utJYe4yreFQGymt9sBQoWjPVV
oPscsFnGDiIQD6mMZn3KR8R9C0hhlI0aM2Zk/9eYTMZgX58a4qCKCOJnpW93vTev
fSwVuNSFpoiYg5diTdu3RFhjEHHQJE4j7HVpOwRx7NBLAqxV4Sn8Gl6YToyiaASm
Hys0Xc1I/vdLda5A/SMJfd0CAwEAATANBgkqhkiG9w0BAQsFAAOCAQEAX1k2rWvP
GJSiLoQuenA3iOT0BmRZYQSG/0uHMFaJOb42Xb3V0F35sjyYDGqH/yDI8/MlA0mq
/BzkOG+5yQM2F3EjqAnFvO7eFfRHxU0E9xuiweFIx615sYTID6xvvbFFtZ1xD1YV
EV3G88wSk1g6NYz2BfzpY089JrRNvLApNUk7ssemLOY/FMu+1bI6TNxgn1MU6zHK
MlV3DJZAUdOZOyx77p4QQVH0BaPWSkNsmUXNQu/8aNbNktuiQN65+ByBPvq6W64u
oVwftZpq3axVJyOZ9sfISPLUPvsHvK5r24kaSxFKknKO7X8Jb1FUGJ0UidgBi1pf
hYWUqOAgiBY3RQ==
-----END CERTIFICATE-----
">>.
