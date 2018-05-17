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

-define(firebase, 'Elixir.Wocky.Account.Firebase').
-define(PHONE, <<"+15551234567">>).

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
     login_with_non_string_provider,
     login_with_missing_provider,
     login_with_invalid_provider,
     login_with_digits,
     login_with_digits_bypass,
     login_with_firebase,
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

    User = ?wocky_factory:insert(user, #{welcome_sent => true}),

    escalus:init_per_suite([{user, User} | Config]).

end_per_suite(Config) ->
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
    User = ?wocky_factory:build(user),
    Data = provider_data(firebase, User),
    Stanza = request_stanza(request_data(firebase, Data)),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_redirect(Result, true, true).

register_with_digits_bypass(Config) ->
    Client = start_client(Config),
    User = ?wocky_factory:build(user),
    Stanza = request_stanza(request_data(digits, provider_data(digits, User))),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_redirect(Result, true, true).


%%--------------------------------------------------------------------
%% existing user tests
%%--------------------------------------------------------------------

login_with_invalid_json(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(<<"Not at all valid JSON">>),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_malformed_error(Result).

login_with_non_string_provider(Config) ->
    Client = start_client(Config),
    User = proplists:get_value(user, Config),
    Data = request_data(firebase, provider_data(firebase, User)),
    BrokenData = [{provider, 1000000} | proplists:delete(provider, Data)],
    BrokenStanza = request_stanza(BrokenData),
    Result = escalus:send_and_wait(Client, BrokenStanza),
    ct:pal("~p", [Result]),
    assert_is_malformed_error(Result).

login_with_missing_provider(Config) ->
    Client = start_client(Config),
    User = proplists:get_value(user, Config),
    Data = request_data(firebase, provider_data(firebase, User)),
    BrokenData = proplists:delete(provider, Data),
    BrokenStanza = request_stanza(BrokenData),
    Result = escalus:send_and_wait(Client, BrokenStanza),
    assert_is_malformed_error(Result).

login_with_invalid_provider(Config) ->
    Client = start_client(Config),
    User = proplists:get_value(user, Config),
    Data = request_data(firebase, provider_data(firebase, User)),
    BrokenData = [{provider, <<"NOTFirebase">>} |
                  proplists:delete(provider, Data)],
    BrokenStanza = request_stanza(BrokenData),
    Result = escalus:send_and_wait(Client, BrokenStanza),
    assert_is_not_authorized(Result).

login_with_digits(Config) ->
    Client = start_client(Config),
    User0 = proplists:get_value(user, Config),
    User = maps:put(phone_number, <<"+12105551234">>, User0),
    Stanza = request_stanza(request_data(digits, provider_data(digits, User))),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_not_authorized(Result).

login_with_digits_bypass(Config) ->
    Client = start_client(Config),
    User = proplists:get_value(user, Config),
    Stanza = request_stanza(request_data(digits, provider_data(digits, User))),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_redirect(Result, false, true).

login_with_firebase(Config) ->
    Client = start_client(Config),
    User = proplists:get_value(user, Config),
    Data = provider_data(firebase, User),
    Stanza = request_stanza(request_data(firebase, Data)),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_redirect(Result, false, true).

login_with_firebase_no_token(Config) ->
    Client = start_client(Config),
    User = proplists:get_value(user, Config),
    Data = request_data(firebase, provider_data(firebase, User)),
    NoTokenData = [{token, false} |
                   proplists:delete(token, Data)],
    NoTokenStanza = request_stanza(NoTokenData),
    Result = escalus:send_and_wait(Client, NoTokenStanza),
    assert_is_redirect(Result, false, false).

login_with_firebase_invalid_provider_data(Config) ->
    Client = start_client(Config),
    User = proplists:get_value(user, Config),
    Data = request_data(firebase, provider_data(firebase, User)),
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

provider_data(firebase_bad_jwt) ->
    [{jwt, <<"ThisIsNotAValidJWT">>}].

provider_data(digits, #{external_id := EID, phone_number := Phone}) ->
    [{userID, EID},
     {phoneNumber, Phone},
     {authTokenSecret, <<"vViH56F2f1sNi6RYZZeztDo8NoQMWxhGMDKAL0wCFcIUH">>},
     {authToken, <<"701990807448920064-JxNX4i57y5Wp6xBDVjNwKB4ZYUcC8FK">>},
     {'X-Auth-Service-Provider', <<"http://doesnt.really.matter/">>},
     {'X-Verify-Credentials-Authorization',
      <<"OAuth oauth_signature=\"%2FeT%2FOC%2F78Rij8QPEd3ghy%2FUOIbI%3D\","
        "oauth_nonce=\"944F1D89-161C-47E4-8730-41BD43BB164F\","
        "oauth_timestamp=\"1456701445\","
        "oauth_consumer_key=\"e527IQiWSXZ5WHNxROUZk87uV\","
        "oauth_token=\"701990807448920064-JxNX4i57y5Wp6xBDVjNwKB4ZYUcC8FK\","
        "oauth_version=\"1.0\","
        "oauth_signature_method=\"HMAC-SHA1\"">>
     }];

provider_data(firebase, User) ->
    {ok, JWT, _} = ?firebase:encode_and_sign(User),
    [{jwt, JWT}].

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
