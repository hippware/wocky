%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_user
-module(xmpp_reg_SUITE).
-compile(export_all).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky_db_seed.hrl").
-include("wocky.hrl").

%TODO: Remove me
-define(ALICE_UUID, escalus_users:get_username(Config, alice)).
-define(BOB_UUID, escalus_users:get_username(Config, bob)).

%-import(test_helper, [expect_iq_success/2, expect_iq_error/2]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, new},
     {group, existing},
     {group, no_digits}
    ].

groups() ->
    [{new, [], new_cases()},
     {existing, [], existing_cases()},
     {no_digits, [], no_digits_cases()}
    ].

new_cases() ->
    [
     new_user,
     invalid_json,
     missing_field,
     unauthorized_new,
     invalid_auth_provider,
     invalid_provider_data
    ].

existing_cases() ->
    [
     bypass_prefix,
     update,
     update_no_changes,
     no_token,
     unauthorized_update,
     empty_phone_number
    ].

no_digits_cases() ->
    [
     digits_unavailable
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    ?wocky_repo:delete_all(?wocky_user),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(no_digits, Config) ->
    Config;
init_per_group(_GroupName, Config) ->
    fake_digits_server:start(true, ?PHONE_NUMBER),
    Config.

end_per_group(no_digits, Config) ->
    Config;
end_per_group(_GroupName, Config) ->
    fake_digits_server:stop(),
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% mod_wocky_reg new user tests
%%--------------------------------------------------------------------

new_user(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(request_data(provider_data())),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_redirect(Result, true, true).

invalid_json(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(<<"Not at all valid JSON">>),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_malformed_error(Result).

missing_field(Config) ->
    Client = start_client(Config),
    Data = request_data(provider_data()),
    BrokenData = proplists:delete(provider, Data),
    BrokenStanza = request_stanza(BrokenData),
    Result = escalus:send_and_wait(Client, BrokenStanza),
    assert_is_malformed_error(Result).

unauthorized_new(Config) ->
    fake_digits_server:stop(),
    fake_digits_server:start(false, ?PHONE_NUMBER),
    Client = start_client(Config),
    Stanza = request_stanza(request_data(provider_data())),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_not_authorized(Result).

invalid_auth_provider(Config) ->
    Client = start_client(Config),
    Data = request_data(provider_data()),
    BrokenData = [{provider, <<"NOTDigits">>} |
                   proplists:delete(provider, Data)],
    BrokenStanza = request_stanza(BrokenData),
    Result = escalus:send_and_wait(Client, BrokenStanza),
    assert_is_not_authorized(Result).

invalid_provider_data(Config) ->
    Client = start_client(Config),
    Data = request_data(provider_data()),
    BrokenData = [{provider_data, <<"NOTDigitsData">>} |
                   proplists:delete(provider_data, Data)],
    BrokenStanza = request_stanza(BrokenData),
    Result = escalus:send_and_wait(Client, BrokenStanza),
    assert_is_malformed_error(Result).

%%--------------------------------------------------------------------
%% mod_wocky_reg existing user tests
%%--------------------------------------------------------------------

bypass_prefix(Config) ->
    Client = start_client(Config),
    ProviderData = provider_data(),
    BypassProviderData = [{userID, ?EXTERNAL_ID},
                          {phoneNumber, <<"+155566854">>} |
                          proplists:delete(phoneNumber, ProviderData)],
    Stanza = request_stanza(request_data(BypassProviderData)),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_redirect(Result, false, true).

update(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(request_data(provider_data())),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_redirect(Result, false, true).

update_no_changes(Config) ->
    update(Config).

no_token(Config) ->
    Client = start_client(Config),

    Data = request_data(provider_data()),
    NoTokenData = [{token, false} |
                   proplists:delete(token, Data)],
    NoTokenStanza = request_stanza(NoTokenData),
    Result = escalus:send_and_wait(Client, NoTokenStanza),
    assert_is_redirect(Result, false, false).

unauthorized_update(Config) ->
    % Same test as new once the user exists
    unauthorized_new(Config).

empty_phone_number(Config) ->
    fake_digits_server:stop(),
    fake_digits_server:start(true, <<>>),
    Client = start_client(Config),
    Stanza = request_stanza(request_data(provider_data())),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_redirect(Result, false, true).

digits_unavailable(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(request_data(provider_data())),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_temp_failure(Result).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_client(Config) ->
    AliceSpec = escalus_users:get_options(Config, alice),
    {ok, Client, _, _} = escalus_connection:start(AliceSpec,
                                                  [start_stream,
                                                   stream_features]),
    Client.

assert_is_malformed_error(Result) ->
    assert_is_failure(Result, <<"malformed-request">>).

assert_is_not_authorized(Result) ->
    assert_is_failure(Result, <<"not-authorized">>).

assert_is_temp_failure(Result) ->
    assert_is_failure(Result, <<"temporary-auth-failure">>).

assert_is_failure(Result = #xmlel{name = Name, children = Children}, Type) ->
    ct:log("Result ~p", [Result]),
    ?assertEqual(<<"failure">>, Name),
    ?assertNotEqual(false, lists:keyfind(Type, #xmlel.name, Children)).

assert_is_redirect(Result = #xmlel{name = Name, children = Children},
                   IsNew, HasToken) ->
    ct:log("Result ~p", [Result]),
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
    check_token(Fields, HasToken).

check_token(Fields, HasToken) ->
    lists:foreach(fun(F) ->
                          ?assertEqual(HasToken,
                                       proplists:is_defined(F, Fields))
                  end,
                  [<<"token">>, <<"token_expiry">>]).

request_data(ProviderData) ->
    [{provider, <<"digits">>},
     {resource, <<"test_resource">>},
     {token, true},
     {provider_data, {struct, ProviderData}}].

provider_data() ->
    [{authTokenSecret, <<"vViH56F2f1sNi6RYZZeztDo8NoQMWxhGMDKAL0wCFcIUH">>},
     {authToken, <<"701990807448920064-JxNX4i57y5Wp6xBDVjNwKB4ZYUcC8FK">>},
     {'X-Auth-Service-Provider', list_to_binary(fake_digits_server:url())},
     {'X-Verify-Credentials-Authorization',
         <<"OAuth oauth_signature=\"%2FeT%2FOC%2F78Rij8QPEd3ghy%2FUOIbI%3D\""
         ",oauth_nonce=\"944F1D89-161C-47E4-8730-41BD43BB164F\","
         "oauth_timestamp=\"1456701445\",oauth_consumer_key="
         "\"e527IQiWSXZ5WHNxROUZk87uV\",oauth_token="
         "\"701990807448920064-JxNX4i57y5Wp6xBDVjNwKB4ZYUcC8FK\""
         ",oauth_version=\"1.0\",oauth_signature_method=\"HMAC-SHA1\"">>
     }].

request_stanza(Data) when is_list(Data) ->
    BinData = iolist_to_binary(mochijson2:encode({struct, Data})),
    ct:log("BinData: ~p", [BinData]),
    request_stanza(BinData);

request_stanza(BinData) when is_binary(BinData) ->
    Payload = <<0:8, "register", 0:8, "$J$", BinData/binary>>,
    Stanza = escalus_stanza:auth(<<"PLAIN">>, [base64_cdata(Payload)]),
    ct:log("Stanza: ~p", [Stanza]),
    Stanza.

base64_cdata(Payload) ->
    #xmlcdata{content = base64:encode(Payload)}.
