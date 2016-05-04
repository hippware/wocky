%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_user
-module(xmpp_reg_SUITE).
-compile(export_all).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky_db_seed.hrl").

%TODO: Remove me
-define(ALICE_UUID, escalus_users:get_username(Config, alice)).
-define(BOB_UUID, escalus_users:get_username(Config, bob)).

%-import(test_helper, [expect_iq_success/2, expect_iq_error/2]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, new} %,
%     {group, existing}
    ].

groups() ->
    [{new, [sequence], new_cases()}%,
   %  {existing, [sequence], existing_cases()}
    ].

new_cases() ->
    [
     new_user%,
     %invalid_json,
   %  missing_fields,
   %  unauthorized_new,
   %  invalid_phone_number,
   %  invalid_auth_provider,
   %  missing_auth_data,
   %  missing_user_id,
   %  bypass_prefixes
    ].

existing_cases() ->
    [
   %  unauthorized_update,
   %  update,
   %  invalid_user_id,
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    test_helper:start_ejabberd(),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config),
    test_helper:stop_ejabberd(),
    ok.

init_per_group(existing, Config) ->
    wocky_db_seed:clear_user_tables(?LOCAL_CONTEXT),
    escalus:create_users(Config),
    escalus_ejabberd:wait_for_session_count(Config, 0),
    Config;
init_per_group(_, Config) ->
    wocky_db_seed:clear_user_tables(?LOCAL_CONTEXT),
    Config.

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% mod_wocky_reg new user tests
%%--------------------------------------------------------------------

new_user(Config) ->
    AliceSpec = escalus_users:get_options(Config, alice),
    {ok, Client, _, _} = escalus_connection:start(AliceSpec, [start_stream, stream_features]),
    Stanza = request_stanza(request_data(provider_data())),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_redirect(Result).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

assert_is_redirect(#xmlel{name = Name, children = Children}) ->
    ?assertEqual(<<"failure">>, Name),
    assert_has_redirect_children(Children).

assert_has_redirect_children(Children) ->
    ?assertNotEqual(false, lists:keyfind(<<"redirect">>, #xmlel.name, Children)),
    Text = lists:keyfind(<<"text">>, #xmlel.name, Children),
    ?assertNotEqual(false, Text),
    [#xmlcdata{content = JSON}] = Text#xmlel.children,
    assert_redirect_data(JSON).

assert_redirect_data(JSON) ->
    {struct, Fields} = mochijson2:decode(JSON),
    RequiredFields = [<<"user">>, <<"server">>, <<"handle">>,
                      <<"provider">>, <<"external_id">>, <<"is_new">>,
                      <<"token">>, <<"token_expiry">>],
    lists:foreach(fun(F) -> ?assertNotEqual(none, proplists:lookup(F, Fields)) end,
                  RequiredFields).


request_data(ProviderData) ->
    [{provider, "digits"},
     {resource, "test_resource"},
     {token, true},
     {provider_data, {struct, ProviderData}}].

provider_data() ->
    [{userID, "701990807448920064"},
     {phoneNumber, "+15556667890"},
     {authTokenSecret, "vViH56F2f1sNi6RYZZeztDo8NoQMWxhGMDKAL0wCFcIUH"},
     {authToken, "701990807448920064-JxNX4i57y5Wp6xBDVjNwKB4ZYUcC8FK"},
     {'X-Auth-Service-Provider', fake_digits_server:url()},
     {'X-Verify-Credentials-Authorization',
         "OAuth oauth_signature=\"%2FeT%2FOC%2F78Rij8QPEd3ghy%2FUOIbI%3D\""
         ",oauth_nonce=\"944F1D89-161C-47E4-8730-41BD43BB164F\","
         "oauth_timestamp=\"1456701445\",oauth_consumer_key="
         "\"e527IQiWSXZ5WHNxROUZk87uV\",oauth_token="
         "\"701990807448920064-JxNX4i57y5Wp6xBDVjNwKB4ZYUcC8FK\""
         ",oauth_version=\"1.0\",oauth_signature_method=\"HMAC-SHA1\""
     }].

request_stanza(Data) ->
    BinData = iolist_to_binary(mochijson2:encode({struct, Data})),
    Payload = <<0:8, "register", 0:8,"$J$", BinData/binary>>,
    escalus_stanza:auth(<<"PLAIN">>, [base64_cdata(Payload)]).

base64_cdata(Payload) ->
    #xmlcdata{content = base64:encode(Payload)}.
