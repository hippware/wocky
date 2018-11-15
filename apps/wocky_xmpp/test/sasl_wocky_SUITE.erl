%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for extensions to the SASL X-WOCKY mechanism
-module(sasl_wocky_SUITE).

-compile(export_all).
-compile({parse_transform, fun_chain}).
-compile({parse_transform, cut}).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").

-define(client_jwt, 'Elixir.Wocky.Account.JWT.Client').
-define(firebase, 'Elixir.Wocky.Account.JWT.Firebase').
-define(PHONE, <<"+15551234567">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, new},
     {group, existing}
    ].

groups() ->
    [
     {new, [], new_cases()},
     {existing, [], existing_cases()}
    ].

new_cases() ->
    [
     new_user_with_firebase,
     new_user_with_bypass
    ].

existing_cases() ->
    [
     login_with_invalid_json,
     login_with_bypass,
     login_with_firebase
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

new_user_with_firebase(Config) ->
    Client = start_client(Config),
    User = ?wocky_factory:build(user),
    Stanza = request_stanza(client_token(firebase, User)),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_success(Result).

new_user_with_bypass(Config) ->
    Client = start_client(Config),
    User = ?wocky_factory:build(user),
    Stanza = request_stanza(client_token(bypass, User)),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_success(Result).


%%--------------------------------------------------------------------
%% existing user tests
%%--------------------------------------------------------------------

login_with_invalid_json(Config) ->
    Client = start_client(Config),
    Stanza = request_stanza(<<"Not at all valid JSON">>),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_not_authorized(Result).

login_with_bypass(Config) ->
    Client = start_client(Config),
    User = proplists:get_value(user, Config),
    Stanza = request_stanza(client_token(bypass, User)),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_success(Result).

login_with_firebase(Config) ->
    Client = start_client(Config),
    User = proplists:get_value(user, Config),
    Stanza = request_stanza(client_token(firebase, User)),
    Result = escalus:send_and_wait(Client, Stanza),
    assert_is_success(Result).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_client(Config) ->
    AliceSpec = escalus_users:get_options(Config, alice),
    {ok, Client, _} = escalus_connection:start(AliceSpec,
                                               [start_stream,
                                                stream_features]),
    Client.

assert_is_success(#xmlel{name = Name}) ->
    ?assertEqual(<<"success">>, Name).

assert_is_not_authorized(Result) ->
    assert_is_failure(Result, <<"not-authorized">>).

assert_is_failure(#xmlel{name = Name, children = Children}, Type) ->
    ?assertEqual(<<"failure">>, Name),
    ?assertNotEqual(false, lists:keyfind(Type, #xmlel.name, Children)).

client_token(bypass, User) ->
    {ok, JWT, _} = ?client_jwt:encode_and_sign(User),
    JWT;
client_token(firebase, User) ->
    {ok, FBT, _} = ?firebase:encode_and_sign(User),
    {ok, JWT, _} = ?client_jwt:encode_and_sign(FBT),
    JWT.

request_stanza(Payload) when is_binary(Payload) ->
    Stanza = escalus_stanza:auth(<<"X-WOCKY">>, [base64_cdata(Payload)]),
    ct:log("Auth Stanza: ~p", [Stanza]),
    Stanza.

base64_cdata(Payload) ->
    #xmlcdata{content = base64:encode(Payload)}.
