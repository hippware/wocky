%%% @copyright 2017+ Hippware, Inc.
%%% @doc Integration test suite for auth functionality
%%% Note that we are not trying to re-test all of the underlying authentication
%%% functionality from MIM and ejabberd_auth_riak. We are only testing changes
%%% to the stock MIM functionality made in ejabberd_auth_wocky and
%%% mod_wocky_sasl_plain.
-module(auth_SUITE).

-compile(export_all).
-compile({parse_transform, fun_chain}).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [acquire_token,
     release_token,
     login_with_token,
     auth_failure].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        test_helper:setup_users([alice])
    ).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

acquire_token(Config) ->
    escalus:story(Config, [{alice, 1}], fun (Alice) ->
        escalus_client:send(Alice, token_stanza(<<"get">>)),
        Reply = escalus_client:wait_for_stanza(Alice),
        <<"result">> = exml_query:path(Reply, [{attr, <<"type">>}]),
        <<"$T$", _/binary>> =
            exml_query:path(Reply, [{element, <<"query">>}, cdata])
    end).

release_token(Config) ->
    escalus:story(Config, [{alice, 1}], fun (Alice) ->
        escalus_client:send(Alice, token_stanza(<<"set">>)),
        Reply = escalus_client:wait_for_stanza(Alice),
        <<"result">> = exml_query:path(Reply, [{attr, <<"type">>}])
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
auth_failure(Config) ->
    UserSpec = escalus_users:get_userspec(Config, alice),
    ConnSteps = [start_stream,
                 stream_features],
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

token_stanza(Type) ->
    escalus_stanza:iq(Type, [escalus_stanza:query_el(?NS_TOKEN, [])]).
