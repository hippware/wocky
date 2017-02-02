%%% @copyright 2017+ Hippware, Inc.
%%% @doc Integration test suite for auth functionality
-module(auth_SUITE).
-compile(export_all).

-compile({parse_transform, fun_chain}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky_db_seed.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          auth_failure
         ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    Users = escalus:get_users([alice]),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%% Test that our authorisation system is returning the correct
%% failure message
auth_failure(Config) ->
    UserSpec = escalus_users:get_userspec(Config, alice),
    ConnSteps = [start_stream,
                 stream_features],
    {ok, ClientConnection, _Props, _Features} =
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
