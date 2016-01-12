%%% @copyright 2015+ Hippware, Inc.
%%% @doc Integration test suite for ejabberd
-module(ejabberd_integration_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, smoke}].

groups() ->
    [{smoke, [sequence], [messages_story]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    net_kernel:start(['mongooseim@localhost', longnames]),

    wocky_app:start(),

    application:load(ejabberd),
    DataDir = proplists:get_value(data_dir, Config),
    ConfigPath = filename:join([DataDir, "ejabberd.cfg"]),
    application:set_env(ejabberd, config, ConfigPath),
    application:ensure_all_started(ejabberd),

    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config),
    application:stop(ejabberd),
    wocky_app:stop(),
    ok.

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

messages_story(Config) ->
    %% Note that this story involves creating users and authenticating
    %% them via ejabberd_auth_wocky
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% Alice sends a message to Bob
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob gets the message
        escalus:assert(is_chat_message, [<<"OH, HAI!">>],
                       escalus:wait_for_stanza(Bob))
    end).
