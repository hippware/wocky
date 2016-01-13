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
    [{group, smoke},
     {group, last_activity}].

groups() ->
    [{smoke, [sequence], [messages_story]},
     {last_activity, [sequence], [activity_story,
                                  update_activity_story,
                                  server_uptime_story,
                                  unknown_user_acivity_story]
     }].

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
    escalus:create_users(Config),
    Config2 = escalus:make_everyone_friends(Config),
    escalus_ejabberd:wait_for_session_count(Config2, 0),
    Config2.


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
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        %% Alice sends a message to Bob
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob gets the message
        escalus:assert(is_chat_message, [<<"OH, HAI!">>],
                       escalus:wait_for_stanza(Bob))
    end).


%%--------------------------------------------------------------------
%% mod_last tests
%%--------------------------------------------------------------------

activity_story(Config) ->
    % Last online story
    escalus:story(Config, [1, 1],
        fun(Alice, _Bob) ->
            %% Alice asks about Bob's last activity
            escalus_client:send(Alice, escalus_stanza:last_activity(bob)),

            %% server replies on Bob's behalf
            Stanza = escalus_client:wait_for_stanza(Alice),
            escalus:assert(is_last_result, Stanza),
            0 = get_last_activity(Stanza)
        end).


update_activity_story(Config) ->
    escalus:story(Config, [1],
        fun(Alice) ->
            %% Bob logs in
            {ok, Bob} = escalus_client:start_for(Config, bob, <<"bob">>),

            %% Bob logs out with a status
            Status = escalus_stanza:tags([{<<"status">>,
                                           <<"I am a banana!">>}]),
            Presence = escalus_stanza:presence(<<"unavailable">>, Status),
            escalus_client:send(Bob, Presence),
            escalus_client:stop(Bob),
            timer:sleep(1024), % more than a second

            %% Alice asks for Bob's last availability
            escalus_client:send(Alice, escalus_stanza:last_activity(bob)),

            %% Alice receives Bob's status and last online time > 0
            Stanza = escalus_client:wait_for_stanza(Alice),
            escalus:assert(is_last_result, Stanza),
            true = (1 =< get_last_activity(Stanza)),
            <<"I am a banana!">> = get_last_status(Stanza)
        end).

server_uptime_story(Config) ->
    escalus:story(Config, [1],
        fun(Alice) ->
            %% Alice asks for server's uptime
            Server = escalus_users:get_server(Config, alice),
            escalus_client:send(Alice, escalus_stanza:last_activity(Server)),

            %% Server replies with the uptime > 0
            Stanza = escalus_client:wait_for_stanza(Alice),
            escalus:assert(is_last_result, Stanza),
            true = (get_last_activity(Stanza) > 0)
        end).

unknown_user_acivity_story(Config) ->
    escalus:story(Config, [1],
        fun(Alice) ->
            escalus_client:send(Alice,
                                escalus_stanza:last_activity(<<"sven">>)),
            Stanza = escalus_client:wait_for_stanza(Alice),
            escalus:assert(is_error,
                           [<<"cancel">>, <<"service-unavailable">>], Stanza)
        end),
    ok.


get_last_activity(Stanza) ->
    S = exml_query:path(Stanza, [{element, <<"query">>}, {attr, <<"seconds">>}]),
    list_to_integer(binary_to_list(S)).

get_last_status(Stanza) ->
    exml_query:path(Stanza, [{element, <<"query">>}, cdata]).
