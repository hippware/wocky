%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for message stanza extensions
-module(message_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky_db_seed.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [messages_story, extended_fields].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        test_helper:setup_users([alice, bob]),
        test_helper:make_everyone_friends(escalus:get_users([alice, bob]))
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

messages_story(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        % Alice sends a message to Bob
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),

        % Bob gets the message
        escalus_assert:is_chat_message(
          <<"Hi!">>, escalus_client:wait_for_stanza(Bob))
    end).

%% Verify that our extended fields survive the trip through wocky
extended_fields(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Stanza = escalus_stanza:chat_to(?BOB_B_JID, <<"Check out my LOLCAT">>),
        ImageURL = #xmlel{name = <<"url">>,
                          children =
                          [#xmlcdata{content =
                                     <<"tros", ?ALICE/binary, "@",
                                       ?LOCAL_CONTEXT/binary, "/file/",
                                       ?AVATAR_FILE/binary>>}]},
        ImageField = #xmlel{name = <<"image">>,
                            attrs = [{<<"xmlns">>,
                                      <<"hippware.com/hxep/media">>}],
                            children = [ImageURL]},
        SendStanza = Stanza#xmlel{children =
                                  [ImageField | Stanza#xmlel.children]},
        escalus:send(Alice, SendStanza),

        RecStanza = escalus_client:wait_for_stanza(Bob),

        ?assert(lists:member(ImageField, RecStanza#xmlel.children))
    end).
