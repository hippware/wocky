%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for message stanza extensions
-module(message_SUITE).
-compile(export_all).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky_db_seed.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, messaging}].

groups() ->
    [{messaging, [sequence],
      [extended_fields
      ]}
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:start_ejabberd(),
    wocky_db_seed:clear_user_tables(?LOCAL_CONTEXT),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config),
    test_helper:stop_ejabberd().

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

% Verify that our extended fields survive the trip through wocky
extended_fields(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Stanza = escalus_stanza:chat_to(bob, <<"Check out my LOLCAT">>),
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

        ct:log("BJD\n~p\n~p", [ImageField, RecStanza#xmlel.children]),
        ?assert(lists:member(ImageField, RecStanza#xmlel.children))
    end).

%is_chat(Content) ->
%    fun(Stanza) -> escalus_pred:is_chat_message(Content, Stanza) end.

