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

all() -> [extended_fields].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    Users = escalus:get_users([alice, bob]),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ).

end_per_suite(Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

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
