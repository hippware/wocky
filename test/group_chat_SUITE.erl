%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_user
-module(group_chat_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).

-include("wocky.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky_db_seed.hrl").

-import(test_helper, [expect_iq_success/2, expect_iq_error/2]).

% There's some overlap here where we want to test the MAM operation
% on group messages, so it's handy to use a couple of functions from the
% MAM suite.
-import(mam_SUITE, [stanza_archive_request/3,
                    assert_respond_size/2,
                    wait_archive_respond_iq_first/1
                   ]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [send_messages,
     block_messages,
     add_user,
     remove_user,
     get_info,
     invalid_messages,
     invalid_iq,
     archive
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    Users = escalus:get_users([alice, bob, carol, karen]),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ).

end_per_suite(Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, carol, karen])),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% mod_group_chat tests
%%--------------------------------------------------------------------

send_messages(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        GroupID = start_chat(Alice, [Bob, Carol]),
        Msg = <<"It's goodnight from me">>,
        MsgStanza = chat_stanza(?ALICE_B_JID, GroupID, Msg),
        escalus:send(Alice, MsgStanza),
        lists:foreach(fun(U) -> wait_for_msg(U, ?ALICE_B_JID, Msg) end,
                      [Bob, Carol]),
        Msg2 = <<"And its goodnight from him">>,
        MsgStanza2 = chat_stanza(?BOB_B_JID, GroupID, Msg2),
        escalus:send(Bob, MsgStanza2),
        lists:foreach(fun(U) -> wait_for_msg(U, ?BOB_B_JID, Msg2) end,
                      [Alice, Carol]),
        timer:sleep(500),
        lists:foreach(fun(U) -> assert_no_stanzas(U) end,
                      [Alice, Bob, Carol])
      end).

block_messages(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        GroupID = start_chat(Alice, [Bob]),
        Msg = <<"It's goodnight from me">>,
        MsgStanza = chat_stanza(?CAROL_B_JID, GroupID, Msg),
        escalus:send(Carol, MsgStanza),

        ResponseStanza = escalus:wait_for_stanza(Carol),
        escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
                       ResponseStanza),

        % Message sent by non-participant should never be seen by participants
        timer:sleep(500),
        lists:foreach(fun(U) -> assert_no_stanzas(U) end,
                      [Alice, Bob, Carol])
      end).

add_user(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {karen, 1}],
      fun(Alice, Bob, Carol, Karen) ->
        GroupID = start_chat(Alice, [Bob, Carol]),
        AddStanza = add_participant_stanza(?ALICE_B_JID, ?KAREN_B_JID,
                                           GroupID),
        expect_iq_success(AddStanza, Alice),
        wait_for_add(Karen, GroupID),
        lists:foreach(fun(U) ->
                              wait_for_other_add(U, ?KAREN_B_JID, GroupID)
                      end,
                      [Bob, Carol]),
        Msg = <<"Welcome, Karen!">>,
        MsgStanza = chat_stanza(?ALICE_B_JID, GroupID, Msg),
        escalus:send(Alice, MsgStanza),
        lists:foreach(fun(U) -> wait_for_msg(U, ?ALICE_B_JID, Msg) end,
                      [Bob, Carol, Karen]),
        timer:sleep(500),
        lists:foreach(fun(U) -> assert_no_stanzas(U) end,
                      [Alice, Bob, Carol, Karen])
      end).

remove_user(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        GroupID = start_chat(Alice, [Bob, Carol]),
        AddStanza = remove_participant_stanza(?ALICE_B_JID, ?CAROL_B_JID,
                                           GroupID),
        expect_iq_success(AddStanza, Alice),
        wait_for_remove(Carol, GroupID),
        lists:foreach(fun(U) ->
                              wait_for_other_remove(U, ?CAROL_B_JID, GroupID)
                      end,
                      [Bob]),
        Msg = <<"Bye Felicia!">>,
        MsgStanza = chat_stanza(?ALICE_B_JID, GroupID, Msg),
        escalus:send(Alice, MsgStanza),
        lists:foreach(fun(U) -> wait_for_msg(U, ?ALICE_B_JID, Msg) end,
                      [Bob]),
        timer:sleep(500),
        lists:foreach(fun(U) -> assert_no_stanzas(U) end,
                      [Alice, Bob, Carol])
      end).

get_info(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        GroupID = start_chat(Alice, [Bob, Carol]),
        GIStanza = get_info_stanza(GroupID),
        ResultStanza = expect_iq_success(GIStanza, Bob),
        check_info_result(ResultStanza, GroupID)
      end).

invalid_messages(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        GroupID = start_chat(Alice, [Bob, Carol]),
        Msg = <<"It's goodnight from me">>,
        ResponseMessages = [chat_stanza(?ALICE_B_JID,
                                        wocky_db:create_id(), Msg),
                            chat_stanza(?ALICE_B_JID, <<"jibberish">>, Msg),
                            malformed_node_stanza(GroupID)
                           ],
        DroppedMessages = [missing_node_stanza(GroupID),
                           missing_ns_stanza()
                          ],
        lists:foreach(fun(M) -> expect_error(Alice, M) end,
                      ResponseMessages),
        lists:foreach(fun(M) -> escalus:send(Alice, M) end,
                      DroppedMessages),
        timer:sleep(500),
        lists:foreach(fun(U) -> assert_no_stanzas(U) end,
                      [Alice, Bob, Carol])
      end).

archive(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        GroupID = start_chat(Alice, [Bob]),
        Msg = <<"Archive me!">>,
        MsgStanza = chat_stanza(?ALICE_B_JID, GroupID, Msg),
        escalus:send(Alice, MsgStanza),
        wait_for_msg(Bob, ?ALICE_B_JID, Msg),
        GroupJIDBin = jid:to_binary(jid:make(GroupID, ?LOCAL_CONTEXT, <<>>)),
        ArchReq = stanza_archive_request([], <<"q1">>, GroupJIDBin),
        escalus:send(Alice, ArchReq),
        assert_respond_size(1, wait_archive_respond_iq_first(Alice)),
        escalus:send(Bob, ArchReq),
        assert_respond_size(1, wait_archive_respond_iq_first(Bob)),

        % Carol's not in the chat, so she can't see the archive:
        expect_iq_error(ArchReq, Carol),

        % Let's add her
        AddStanza = add_participant_stanza(?ALICE_B_JID, ?CAROL_B_JID,
                                           GroupID),
        expect_iq_success(AddStanza, Alice),
        wait_for_add(Carol, GroupID),
        wait_for_other_add(Bob, ?CAROL_B_JID, GroupID),

        % Carol can now read the archive
        escalus:send(Carol, ArchReq),
        assert_respond_size(1, wait_archive_respond_iq_first(Carol)),

        % Test with XEP-313 style form-baed query
        escalus:send(Carol, xep_0313_gc_query(GroupID)),
        assert_respond_size(1, wait_archive_respond_iq_first(Carol)),

        timer:sleep(500),
        lists:foreach(fun(U) -> assert_no_stanzas(U) end,
                      [Alice, Bob, Carol])
      end).


invalid_iq(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        InvalidStanza = invalid_iq_stanza(),
        expect_iq_error(InvalidStanza, Alice)
      end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_chat(Owner, Participants) ->
    QueryStanza = chat_request(Participants, ?CHAT_TITLE),
    ResultStanza = expect_iq_success(QueryStanza, Owner),
    FieldsXML = exml_query:path(ResultStanza,
                                [{element, <<"chat-created">>}]),
    {value, <<?GROUP_CHAT_RESOURCE_PREFIX, GroupID/binary>>} =
    xml:get_tag_attr(<<"node">>, FieldsXML),
    lists:foreach(fun(U) -> wait_for_add(U, GroupID) end,
                  Participants),
    GroupID.

chat_request(Participants, Title) ->
    test_helper:iq_get(?NS_GROUP_CHAT,
                       #xmlel{name = <<"new-chat">>,
                              children = [title_field(Title),
                                          participants_field(Participants)]}).

participants_field(Participants) ->
    #xmlel{name = <<"participants">>,
           children = [participant_field(P) || P <- Participants]}.

participant_field(Participant) ->
    #xmlel{name = <<"participant">>,
           children = [#xmlcdata{content =
                                 escalus_client:short_jid(Participant)}]}.

title_field(Title) ->
    #xmlel{name = <<"title">>,
           children = [#xmlcdata{content = Title}]}.

wait_for_add(User, GroupID) ->
    AddNotify = check_common_change_fields(User, GroupID),
    ?assertMatch(#xmlel{name = <<"added-to-group">>}, AddNotify),
    TitleTag = xml:get_subtag(AddNotify, <<"title">>),
    ?assertEqual(?CHAT_TITLE, xml:get_tag_cdata(TitleTag)),
    OwnerTag = xml:get_subtag(AddNotify, <<"owner">>),
    ?assertEqual(?ALICE_B_JID, xml:get_tag_cdata(OwnerTag)),
    #xmlel{} = xml:get_subtag(AddNotify, <<"participants">>).

wait_for_remove(User, GroupID) ->
    RemoveNotify = check_common_change_fields(User, GroupID),
    ?assertMatch(#xmlel{name = <<"removed-from-group">>}, RemoveNotify).

check_common_change_fields(User, GroupID) ->
    Stanza = escalus:wait_for_stanza(User),
    escalus:assert(is_message, Stanza),
    ?assertEqual({value, <<"headline">>}, xml:get_tag_attr(<<"type">>, Stanza)),
    [Notify] = Stanza#xmlel.children,
    {value, NodeAttr} = xml:get_tag_attr(<<"node">>, Notify),
    ExpectedNode = make_node(GroupID),
    ?assertEqual(ExpectedNode, NodeAttr),
    Notify.

wait_for_other_add(User, NewJID, GroupID) ->
    wait_for_other_change(User, NewJID, GroupID, add).

wait_for_other_remove(User, NewJID, GroupID) ->
    wait_for_other_change(User, NewJID, GroupID, remove).

wait_for_other_change(User, NewJID, GroupID, Op) ->
    Stanza = escalus:wait_for_stanza(User),
    escalus:assert(is_message, Stanza),
    assert_participant_changed(NewJID, GroupID, Stanza#xmlel.children, Op).

wait_for_msg(User, From, Msg) ->
    Stanza = escalus:wait_for_stanza(User),
    escalus:assert(is_chat_message, Stanza),
    {value, FromAttr} = xml:get_tag_attr(<<"from">>, Stanza),
    ?assertEqual(From, jid:to_binary(
                         jid:to_bare(
                           jid:from_binary(FromAttr)))),
    Body = xml:get_subtag(Stanza, <<"body">>),
    ?assertEqual(Msg, xml:get_tag_cdata(Body)).

chat_stanza(From, GroupID, Msg) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"chat">>},
                    {<<"to">>, ?LOCAL_CONTEXT},
                    {<<"from">>, From},
                    {<<"id">>, wocky_util:iq_id()},
                    {<<"xmlns:groupchat">>, ?NS_GROUP_CHAT},
                    {<<"groupchat:node">>, make_node(GroupID)}
                   ],
           children = [#xmlel{name = <<"body">>,
                              children = [#xmlcdata{content = Msg}]}]}.

missing_node_stanza(GroupID) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"chat">>},
                    {<<"to">>, ?LOCAL_CONTEXT},
                    {<<"from">>, ?ALICE_B_JID},
                    {<<"id">>, wocky_util:iq_id()},
                    {<<"xmlns:groupchat">>, ?NS_GROUP_CHAT},
                    {<<"groupchat:XXXXXX">>, make_node(GroupID)}
                   ],
           children = [#xmlel{name = <<"body">>,
                              children = [#xmlcdata{content = <<"asdf">>}]}]}.

missing_ns_stanza() ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"chat">>},
                    {<<"to">>, ?LOCAL_CONTEXT},
                    {<<"from">>, ?ALICE_B_JID},
                    {<<"id">>, wocky_util:iq_id()}
                   ],
           children = [#xmlel{name = <<"body">>,
                              children = [#xmlcdata{content = <<"asdf">>}]}]}.

malformed_node_stanza(GroupID) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"chat">>},
                    {<<"to">>, ?LOCAL_CONTEXT},
                    {<<"from">>, ?ALICE_B_JID},
                    {<<"id">>, wocky_util:iq_id()},
                    {<<"xmlns:groupchat">>, ?NS_GROUP_CHAT},
                    {<<"groupchat:node">>, GroupID}
                   ],
           children = [#xmlel{name = <<"body">>,
                              children = [#xmlcdata{content = <<"asdf">>}]}]}.

assert_no_stanzas(User) ->
    dump_stanzas(User),
    ?assertNot(escalus_client:has_stanzas(User)).

dump_stanzas(User) ->
    ct:log("------------------"),
    ct:log("Peek: ~p", [escalus_client:peek_stanzas(User)]).

add_participant_stanza(From, Participant, GroupID) ->
    change_participant_stanza(From, Participant, GroupID, add).

remove_participant_stanza(From, Participant, GroupID) ->
    change_participant_stanza(From, Participant, GroupID, remove).

change_participant_stanza(_From, Participant, GroupID, Op) ->
    OpField = case Op of
                  add -> <<"add-participant">>;
                  remove -> <<"remove-participant">>
              end,
    test_helper:iq_set(
      ?NS_GROUP_CHAT,
      #xmlel{name = OpField,
             attrs = [{<<"node">>, make_node(GroupID)}],
             children = [#xmlcdata{content = Participant}]}).

assert_participant_changed(SubjectJID, GroupID, [Child], Op) ->
    OpField = case Op of
                  add -> <<"participant-added">>;
                  remove -> <<"participant-removed">>
              end,
    ?assertEqual(OpField, Child#xmlel.name),
    Node = make_node(GroupID),
    ?assertEqual({value, Node}, xml:get_tag_attr(<<"node">>, Child)),
    ?assertEqual({value, ?NS_GROUP_CHAT}, xml:get_tag_attr(<<"xmlns">>, Child)),
    ?assertEqual(SubjectJID, xml:get_tag_cdata(Child)).

make_node(ChatID) ->
    <<?GROUP_CHAT_RESOURCE_PREFIX, ChatID/binary>>.

get_info_stanza(GroupID) ->
    test_helper:iq_get(?NS_GROUP_CHAT,
                       #xmlel{name = <<"group-info">>,
                              attrs = [{<<"node">>, make_node(GroupID)}]}).

check_info_result(#xmlel{children = [Child]}, GroupID) ->
    ?assertEqual(<<"group-info">>, Child#xmlel.name),
    {value, Node} = xml:get_tag_attr(<<"node">>, Child),
    ?assertEqual(Node, make_node(GroupID)),
    ?assertEqual({value, ?NS_GROUP_CHAT}, xml:get_tag_attr(<<"xmlns">>, Child)),
    ?assertEqual(?CHAT_TITLE, xml:get_tag_cdata(
                                xml:get_subtag(Child, <<"title">>))),
    ?assertEqual(?ALICE_B_JID, xml:get_tag_cdata(
                                 xml:get_subtag(Child, <<"owner">>))),
    ?assertEqual(3, length((xml:get_subtag(
                              Child, <<"participants">>))#xmlel.children)).

expect_error(User, Packet) ->
    assert_error_msg(escalus:send_and_wait(User, Packet)).

assert_error_msg(Packet) ->
    ?assertEqual(<<"message">>, Packet#xmlel.name),
    ?assertEqual({value, <<"error">>}, xml:get_tag_attr(<<"type">>, Packet)),
    [Child] = Packet#xmlel.children,
    ?assertEqual(<<"error">>, Child#xmlel.name).

expect_n_messages(User, N) ->
    lists:foreach(fun(_) -> escalus:wait_for_stanza(User) end,
                  lists:seq(1, N)).

invalid_iq_stanza() ->
    test_helper:iq_set(?NS_GROUP_CHAT, #xmlel{name = <<"new-chat">>}).

xep_0313_gc_query(GroupID) ->
    {ok, XML} = exml:parse(iolist_to_binary(
        ["<iq type='set' id='juliet1'>"
          "<query xmlns='urn:xmpp:mam:1'>"
            "<x xmlns='jabber:x:data' type='submit'>"
              "<field var='FORM_TYPE' type='hidden'>"
                "<value>urn:xmpp:mam:1</value>"
              "</field>"
              "<field var='with'>"
                "<value>", GroupID, "@", ?LOCAL_CONTEXT, "</value>"
              "</field>"
            "</x>"
          "</query>"
        "</iq>"])),
    XML.
