%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_group_chat.erl
-module(mod_wocky_group_chat_tests).

-include("wocky.hrl").
-include("wocky_db_seed.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-compile(export_all).

-import(mod_wocky_group_chat, [new_chat/3,
                               add_participant/2,
                               remove_participant/2,
                               get_info/2,
                               handle_packet/4,
                               is_participant/2
                              ]).

-define(ID, <<"123456">>).
-define(SUCCESS_IQ,
        #iq{id = ?ID,
            type = result,
            xmlns = ?NS_GROUP_CHAT}).
-define(ERR_STANZA,
        #xmlel{name = <<"error">>}).
-define(MSG_TXT, <<"HIYA!">>).

mod_wocky_group_chat_test_() -> {
  "mod_wocky_group_chat",
  setup, fun before_all/0, fun after_all/1,
  [
    test_new_chat(),
    test_add_participant(),
    test_remove_participant(),
    test_get_info(),
    test_handle_packet(),
    test_is_participant()
  ]
 }.

group_chat_tables() -> [group_chat].
shared_group_chat_tables() -> [user].

before_all() ->
    ok = wocky_db_seed:seed_tables(?LOCAL_CONTEXT, group_chat_tables()),
    ok = wocky_db_seed:seed_tables(shared, shared_group_chat_tables()),
    ok.

after_all(_) ->
    ok.

test_new_chat() ->
    { "new_chat", [
      { "creates a new chat",
        setup,
        fun() -> wocky_db:clear_tables(?LOCAL_CONTEXT, [group_chat]) end,
        fun(_) ->
                wocky_db:clear_tables(?LOCAL_CONTEXT, [group_chat]),
                wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [group_chat])
        end,
        [
        ?_assertMatch({ok, ?SUCCESS_IQ},
                      new_chat(?ALICE_JID, ?SERVER_JID, create_iq())),
        ?_assertMatch([#{owner := ?ALICE, title := ?CHAT_TITLE}],
                      wocky_db:select(?LOCAL_CONTEXT, group_chat, all, #{})),
        ?_assertEqual(lists:sort([jid:to_binary(M)
                                  || M <- [?ALICE_JID, ?BOB_JID, ?CAROL_JID]]),
                      lists:sort(wocky_db:select_one(?LOCAL_CONTEXT, group_chat,
                                                     participants, #{})))
      ]},
      { "fails on invalid user", [
        ?_assertMatch({error, ?ERR_STANZA},
                      new_chat(jid:make(wocky_db:create_id(),
                                        ?LOCAL_CONTEXT, <<>>),
                               ?SERVER_JID, create_iq())),
        ?_assertMatch({error, ?ERR_STANZA},
                      new_chat(jid:make(?ALICE, <<"otherserver">>, <<>>),
                               ?SERVER_JID, create_iq()))
      ]},
      { "fails on invalid server", [
        ?_assertMatch({error, ?ERR_STANZA},
                      new_chat(?ALICE_JID,
                               jid:make(<<>>, <<"otherserver">>, <<>>),
                               create_iq()))
      ]},
      { "fails on missing title", [
        ?_assertMatch({error, ?ERR_STANZA},
                      new_chat(?ALICE_JID, ?SERVER_JID,
                               create_iq(missing, default_part_list())))
      ]},
      { "fails on missing participant element", [
        ?_assertMatch({error, ?ERR_STANZA},
                      new_chat(?ALICE_JID, ?SERVER_JID,
                               create_iq(?CHAT_TITLE, missing)))
      ]},
      { "fails on empty participant element", [
        ?_assertMatch({error, ?ERR_STANZA},
                      new_chat(?ALICE_JID, ?SERVER_JID,
                               create_iq(?CHAT_TITLE, [])))
      ]},
      { "fails on invalid participant", [
        ?_assertMatch({error, ?ERR_STANZA},
                      new_chat(?ALICE_JID, ?SERVER_JID,
                               create_iq(?CHAT_TITLE,
                                         [<<"some guy@#$@#$@#">>]))),
        ?_assertMatch({error, ?ERR_STANZA},
                      new_chat(?ALICE_JID, ?SERVER_JID,
                               create_iq(?CHAT_TITLE, [garbage])))
      ]}
     ]
    }.

test_add_participant() ->
    { "add_participant", [
      { "adds a participant to a chat", [
        ?_assertMatch({ok, ?SUCCESS_IQ},
                      add_participant(?ALICE_JID,
                                      add_iq(jid:to_binary(?KAREN_JID),
                                             ?GROUP_CHAT))),
        ?_assert(lists:member(jid:to_binary(?KAREN_JID),
                              wocky_db:select_one(?LOCAL_CONTEXT,
                                                  group_chat,
                                                  participants, #{}))),
        ?_assertEqual(3, length(
                              wocky_db:select_one(?LOCAL_CONTEXT,
                                                  group_chat,
                                                  participants, #{})))
      ]},
      { "fails on invalid chat", [
        ?_assertMatch({error, ?ERR_STANZA},
                      add_participant(?ALICE_JID,
                                      add_iq(jid:to_binary(?KAREN_JID),
                                             wocky_db:create_id()))),
        ?_assertMatch({error, ?ERR_STANZA},
                      add_participant(?ALICE_JID,
                                         add_iq(jid:to_binary(?BOB_JID),
                                                missing))),
        ?_assertMatch({error, ?ERR_STANZA},
                      add_participant(?ALICE_JID,
                                         add_iq(jid:to_binary(?BOB_JID),
                                                garbage))),
        ?_assertMatch({error, ?ERR_STANZA},
                      add_participant(?ALICE_JID,
                        add_iq(jid:to_binary(?BOB_JID),
                                                <<"asdfasd">>)))
      ]},
      { "fails on non-owner", [
        ?_assertMatch({error, ?ERR_STANZA},
                      add_participant(?BOB_JID,
                                      add_iq(jid:to_binary(?KAREN_JID),
                                             ?GROUP_CHAT)))
      ]},
      { "fails on invalid participant", [
        ?_assertMatch({error, ?ERR_STANZA},
                      add_participant(?ALICE_JID,
                                      add_iq(<<"@@#@@##@">>,
                                             ?GROUP_CHAT)))
      ]},
      { "fails on empty participant", [
        ?_assertMatch({error, ?ERR_STANZA},
                      add_participant(?ALICE_JID,
                                      add_iq(missing,
                                             ?GROUP_CHAT)))
      ]}
    ]}.

test_remove_participant() ->
    { "remove_participant", [
      { "removes a participant from a chat", [
        ?_assertMatch({ok, ?SUCCESS_IQ},
                      remove_participant(?ALICE_JID,
                                      remove_iq(jid:to_binary(?BOB_JID),
                                                ?GROUP_CHAT))),
        ?_assertNot(lists:member(jid:to_binary(?BOB_JID),
                              wocky_db:select_one(?LOCAL_CONTEXT,
                                                  group_chat,
                                                  participants, #{}))),
        ?_assertEqual(2, length(
                              wocky_db:select_one(?LOCAL_CONTEXT,
                                                  group_chat,
                                                  participants, #{})))
      ]},
      { "fails on invalid chat", [
        ?_assertMatch({error, ?ERR_STANZA},
                      remove_participant(?ALICE_JID,
                                         remove_iq(jid:to_binary(?BOB_JID),
                                                wocky_db:create_id()))),
        ?_assertMatch({error, ?ERR_STANZA},
                      remove_participant(?ALICE_JID,
                                         remove_iq(jid:to_binary(?BOB_JID),
                                                missing))),
        ?_assertMatch({error, ?ERR_STANZA},
                      remove_participant(?ALICE_JID,
                                         remove_iq(jid:to_binary(?BOB_JID),
                                                garbage))),
        ?_assertMatch({error, ?ERR_STANZA},
                      remove_participant(?ALICE_JID,
                                         remove_iq(jid:to_binary(?BOB_JID),
                                                <<"asdfasd">>)))
      ]},
      { "fails on non-owner", [
        ?_assertMatch({error, ?ERR_STANZA},
                      remove_participant(?BOB_JID,
                                      remove_iq(jid:to_binary(?KAREN_JID),
                                             ?GROUP_CHAT)))
      ]},
      { "fails on invalid participant", [
        ?_assertMatch({error, ?ERR_STANZA},
                      remove_participant(?ALICE_JID,
                                      remove_iq(<<"@@#@@##@">>,
                                             ?GROUP_CHAT)))
      ]},
      { "fails on non-participant", [
        ?_assertMatch({error, ?ERR_STANZA},
                      remove_participant(?ALICE_JID,
                                      remove_iq(jid:to_binary(?BOB_JID),
                                             ?GROUP_CHAT)))
      ]},
      { "fails on empty participant", [
        ?_assertMatch({error, ?ERR_STANZA},
                      remove_participant(?ALICE_JID,
                                      remove_iq(missing,
                                             ?GROUP_CHAT)))
      ]}
    ]}.

test_get_info() ->
    { "get_info", [
      { "gets info on a chat", [
        ?_assertMatch({ok, ?SUCCESS_IQ},
                      get_info(?ALICE_JID, get_info_iq(?GROUP_CHAT))),
        ?_assertMatch({ok, ?SUCCESS_IQ},
                      get_info(?KAREN_JID, get_info_iq(?GROUP_CHAT)))
      ]},
      { "fails for non-participant", [
        ?_assertMatch({error, ?ERR_STANZA},
                      get_info(?BOB_JID, get_info_iq(?GROUP_CHAT)))
      ]},
      { "fails for invalid group", [
        ?_assertMatch({error, ?ERR_STANZA},
                      get_info(?ALICE_JID, get_info_iq(wocky_db:create_id()))),
        ?_assertMatch({error, ?ERR_STANZA},
                      get_info(?ALICE_JID, get_info_iq(missing))),
        ?_assertMatch({error, ?ERR_STANZA},
                      get_info(?ALICE_JID, get_info_iq(garbage))),
        ?_assertMatch({error, ?ERR_STANZA},
                      get_info(?ALICE_JID, get_info_iq(<<"xyzzy">>)))
      ]}
    ]}.

test_handle_packet() ->
    { "handle_packet", [
      { "accepts a valid group message", [
        ?_assertEqual(ok,
                      handle_packet(?ALICE_JID, ?LOCAL_CONTEXT,
                                    ?GROUP_CHAT,
                                    msg_packet(jid:to_binary(?ALICE_JID),
                                               ?GROUP_CHAT))),
        ?_assertEqual(ok, handle_packet(?BOB_JID, ?LOCAL_CONTEXT,
                                        ?GROUP_CHAT,
                                        msg_packet(jid:to_binary(?BOB_JID),
                                                   ?GROUP_CHAT)))
      ]}
    ]}.

test_is_participant() ->
    { "is_participant", [
      { "Returns true when a member of the specified chat is supplied", [
        ?_assert(is_participant(?ALICE_JID, ?GROUP_CHAT_JID)),
        ?_assert(is_participant(?KAREN_JID, ?GROUP_CHAT_JID))
      ]},
      { "Returns false for non- and removed members", [
        ?_assertNot(is_participant(?BOB_JID, ?GROUP_CHAT_JID)),
        ?_assertNot(is_participant(?CAROL_JID, ?GROUP_CHAT_JID))
      ]},
      { "Returns false for non-existant groups", [
        ?_assertNot(is_participant(?ALICE_JID, jid:make(
                                                 wocky_db:create_id(),
                                                 ?LOCAL_CONTEXT, <<>>)))
      ]}
    ]}.

%% Helpers

default_part_list() ->
    [jid:to_binary(?BOB_JID), jid:to_binary(?CAROL_JID)].

create_iq() ->
    create_iq(?CHAT_TITLE, default_part_list()).

create_iq(Title, ParticipantList) ->
    #iq{id = ?ID,
        type = get,
        xmlns = ?NS_GROUP_CHAT,
        sub_el = new_chat_el(Title, ParticipantList)}.

new_chat_el(Title, ParticipantList) ->
    #xmlel{name = <<"new-chat">>,
           attrs = [{<<"xmlel">>, ?NS_GROUP_CHAT}],
           children = title(Title) ++
                      participants(ParticipantList)}.

title(missing) -> [];
title(Title) ->
    [#xmlel{name = <<"title">>,
            children = [#xmlcdata{content = Title}]}].

participants(missing) -> [];
participants(ParticipantList) ->
    [#xmlel{name = <<"participants">>,
            children = [participant(P) || P <- ParticipantList]}].

participant(garbage) ->
    #xmlel{name = <<"lasdkfjdlskafjds">>,
           children = [#xmlcdata{content = <<"Sdafasd">>}]};
participant(P) ->
    #xmlel{name = <<"participant">>,
           children = [#xmlcdata{content = P}]}.

remove_iq(User, GroupID) ->
    change_iq(<<"remove">>, User, GroupID).
add_iq(User, GroupID) ->
    change_iq(<<"add">>, User, GroupID).

change_iq(Action, User, GroupID) ->
    #iq{id = ?ID,
        type = set,
        xmlns = ?NS_GROUP_CHAT,
        sub_el = change_participant_el(Action, User, GroupID)}.

change_participant_el(Action, User, GroupID) ->
    #xmlel{name = <<Action/binary, "-participant">>,
           attrs = [{<<"xmlns">>, ?NS_GROUP_CHAT} | group_node(GroupID)],
           children = user_cdata(User)}.

group_node(missing) ->
    [];
group_node(garbage) ->
    [{<<"node">>, <<"fnord">>}];
group_node(GroupID) ->
    [{<<"node">>, <<?GROUP_CHAT_RESOURCE_PREFIX, GroupID/binary>>}].

user_cdata(missing) ->
    [];
user_cdata(User) ->
    [#xmlcdata{content = User}].

get_info_iq(GroupID) ->
    #iq{id = ?ID,
        type = get,
        xmlns = ?NS_GROUP_CHAT,
        sub_el = get_info_el(GroupID)}.

get_info_el(GroupID) ->
    #xmlel{name = <<"group-info">>,
           attrs = [{<<"xmlns">>, ?NS_GROUP_CHAT} | group_node(GroupID)]}.

msg_packet(From, GroupID) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"from">>, From},
                    {<<"to">>, ?LOCAL_CONTEXT},
                    {<<"type">>, <<"chat">>},
                    {<<"id">>, ?ID},
                    {<<"xmlns:mygroupchat">>, ?NS_GROUP_CHAT},
                    {<<"mygroupchat:node">>,
                     <<?GROUP_CHAT_RESOURCE_PREFIX, GroupID/binary>>}],
           children = [body()]}.

body() ->
    #xmlel{name = <<"body">>,
           children = [#xmlcdata{content = ?MSG_TXT}]}.
