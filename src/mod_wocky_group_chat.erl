%%% @copyright 2016+ Hippware, Inc.
%%% @doc Module implementing Wocky group chat
%%% See https://github.com/hippware/tr-wiki/wiki/Group-messaging
%%%
-module(mod_wocky_group_chat).

-behaviour(gen_mod).

-compile({parse_transform, do}).

-include("wocky.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).

%% IQ and message filter hooks
-export([filter_packet/1, handle_iq/3]).
-ignore_xref([{filter_packet, 1}, {handle_iq, 3}]).

%% Other group chat API functions
-export([is_participant/2]).

-ifdef(TEST).
-export([new_chat/3,
         add_participant/2,
         remove_participant/2,
         get_info/2,
         handle_packet/4
        ]).
-endif.

start(Host, _Opts) ->
    ejabberd_hooks:add(filter_local_packet, Host,
                       ?MODULE, filter_packet, 200),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_GROUP_CHAT,
                                  ?MODULE, handle_iq, parallel),
    setup_metrics().

stop(Host) ->
    ejabberd_hooks:delete(filter_local_packet, Host,
                          ?MODULE, filter_packet, 200),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_GROUP_CHAT).

filter_packet(drop) ->
    drop;
filter_packet(In = {From,
                    #jid{luser = <<>>,
                         lserver = LServer,
                         lresource = <<>>},
                    Packet}) ->
    case maybe_handle_packet(From, LServer, Packet) of
        handled -> drop;
        not_handled -> In
    end;
filter_packet(AnythingElse) ->
    AnythingElse.

-spec is_participant(jid(), jid()) -> boolean().
is_participant(User, GroupJID) ->
    Group = GroupJID#jid.luser,
    R = do([error_m ||
            check_group(Group),
            GroupRec <- get_group(Group),
            check_existing_participant(
              jid:to_binary(jid:to_bare(User)), GroupRec)
           ]),
    R =:= ok.

%%%===================================================================
%%% Message packet handling
%%%===================================================================

maybe_handle_packet(From, LServer, Packet = #xmlel{name = <<"message">>}) ->
    case wocky_app:server() of
        LServer -> maybe_handle_message_packet(From, LServer, Packet);
        _ -> not_handled
    end;
maybe_handle_packet(_, _, _) ->
    not_handled.

maybe_handle_message_packet(From, LServer, Packet = #xmlel{attrs = Attrs}) ->
    case find_group_node(Attrs) of
        undefined ->
            not_handled;
        Node ->
            handle_packet(From, LServer, Node, Packet),
            handled
    end.

handle_packet(From, LServer, Group, Packet) ->
    case process_packet(From, LServer, Group, Packet) of
        {error, E} -> msg_error_response(From, E, Packet);
        ok -> ok
    end.

process_packet(From, LServer, Node, Packet) ->
    do([error_m ||
        GroupID <- check_node(Node),
        check_group(GroupID),
        GroupRec <- get_group(LServer, GroupID),
        check_existing_participant(jid:to_binary(jid:to_bare(From)), GroupRec),
        forward_packet(From, GroupRec, Packet),
        archive_packet(LServer, GroupID, Packet),
        {ok, wocky_metrics:inc(mod_group_chat_messages)}
       ]).

check_group(Group) ->
    case wocky_db:is_valid_id(Group) of
        true -> ok;
        false -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid groupchat ID">>)}
    end.

get_group(LServer, Group) ->
    case wocky_db:select_row(LServer, group_chat, all, #{id => Group}) of
        not_found -> {error, ?ERRT_ITEM_NOT_FOUND(?MYLANG,
                                                  <<"Group not found">>)};
        GroupRec -> {ok, GroupRec}
    end.

forward_packet(From, #{participants := Participants},
               ForwardPacket) ->
    ForwardTo = Participants -- [jid:to_binary(jid:to_bare(From))],
    lists:foreach(fun(P) ->
                          forward_to_participant(From, P, ForwardPacket)
                  end,
                  ForwardTo).

forward_to_participant(From, To, Packet) ->
    ejabberd_router:route(From, jid:from_binary(To), Packet).

archive_packet(LServer, GroupID, Packet) ->
    GroupJID = jid:make(GroupID, LServer, <<>>),
    ejabberd_hooks:run(user_send_packet, LServer,
                       [GroupJID,
                        jid:from_binary(?GROUP_CHAT_WITH_JID),
                        Packet]).

msg_error_response(From, E, Orig) ->
    Msg = xml:replace_tag_attr(<<"type">>, <<"error">>, Orig),
    Msg2 = Msg#xmlel{children = [E]},
    ejabberd_router:route_error(jid:make(<<>>, wocky_app:server(), <<>>),
                                From, Msg2, Orig).

%%%===================================================================
%%% IQ packet handling
%%%===================================================================

-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> iq().
handle_iq(From, To, IQ) ->
    case handle_iq_type(From, To, IQ) of
        {ok, ResponseIQ} -> ResponseIQ;
        {error, Error} -> wocky_util:make_error_iq_response(IQ, Error)
    end.

handle_iq_type(From, To, IQ = #iq{type = get,
                                  sub_el = #xmlel{name = <<"new-chat">>}}) ->
    new_chat(From, To, IQ);
handle_iq_type(From, _To, IQ = #iq{type = get,
                   sub_el = #xmlel{name = <<"group-info">>}}) ->
    get_info(From, IQ);
handle_iq_type(From, _To, IQ = #iq{type = set,
                   sub_el = #xmlel{name = <<"add-participant">>}}) ->
    add_participant(From, IQ);
handle_iq_type(From, _To, IQ = #iq{type = set,
                   sub_el = #xmlel{name = <<"remove-participant">>}}) ->
    remove_participant(From, IQ);
handle_iq_type(_From, _To, _IQ) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid query">>)}.

%%%===================================================================
%%% New chat creation
%%%===================================================================

new_chat(From, To, IQ = #iq{sub_el = SubEl}) ->
    do([error_m ||
        check_server(To#jid.lserver, "to"),
        check_server(From#jid.lserver, "from"),
        check_user(From#jid.luser),
        Title <- check_title(SubEl),
        Participants <- check_participants(SubEl),
        GroupRec <- create_chat(From, Title, Participants),
        notify_other_participants(From, Participants, GroupRec),
        {ok, wocky_metrics:inc(mod_group_chat_joins, length(Participants) + 1)},
        create_response(IQ, GroupRec)
       ]).

check_server(LServer, FieldName) ->
    case wocky_app:server() of
        LServer -> ok;
        _ -> {error, ?ERRT_NOT_ACCEPTABLE(?MYLANG, iolist_to_binary(
                         ["Invalid server specified in '", FieldName, "'"]))}
    end.

check_user(LUser) ->
    case wocky_db_user:does_user_exist(LUser, wocky_app:server()) of
        true -> ok;
        false -> {error, ?ERRT_NOT_ACCEPTABLE(?MYLANG, <<"Invalid user">>)}
    end.

check_title(SubEl) ->
    case xml:get_subtag(SubEl, <<"title">>) of
        TitleEl = #xmlel{} ->
            {ok, xml:get_tag_cdata(TitleEl)};
        false ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"<title> element required">>)}
    end.

check_participants(SubEl) ->
    case xml:get_subtag(SubEl, <<"participants">>) of
        #xmlel{children = Children} ->
            extract_participants(Children, []);
        false ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"<participants> element required">>)}
    end.

extract_participants([], []) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG,
                              <<"No participants specified">>)};
extract_participants([], Acc) ->
    {ok, lists:usort(Acc)};
extract_participants([PartEl = #xmlel{name = <<"participant">>} | Rest], Acc) ->
    Participant = xml:get_tag_cdata(PartEl),
    case jid:from_binary(Participant) of
        JID = #jid{} ->
            extract_participants(Rest, [jid:to_binary(jid:to_bare(JID)) | Acc]);
        error ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG, iolist_to_binary(
                                      ["Invalid participant: ", Participant]))}
    end;
extract_participants([_ | Rest], Acc) ->
    extract_participants(Rest, Acc).

create_chat(User = #jid{luser = LUser}, Title, Participants) ->
    AllParticipants = [jid:to_binary(jid:to_bare(User)) | Participants],
    GroupRec = #{id => wocky_db:create_id(),
                 owner => LUser,
                 participants => AllParticipants,
                 title => Title},
    ok = wocky_db:insert(wocky_app:server(), group_chat, GroupRec),
    {ok, GroupRec}.

create_response(IQ, #{id := ID}) ->
    {ok, IQ#iq{type = result,
               sub_el = #xmlel{name = <<"chat-created">>,
                               attrs = response_attrs(ID)}}}.

response_attrs(ID) ->
    [{<<"xmlns">>, ?NS_GROUP_CHAT},
     {<<"node">>, <<"groupchat/", ID/binary>>}].

notify_other_participants(From, Participants, GroupRec) ->
    lists:foreach(fun(P) -> notify_youre_added(From, P, GroupRec) end,
                  Participants).

%%%===================================================================
%%% get-info functions
%%%===================================================================

get_info(From, IQ = #iq{sub_el = SubEl}) ->
    do([error_m ||
        GroupRec <- check_chat(SubEl),
        check_existing_participant(jid:to_binary(jid:to_bare(From)), GroupRec),
        info_response(IQ, GroupRec)]).

info_response(IQ, GroupRec = #{id := ID}) ->
    {ok, IQ#iq{type = result,
               sub_el = #xmlel{name = <<"group-info">>,
                               attrs = response_attrs(ID),
                               children = group_data(GroupRec)}}}.

%%%===================================================================
%%% add-participant functions
%%%===================================================================

add_participant(From, IQ = #iq{sub_el = SubEl}) ->
    do([error_m ||
        GroupRec <- check_chat(SubEl),
        check_owner(From, GroupRec),
        Participant <- check_valid_participant(SubEl),
        GroupRec2 <- do_add_participant(Participant, GroupRec),
        notify_add(From, Participant, GroupRec2),
        R <- add_remove_success(IQ, GroupRec2),
        {ok, wocky_metrics:inc(mod_group_chat_joins)},
        {ok, R}
       ]).

do_add_participant(Participant, GroupRec) ->
    do_add_remove_participant(Participant, GroupRec, "+").

notify_add(From, Participant,
           GroupRec = #{id := ID, participants := Participants}) ->
    notify_youre_added(From, Participant, GroupRec),
    notify_other_added(From, Participant, Participants, ID).

notify_youre_added(From, Participant, GroupRec = #{id := ID}) ->
    Msg = (headline_msg())#xmlel{
           children = [#xmlel{name = <<"added-to-group">>,
                              attrs = response_attrs(ID),
                              children = group_data(GroupRec)}]},
    ejabberd_router:route(From, jid:from_binary(Participant), Msg).

notify_other_added(From, Participant, Participants, ID) ->
    notify_other_change(From, Participant, Participants,
                        <<"participant-added">>, ID).

%%%===================================================================
%%% remove-participant functions
%%%===================================================================

remove_participant(From, IQ = #iq{sub_el = SubEl}) ->
    do([error_m ||
        GroupRec <- check_chat(SubEl),
        check_owner(From, GroupRec),
        Participant <- check_valid_participant(SubEl),
        check_existing_participant(Participant, GroupRec),
        GroupRec2 <- do_remove_participant(Participant, GroupRec),
        notify_remove(From, Participant, GroupRec2),
        {ok, wocky_metrics:inc(mod_group_chat_departs)},
        add_remove_success(IQ, GroupRec2)
       ]).

check_existing_participant(Participant, #{participants := Participants}) ->
    case lists:member(Participant, Participants) of
        true -> ok;
        false -> {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                           <<"Participant not found">>)}
    end.

do_remove_participant(Participant, GroupRec) ->
    do_add_remove_participant(Participant, GroupRec, "-").

notify_remove(From, Participant, #{id := ID, participants := Participants}) ->
    notify_youre_removed(From, Participant, ID),
    notify_other_removed(From, Participant, Participants, ID).

notify_youre_removed(From, Participant, ID) ->
    Msg = (headline_msg())#xmlel{
           children = [#xmlel{name = <<"removed-from-group">>,
                              attrs = response_attrs(ID)}]},
    ejabberd_router:route(From, jid:from_binary(Participant), Msg).

notify_other_removed(From, Participant, Participants, ID) ->
    notify_other_change(From, Participant, Participants,
                        <<"participant-removed">>, ID).

%%%===================================================================
%%% Common helper functions
%%%===================================================================

check_owner(#jid{luser = LUser}, #{owner := LUser}) -> ok;
check_owner(_, _) -> {error, ?ERRT_NOT_ALLOWED(?MYLANG, <<"Not chat owner">>)}.

check_valid_participant(#xmlel{children = [#xmlcdata{content = User}]}) ->
    case jid:from_binary(User) of
        JID = #jid{} -> {ok, jid:to_binary(jid:to_bare(JID))};
        error -> {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                           <<"Invalid user specified">>)}
    end;
check_valid_participant(_) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"No user specified">>)}.

group_data(#{owner := Owner, title := Title, participants := Participants}) ->
    [text_element(<<"title">>, Title),
     text_element(<<"owner">>, jid:to_binary(
                                 jid:make(Owner, wocky_app:server(), <<>>))),
     #xmlel{name = <<"participants">>,
            children = participant_elements(Participants)}].

participant_elements(Participants) ->
    [text_element(<<"participant">>, P) || P <- Participants].

headline_msg() ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}]}.

text_element(Name, Value) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content = Value}]}.

text_element_ns(Name, Value, ID) ->
    (text_element(Name, Value))#xmlel{attrs = [{<<"xmlns">>, ?NS_GROUP_CHAT},
                                               {<<"node">>, make_node(ID)}]}.

make_node(ID) ->
    <<?GROUP_CHAT_RESOURCE_PREFIX, ID/binary>>.

check_chat(SubEl) ->
    do([error_m ||
        Node <- get_node(SubEl),
        ID <- check_valid_id(Node),
        get_group(ID)
       ]).

get_node(SubEl) ->
    case xml:get_tag_attr(<<"node">>, SubEl) of
        {value, ID} -> {ok, ID};
        false -> {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                           <<"No chat node specified">>)}
    end.

check_valid_id(<<?GROUP_CHAT_RESOURCE_PREFIX, ID/binary>>) ->
    case wocky_db:is_valid_id(ID) of
        true -> {ok, ID};
        false -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid chat node">>)}
    end;
check_valid_id(_) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid chat node">>)}.

get_group(ID) ->
    case wocky_db:select_row(wocky_app:server(),
                             group_chat, all, #{id => ID}) of
        not_found -> {error, ?ERRT_ITEM_NOT_FOUND(?MYLANG,
                                                  <<"Chat not found">>)};
        Group -> {ok, Group}
    end.

do_add_remove_participant(Participant, #{id := ID}, Op) ->
    Q = "UPDATE group_chat SET participants = participants "
        ++ Op ++ " ? WHERE id = ?",
    V = #{participants => [Participant],
          id => ID},
    {ok, _} = wocky_db:query(wocky_app:server(), Q, V, quorum),
    {ok, wocky_db:select_row(wocky_app:server(), group_chat,
                             all, #{id => ID})}.

notify_other_change(From, Participant, Participants, ChangeType, ID) ->
    Msg = (headline_msg())#xmlel{
                           children = [text_element_ns(ChangeType,
                                                       Participant, ID)]},
    lists:foreach(
      fun(P) ->
              ejabberd_router:route(From, jid:from_binary(P), Msg)
      end,
      Participants -- [Participant, jid:to_binary(jid:to_bare(From))]).

add_remove_success(IQ, #{id := ID}) ->
    {ok, IQ#iq{type = result,
               sub_el = #xmlel{name = <<"success">>,
                               attrs = response_attrs(ID)}}}.

find_group_node(Attrs) ->
    case get_group_node(Attrs) of
        {error, _} -> undefined;
        {ok, Node} -> Node
    end.

get_group_node(Attrs) ->
    do([error_m ||
        Binding <- find_ns_binding(Attrs),
        find_group_node_val(Binding, Attrs)]).

find_ns_binding([]) ->
    {error, not_found};
find_ns_binding([{<<"xmlns:", Binding/binary>>, ?NS_GROUP_CHAT} | _])
  when byte_size(Binding) =/= 0 ->
    {ok, Binding};
find_ns_binding([_ | Rest]) ->
    find_ns_binding(Rest).

find_group_node_val(Binding, Attrs) ->
    case xml:get_attr(<<Binding/binary, ":node">>, Attrs) of
        {value, Val} -> {ok, Val};
        _ -> {error, not_found}
    end.

check_node(<<?GROUP_CHAT_RESOURCE_PREFIX, Group/binary>>) -> {ok, Group};
check_node(_) -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid node">>)}.

setup_metrics() ->
    Metrics = [mod_group_chat_messages,
               mod_group_chat_joins,
               mod_group_chat_departs],
    wocky_metrics:setup_spiral(Metrics).
