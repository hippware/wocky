%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Utility module for wocky bots. Contains functions used by
%%% multiple bot implementation modules
%%%
-module(wocky_bot_util).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-compile({parse_transform, fun_chain}).

-include("wocky.hrl").

-export([get_user_from_jid/1,
         get_bot_from_jid/1,
         get_bot_from_jid/2,
         get_bot_from_node/1,
         get_bot_from_node/2,
         get_bot/1,
         get_bot/2,
         owner_jid/1,
         check_owner/2,
         check_access/2,
         get_id_from_node/1,
         get_id_from_fields/1,
         get_image/1,
         bot_packet_action/1,
         follow_stanza/2,
         bot_action_el/2
        ]).


get_user_from_jid(JID) ->
    case ?wocky_user:get_by_jid(JID) of
        nil -> {error, ?ERR_FORBIDDEN};
        User -> {ok, User}
    end.

get_bot_from_jid(JID) -> get_bot_from_jid(JID, false).
get_bot_from_jid(JID, IncludePending) ->
    BotID = ?wocky_bot:get_id_from_jid(JID),
    get_bot(BotID, IncludePending).

get_bot_from_node(NodeOrAttrs) -> get_bot_from_node(NodeOrAttrs, false).
get_bot_from_node(Node, IncludePending) when is_binary(Node) ->
    do([error_m ||
        BotID <- get_id_from_node_value(Node),
        get_bot(BotID, IncludePending)
      ]);
get_bot_from_node(Attrs, IncludePending) when is_list(Attrs) ->
    do([error_m ||
        BotID <- get_id_from_node(Attrs),
        get_bot(BotID, IncludePending)
      ]).

get_id_from_node(Attrs) ->
    case wocky_xml:get_attr(<<"node">>, Attrs) of
        {error, E} -> {error, E};
        {ok, NodeValue} -> get_id_from_node_value(NodeValue)
    end.

get_id_from_node_value(Node) ->
    case ?wocky_bot:get_id_from_node(Node) of
        nil -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid bot node">>)};
        ID -> {ok, ID}
    end.

get_id_from_fields(#xmlel{name = <<"bot">>, children = Fields}) ->
    get_id_from_fields(Fields);
get_id_from_fields([El = #xmlel{name = <<"field">>} | Rest]) ->
    case xml:get_tag_attr_s(<<"var">>, El) of
        <<"id">> ->
            get_field_value(El);
        _ -> get_id_from_fields(Rest)
    end;
get_id_from_fields([_ | Rest]) ->
    get_id_from_fields(Rest);
get_id_from_fields(_) ->
    <<>>.

get_field_value(El) ->
    case wocky_xml:get_subel_cdata(<<"value">>, El) of
        {ok, Value} -> Value;
        {error, _} -> <<>>
    end.

get_bot(ID) -> get_bot(ID, false).
get_bot(ID, IncludePending) ->
    case ?wocky_bot:get(ID, IncludePending) of
        nil -> {error, ?ERR_ITEM_NOT_FOUND};
        Bot -> {ok, Bot}
    end.

owner_jid(Bot) ->
    ?wocky_user:to_jid(?wocky_bot:owner(Bot)).

check_owner(NoBot, _) when NoBot =:= nil orelse NoBot =:= undefined -> ok;
check_owner(BotID, User) when is_binary(BotID) ->
    check_owner(?wocky_repo:get_by(?wocky_bot, [{id, BotID}]), User);
check_owner(#{user_id := UserID}, #{id := UserID}) -> ok;
check_owner(#{user_id := UserID}, #jid{luser = UserID}) -> ok;
check_owner(#{user_id := UserID}, UserID) -> ok;
check_owner(_, _) -> {error, ?ERR_FORBIDDEN}.

check_access(NoUser, _) when NoUser =:= nil orelse NoUser =:= undefined ->
    {error, ?ERR_FORBIDDEN};
check_access(User = #{id := UserID}, Bot = #{user_id := BotOwnerID}) ->
    case ?wocky_user:'can_access?'(User, Bot) of
        true ->
            case ?wocky_blocking:'blocked?'(UserID, BotOwnerID) of
                true -> {error, ?ERR_ITEM_NOT_FOUND};
                false -> ok
            end;
        false ->
            {error, ?ERR_FORBIDDEN}
    end.

get_image(Entry) when is_binary(Entry) ->
    case exml:parse(Entry) of
        {ok, EntryXML} -> get_image(EntryXML);
        _ -> <<>>
    end;
get_image(Entry = #xmlel{}) ->
    case xml:get_subtag(Entry, <<"image">>) of
        #xmlel{children = [#xmlcdata{content = C}]} -> C;
        false -> <<>>
    end.

bot_packet_action(BotStanza) ->
    Action = xml:get_path_s(BotStanza, [{elem, <<"action">>}, cdata]),
    JIDBin = xml:get_path_s(BotStanza, [{elem, <<"jid">>}, cdata]),

    case jid:from_binary(JIDBin) of
        JID = #jid{} -> bot_packet_action(JID, Action);
        error -> {none, none}
    end.

bot_packet_action(JID, Action) ->
    case {?wocky_bot:get(JID), Action} of
        {nil, _}                   -> {none, none};
        {Bot, <<"show">>}          -> {Bot, show};
        {Bot, <<"share">>}         -> {Bot, share};
        {Bot, <<"enter">>}         -> {Bot, enter};
        {Bot, <<"exit">>}          -> {Bot, exit};
        {Bot, <<"follow on">>}     -> {Bot, follow_on};
        {Bot, <<"follow off">>}    -> {Bot, follow_off};
        {Bot, <<"follow expire">>} -> {Bot, follow_expire};
        _                          -> {none, none}
    end.

follow_stanza(Bot, Action) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [bot_action_el(Bot, Action)]}.

-spec bot_action_el(?wocky_bot:t(), binary() | {binary(), binary()}) ->
    jlib:xmlel().
bot_action_el(#{id := ID, server := Server} = Bot, Action) ->
    #xmlel{name = <<"bot">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = [wocky_xml:cdata_el(
                         <<"jid">>, jid:to_binary(?wocky_bot:to_jid(Bot))),
                       wocky_xml:cdata_el(<<"id">>, ID),
                       wocky_xml:cdata_el(<<"server">>, Server) |
                       action_els(Action)]}.

action_els({Action = <<"follow expire">>, Expiry}) ->
    [wocky_xml:cdata_el(<<"action">>, Action),
     wocky_xml:cdata_el(<<"expiry">>, Expiry)];
action_els(Action) when is_binary(Action) ->
    [wocky_xml:cdata_el(<<"action">>, Action)].
