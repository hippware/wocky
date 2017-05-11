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
         get_bot_from_node/1,
         get_bot/1,
         owner_jid/1,
         check_owner/2,
         check_access/2,
         get_id_from_node/1,
         list_hash/1,
         extract_images/1,
         get_image/1,
         bot_packet_action/1,
         follow_stanza/2
        ]).


get_user_from_jid(JID) ->
    case ?wocky_user:get_by_jid(JID) of
        nil -> {error, ?ERR_FORBIDDEN};
        User -> {ok, User}
    end.

get_bot_from_jid(JID) ->
    BotID = ?wocky_bot:get_id_from_jid(JID),
    get_bot(BotID).

get_bot_from_node(Attrs) ->
    do([error_m ||
        BotID <- get_id_from_node(Attrs),
        get_bot(BotID)
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

get_bot(ID) ->
    case ?wocky_repo:get(?wocky_bot, ID) of
        nil -> {error, ?ERR_ITEM_NOT_FOUND};
        Bot -> {ok, Bot}
    end.

owner_jid(Bot) ->
    ?wocky_user:to_jid(?wocky_bot:owner(Bot)).

check_owner(#{user_id := UserID}, #{id := UserID}) -> ok;
check_owner(#{user_id := UserID}, #jid{luser = UserID}) -> ok;
check_owner(#{user_id := UserID}, UserID) -> ok;
check_owner(_, _) -> {error, ?ERR_FORBIDDEN}.

check_access(User, Bot) ->
    case ?wocky_user:'can_access?'(User, Bot) of
        true -> ok;
        false -> {error, ?ERR_FORBIDDEN}
    end.

list_hash(List) ->
    fun_chain:first(
      List,
      lists:sort(),
      term_to_binary(),
      hash(),
      'Elixir.Base':encode32([{padding, false}])
     ).

hash(Term) ->
    crypto:hash(sha512, Term).

extract_images(Items) ->
    lists:reverse(lists:foldl(extract_image(_, _), [], Items)).

extract_image(#{image := true, id := ID, updated_at := Updated, stanza := S},
              Acc) ->
    case get_image(S) of
        none -> Acc;
        I -> [#{id => ID, updated => Updated, image => I} | Acc]
    end;
extract_image(_, Acc) -> Acc.

get_image(Entry) when is_binary(Entry) ->
    case exml:parse(Entry) of
        {ok, EntryXML} -> get_image(EntryXML);
        _ -> none
    end;
get_image(Entry = #xmlel{}) ->
    case xml:get_subtag(Entry, <<"image">>) of
        #xmlel{children = [#xmlcdata{content = C}]} -> C;
        false -> none
    end.

bot_packet_action(BotStanza) ->
    Action = xml:get_path_s(BotStanza, [{elem, <<"action">>}, cdata]),
    JIDBin = xml:get_path_s(BotStanza, [{elem, <<"jid">>}, cdata]),

    case {JIDBin, Action} of
        {<<>>, _}                     -> {none, none};
        {JIDBin, <<"show">>}          -> {JIDBin, show};
        {JIDBin, <<"share">>}         -> {JIDBin, share};
        {JIDBin, <<"enter">>}         -> {JIDBin, enter};
        {JIDBin, <<"exit">>}          -> {JIDBin, exit};
        {JIDBin, <<"follow on">>}     -> {JIDBin, follow_on};
        {JIDBin, <<"follow off">>}    -> {JIDBin, follow_off};
        {JIDBin, <<"follow expire">>} -> {JIDBin, follow_expire};
        _                             -> {none, none}
    end.

follow_stanza(Bot, Action) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [bot_el(Bot, Action)]}.

bot_el(#{id := ID, server := Server} = Bot, Action) ->
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
