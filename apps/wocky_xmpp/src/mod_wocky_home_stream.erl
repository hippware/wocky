%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky home stream
%%% See https://github.com/hippware/tr-wiki/wiki/Home-Stream
%%%
-module(mod_wocky_home_stream).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").
-include("wocky_publishing.hrl").

-behaviour(gen_mod).
-behaviour(wocky_publishing_handler).

-export([start/2, stop/1]).

% wocky_publishing_handler exports
-export([publish/4,
         delete/2,
         get/4,
         subscribe/3,
         unsubscribe/1
        ]).

-define(WATCHER_CLASS, home_stream).
-define(PACKET_FILTER_PRIORITY, 50).
-define(NODE_RESOURCE, <<"home_stream">>).


%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    wocky_publishing_handler:register(?HOME_STREAM_NODE, ?MODULE),
    wocky_watcher:register(?WATCHER_CLASS, Host),
    ejabberd_hooks:add(filter_local_packet, Host,
                       filter_local_packet_hook(_), ?PACKET_FILTER_PRIORITY),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(filter_local_packet, Host,
                          filter_local_packet_hook(_), ?PACKET_FILTER_PRIORITY),
    wocky_watcher:unregister(?WATCHER_CLASS, Host),
    wocky_publishing_handler:unregister(?HOME_STREAM_NODE, ?MODULE),
    ok.

%%%===================================================================
%%% Publishing callback API
%%%===================================================================

-spec publish(ejabberd:jid(),
              ejabberd:jid(),
              ?wocky_home_stream_id:id() | pub_item_id(),
              published_stanza()) -> ok.
publish(TargetJID, From, ID, Stanza) when is_binary(ID) ->
    publish(TargetJID, From, {ID, nil, nil}, Stanza);
publish(TargetJID = #jid{luser = UserID},
        From, {ID, RefUser, RefBot}, Stanza) ->
    {ok, ItemMap} = ?wocky_home_stream_item:put(UserID, ID,
                                                jid:to_binary(From),
                                                exml:to_binary(Stanza),
                                                [{ref_user_id, get_id(RefUser)},
                                                 {ref_bot_id, get_id(RefBot)}]
                                               ),
    send_notifications(TargetJID, map_to_item(ItemMap)),
    ok.

-spec delete(ejabberd:jid(), pub_item_id()) -> ok.
delete(UserJID = #jid{luser = User}, ID) ->
    {ok, ItemMap} = ?wocky_home_stream_item:delete(User, ID),
    send_notifications(UserJID, map_to_item(ItemMap)).

-spec get(ejabberd:jid(), ejabberd:jid(),
          jlib:rsm_in() | pub_item_id(), boolean()) -> pub_get_result().
get(#jid{luser = User}, #jid{luser = User},
    RSMIn = #rsm_in{}, ExcludeDeleted) ->
    Query = ?wocky_home_stream_item:maybe_exclude_deleted(
               ?wocky_home_stream_item:with_user(User), ExcludeDeleted),
    {Results, RSMOut} =
      ?wocky_rsm_helper:rsm_query(RSMIn, Query, key, {asc, created_at}),
    {ok, {[map_to_item(I) || I <- Results],
          format_version(?wocky_home_stream_item:get_latest_time(User)),
          RSMOut}};

get(#jid{luser = User}, #jid{luser = User}, ID, ExcludeDeleted) ->
    Item = ?wocky_home_stream_item:get_by_key(User, ID, ExcludeDeleted),
    case Item of
        nil ->
            {ok, not_found};
        _ ->
            {ok, {map_to_item(Item),
                  format_version(
                    ?wocky_home_stream_item:get_latest_time(User))}}
    end;
get(_, _, _, _) ->
    {error, ?ERR_FORBIDDEN}.

-spec subscribe(ejabberd:jid(), ejabberd:jid(), pub_version()) -> ok.
subscribe(#jid{luser = ToID}, User = #jid{luser = ToID}, Version) ->
    wocky_watcher:watch(?WATCHER_CLASS, User, hs_node(User)),
    maybe_send_catchup(User, Version),
    ok;
subscribe(_, _, _) ->
    {error, ?ERR_FORBIDDEN}.

-spec unsubscribe(ejabberd:jid()) -> ok.
unsubscribe(User) ->
    wocky_watcher:unwatch(?WATCHER_CLASS, User, hs_node(User)),
    ok.

%%%===================================================================
%%% Packet filtering API
%%%===================================================================

-type filter_packet() :: {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.
-spec filter_local_packet_hook(filter_packet() | drop) ->
    filter_packet() | drop.
filter_local_packet_hook(P = {From,
                              To = #jid{lserver = LServer},
                              Stanza = #xmlel{name = <<"message">>}}) ->
    Result = do([error_m ||
                 check_server(LServer),
                 check_user_present(To),
                 {Action, ID} <- check_publish(From, Stanza),
                 publish(jid:to_bare(To), From, ID, Stanza),
                 {ok, Action}
                ]),
    maybe_drop(Result, P);

%% Other types of packets we want to go to the home stream should be
%% matched and inserted here

filter_local_packet_hook(Other) -> Other.

%%%===================================================================
%%% Packet filtering functions
%%%===================================================================

check_user_present(#jid{luser = <<>>}) -> {error, no_user};
check_user_present(#jid{luser = _}) -> ok.

%% Home stream publishing checks are performed in order. They may return one
%% of 3 values:
%%
%% * `continue` - continues on to the next check
%% * `dont_publish` - stops checking immediately and does not publish
%% * `{publish, {Action, ID}}` - publishes the packet with the supplied ID
%%                               Action may be `drop` or `keep` and controls
%%                               whether the packet is dropped from further
%%                               routing.

-type publish_result() :: {publish, {drop | keep, ?wocky_home_stream_id:id()}}.

-type publish_check_cb_result() :: continue | dont_publish | publish_result().

-type publish_check_cb() ::
        fun((ejabberd:jid(), jlib:xmlel()) -> publish_check_cb_result()).

-spec publish_checks() -> [publish_check_cb()].
publish_checks() ->
    [
     fun check_publish_skip_notification/2,
     fun check_publish_headline_message/2,
     fun check_publish_bot/2,
     fun check_publish_bot_description/2,
     fun check_publish_event/2
    ].

check_publish(From, Stanza) ->
    check_publish(publish_checks(), From, Stanza).

-spec check_publish([publish_check_cb()], ejabberd:jid(), jlib:xmlel()) ->
    {ok, publish_result()} | {error, dont_publish}.
check_publish([], _From, _Stanza) ->
    {error, dont_publish};
check_publish([Check | Rest], From, Stanza) ->
    case Check(From, Stanza) of
        dont_publish ->
            {error, dont_publish};
        {publish, {Action, ID}} ->
            {ok, {Action, ID}};
        continue ->
            check_publish(Rest, From, Stanza)
    end.

%% Very important: Filter out publishing notification events -
%% the home stream generates these and we don't want to get caught in a loop.
check_publish_skip_notification(_From, Stanza) ->
    NotificationNS = xml:get_path_s(Stanza, [{elem, <<"notification">>},
                                             {attr, <<"xmlns">>}]),
    case NotificationNS of
        ?NS_PUBLISHING ->
            dont_publish;
        _  ->
            continue
    end.

check_publish_headline_message(_From, Stanza = #xmlel{name = <<"message">>}) ->
    case xml:get_tag_attr(<<"type">>, Stanza) of
        {value, <<"headline">>} ->
            continue;
        _ ->
            dont_publish
    end;
check_publish_headline_message(_From, _Stanza) -> dont_publish.

check_publish_bot(From, Stanza) ->
    case xml:get_subtag(Stanza, <<"bot">>) of
        false ->
            continue;
        BotEl ->
            publish_bot_action(From, BotEl)
    end.

check_publish_bot_description(_From, Stanza) ->
    BotEl = xml:get_path_s(Stanza, [{elem, <<"bot-description-changed">>},
                                    {elem, <<"bot">>}]),
    BotNS = xml:get_path_s(Stanza, [{elem, <<"bot-description-changed">>},
                                    {attr, <<"xmlns">>}]),
    BotID = wocky_bot_util:get_id_from_fields(BotEl),
    Bot = case ?wocky_id:'valid?'(BotID) of
              false -> nil;
              true -> ?wocky_bot:get(BotID)
          end,

    case BotNS of
        ?NS_BOT when Bot =/= nil ->
            {publish, {drop, ?wocky_home_stream_id:bot_description_id(Bot)}};
        _ ->
            continue
    end.

check_publish_event(_From, Stanza) ->
    EventNS = xml:get_path_s(Stanza, [{elem, <<"event">>},
                                      {attr, <<"xmlns">>}]),
    BotNode = xml:get_path_s(Stanza, [{elem, <<"event">>},
                                      {attr, <<"node">>}]),
    {Result, Bot} = wocky_bot_util:get_bot_from_node(BotNode),

    case EventNS of
        ?NS_BOT_EVENT when Result =:= ok ->
            {publish, {drop, ?wocky_home_stream_id:bot_event_id(Bot)}};
        ?NS_PUBSUB_EVENT ->
            dont_publish;
        _ ->
            continue
    end.

publish_bot_action(From, BotEl) ->
    User = ?wocky_user:get_by_jid(From),
    Result =
    case wocky_bot_util:bot_packet_action(BotEl) of
        {none, none}         -> {keep, ?wocky_home_stream_id:user_message_id(
                                          User)};
        {Bot, show}          -> {drop, ?wocky_home_stream_id:bot_id(Bot)};
        {Bot, share}         -> {drop, ?wocky_home_stream_id:bot_id(Bot)};
        {Bot, enter}         -> {drop, ?wocky_home_stream_id:bot_event_id(
                                          Bot, <<"enter">>, User)};
        {Bot, exit}          -> {drop, ?wocky_home_stream_id:bot_event_id(
                                          Bot, <<"exit">>, User)};
        {Bot, follow_on}     -> {drop, ?wocky_home_stream_id:bot_event_id(
                                          Bot, <<"follow_on">>)};
        {Bot, follow_off}    -> {drop, ?wocky_home_stream_id:bot_event_id(
                                          Bot, <<"follow_off">>)};
        {Bot, follow_expire} -> {drop, ?wocky_home_stream_id:bot_event_id(
                                          Bot, <<"follow_expiry">>)}
    end,
    {publish, Result}.

maybe_drop({ok, drop}, _) -> drop;
maybe_drop(_, P) -> P.

map_to_item(#{key := Key, updated_at := UpdatedAt,
              from_jid := FromJID, stanza := StanzaBin,
              deleted := Deleted}) ->
    {ok, Stanza} = wocky_xml:parse_multiple(StanzaBin),
    #published_item{id = Key,
                    version = format_version(UpdatedAt),
                    from = jid:from_binary(FromJID),
                    stanza = Stanza,
                    deleted = Deleted}.

format_version(Time) ->
    ?wocky_timestamp:to_string(Time).

maybe_send_catchup(_, undefined) -> ok;
maybe_send_catchup(UserJID = #jid{luser = User}, Version) ->
    CatchupRows = ?wocky_home_stream_item:get_after_time(User, Version),
    Items = [map_to_item(R) || R <- CatchupRows],
    lists:foreach(
      wocky_publishing_handler:send_notification(UserJID, ?HOME_STREAM_NODE, _),
      Items).

send_notifications(UserJID, Item) ->
    Watchers = wocky_watcher:watchers(?WATCHER_CLASS, hs_node(UserJID)),
    lists:foreach(send_notification(_, Item), Watchers).

send_notification(JID, Item) ->
    wocky_publishing_handler:send_notification(JID,
                                               ?HOME_STREAM_NODE,
                                               Item).

check_server(Server) ->
    case wocky_xmpp_app:server() of
        Server -> ok;
        _ -> {error, not_local_server}
    end.

get_id(nil) -> nil;
get_id(Struct) -> maps:get(id, Struct).

hs_node(UserJID) ->
    jid:replace_resource(UserJID, ?NODE_RESOURCE).
