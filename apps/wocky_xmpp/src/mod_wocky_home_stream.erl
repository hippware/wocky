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

-export([start/2,
         stop/1,
         deps/2,

         send_notifications/2
        ]).

% wocky_publishing_handler exports
-export([publish/4,
         delete/2,
         get/3,
         catchup/3,
         subscribe/3,
         unsubscribe/2
        ]).

-define(WATCHER_CLASS, home_stream).
-define(PACKET_FILTER_PRIORITY, 50).
-define(NO_CATCHUP_BEFORE, <<"2017-01-01T00:00:00Z">>).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    ?wocky_xmpp_home_stream_item_callbacks:register(),
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

deps(_Host, _Opts) ->
    [{mod_wocky_publishing, hard}].

%%%===================================================================
%%% Publishing callback API
%%%===================================================================

-spec publish(ejabberd:jid(),
              ejabberd:jid(),
              ?wocky_home_stream_id:t() | pub_item_id(),
              published_stanza()) -> ok.
publish(TargetJID, From, ID, Stanza) when is_binary(ID) ->
    publish(TargetJID, From, {ID, nil, nil, nil}, Stanza);
publish(#jid{luser = UserID},
        From, {ID, RefUser, RefBot, RefItem}, Stanza) ->
    {ok, _} = ?wocky_home_stream_item:put(UserID, ID,
                                          jid:to_binary(From),
                                          exml:to_binary(Stanza),
                                          [{ref_user_id, get_id(RefUser)},
                                           {ref_bot_id, get_id(RefBot)},
                                           {ref_bot_item_id, get_id(RefItem)}]
                                         ),
    ok.

-spec delete(ejabberd:jid(), pub_item_id()) -> ok.
delete(#jid{luser = User}, ID) ->
    {ok, _} = ?wocky_home_stream_item:delete(User, ID),
    ok.

-spec get(ejabberd:jid(), ejabberd:jid(),
          jlib:rsm_in() | pub_item_id()) -> pub_get_result().
get(#jid{luser = User}, FromJID = #jid{luser = User}, RSMIn = #rsm_in{}) ->
    Query = ?wocky_home_stream_item:get_query(User, [{include_deleted, false}]),
    {Results, RSMOut} =
      ?wocky_rsm_helper:rsm_query(RSMIn, Query, key, {asc, updated_at}),
    {ok, {[map_to_item(I) || I <- Results],
          format_version(?wocky_home_stream_item:get_latest_version(User)),
          extra_data(Results, FromJID),
          RSMOut}};

get(#jid{luser = User}, FromJID = #jid{luser = User}, ID) ->
    Item = ?wocky_home_stream_item:get_by_key(User, ID, true),
    case Item of
        nil ->
            {ok, not_found};
        _ ->
            {ok, {map_to_item(Item),
                  format_version(
                    ?wocky_home_stream_item:get_latest_version(User)),
                  extra_data([Item], FromJID)}}
    end;
get(_, _, _) ->
    {error, ?ERR_FORBIDDEN}.

-spec catchup(ejabberd:jid(), ejabberd:jid(), pub_version())
-> pub_catchup_result().
catchup(#jid{luser = User}, FromJID = #jid{luser = User}, Version) ->
    case ?wocky_timestamp:from_string(Version) of
        {ok, TS} -> do_catchup(User, FromJID, TS);
        {error, _} -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid version">>)}
    end;

catchup(_, _, _) ->
    {error, ?ERR_FORBIDDEN}.

do_catchup(User, FromJID, Version) ->
    LatestVersion = format_version(
                      ?wocky_home_stream_item:get_latest_version(User)),
    CatchupRows = ?wocky_home_stream_item:get_after_time(
                     User, Version, catchup_limit() + 1),

    case should_send_catchup(Version, CatchupRows) of
        true ->
            {ok, {[map_to_item(R) || R <- CatchupRows],
                  LatestVersion,
                  extra_data(CatchupRows, FromJID)
                 }};
        false ->
            {error, ?ERRT_NOT_ACCEPTABLE(
                       ?MYLANG, <<"Would return too many rows">>)}
    end.

-spec subscribe(ejabberd:jid(), ejabberd:jid(), pub_version())
-> pub_sub_result().
subscribe(#jid{luser = ToID}, User = #jid{luser = ToID}, Version) ->
    wocky_watcher:watch(?WATCHER_CLASS, User, hs_node(User)),
    maybe_send_async_catchup(User, Version);
subscribe(_, _, _) ->
    {error, ?ERR_FORBIDDEN}.

-spec unsubscribe(ejabberd:jid(), ejabberd:jid()) -> ok.
unsubscribe(TargetJID, UserJID) ->
    wocky_watcher:unwatch(?WATCHER_CLASS, UserJID, hs_node(TargetJID)),
    ok.

%%%===================================================================
%%% Packet filtering API
%%%===================================================================

-type filter_packet() :: {ejabberd:jid(), ejabberd:jid(),
                          mongoose_acc:t(), jlib:xmlel()}.
-spec filter_local_packet_hook(filter_packet() | drop) ->
    filter_packet() | drop.
filter_local_packet_hook(P = {From, To = #jid{lserver = LServer},
                              _Acc, Stanza = #xmlel{name = <<"message">>}}) ->
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

-type publish_result() :: {publish, {drop | keep, ?wocky_home_stream_id:t()}}.

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
    ItemID = xml:get_path_s(Stanza, [{elem, <<"event">>},
                                     {elem, <<"item">>},
                                     {attr, <<"id">>}]),
    {Result, Bot} = wocky_bot_util:get_bot_from_node(BotNode),

    case EventNS of
        ?NS_BOT_EVENT when Result =:= ok ->
            Item = ?wocky_item:get(Bot, ItemID),
            {publish, {drop, ?wocky_home_stream_id:bot_event_id(Bot, Item)}};
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
                                          Bot, User, <<"enter">>)};
        {Bot, exit}          -> {drop, ?wocky_home_stream_id:bot_event_id(
                                          Bot, User, <<"exit">>)};
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

map_to_item(#{class := ref_update,
              id := ItemID,
              updated_at := Version,
              from_jid := FromJIDBin,
              reference_bot := Bot
             }) ->
    #published_item{
       id = integer_to_binary(ItemID),
       type = <<"reference-changed">>,
       version = format_version(Version),
       from = jid:from_binary(FromJIDBin),
       stanza = wocky_bot_util:bot_action_el(Bot, <<"changed">>)
      };
map_to_item(#{key := Key, updated_at := UpdatedAt,
              from_jid := FromJID, stanza := StanzaBin,
              class := Class}) ->
    {ok, Stanza} = wocky_xml:parse_multiple(StanzaBin),
    #published_item{id = Key,
                    version = format_version(UpdatedAt),
                    from = jid:from_binary(FromJID),
                    stanza = Stanza,
                    deleted = Class =:= deleted}.

format_version(Time) ->
    ?wocky_timestamp:to_string(Time).

maybe_send_async_catchup(_, undefined) -> ok;
maybe_send_async_catchup(UserJID, Version) ->
    case ?wocky_timestamp:from_string(Version) of
        {ok, TS} -> send_async_catchup(UserJID, TS);
        {error, _} -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid version">>)}
    end.

send_async_catchup(UserJID = #jid{luser = User}, Version) ->
    CatchupRows = ?wocky_home_stream_item:get_after_time(
                     User, Version, catchup_limit() + 1),
    case should_send_catchup(Version, CatchupRows) of
        true ->
            send_async_catchup(CatchupRows, UserJID);
        false ->
            {error, ?ERRT_NOT_ACCEPTABLE(
                       ?MYLANG, <<"Would return too many rows">>)}
    end;

send_async_catchup(CatchupRows, UserJID) when is_list(CatchupRows) ->
    Items = [map_to_item(R) || R <- CatchupRows],
    lists:foreach(
      wocky_publishing_handler:send_notification(
        UserJID,
        jid:replace_resource(UserJID, ?HOME_STREAM_NODE),
        _),
      Items).

-spec send_notifications(ejabberd:jid(),
                         pub_item() | ?wocky_home_stream_item:t()) -> ok.
send_notifications(UserJID, Item = #published_item{}) ->
    Watchers = wocky_watcher:watchers(?WATCHER_CLASS, hs_node(UserJID)),
    lists:foreach(send_notification(_, Item), Watchers);
send_notifications(UserJID, HomeStreamItem) ->
    send_notifications(UserJID, map_to_item(HomeStreamItem)).

send_notification(JID, Item) ->
    wocky_publishing_handler:send_notification(
      JID, jid:replace_resource(JID, ?HOME_STREAM_NODE), Item).

check_server(Server) ->
    case wocky_xmpp_app:server() of
        Server -> ok;
        _ -> {error, not_local_server}
    end.

get_id(nil) -> nil;
get_id(Struct) -> maps:get(id, Struct).

hs_node(UserJID) ->
    jid:replace_resource(UserJID, ?HOME_STREAM_NODE).

extra_data(Items, FromJID) ->
    Bots = lists:usort(
             lists:map(
               maps:get(reference_bot, _), Items)) -- [nil],
    User = ?wocky_user:get_by_jid(FromJID),

    mod_wocky_bot:make_bot_els(Bots, User).

catchup_limit() ->
    ?confex:get_env(wocky_xmpp, catchup_limit).

should_send_catchup(Version, Rows) ->
    length(Rows) < catchup_limit()
    andalso
    ?datetime:compare(
       ?wocky_timestamp:'from_string!'(?NO_CATCHUP_BEFORE), Version) =:= lt.
