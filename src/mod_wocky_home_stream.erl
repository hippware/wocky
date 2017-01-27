%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky home stream
%%% See https://github.com/hippware/tr-wiki/wiki/Home-Stream
%%%
-module(mod_wocky_home_stream).

-behaviour(gen_mod).
-behaviour(wocky_publishing_handler).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-export([start/2, stop/1]).

-export([publish/4,
         delete/2,
         get/2,
         available/2,
         unavailable/1
        ]).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").
-include("wocky_publishing.hrl").

-define(PACKET_FILTER_PRIORITY, 50).
-define(NODE_CLEANUP_PRIORITY, 80).

-record(hs_subscription,
        {jid  :: ejabberd:simple_jid() | {binary(), binary(), '_'} | '_',
         node :: node() | '_'
        }).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    mnesia:create_table(
      hs_subscription, [{ram_copies, [node()]}, {type, set},
                        {attributes, record_info(fields, hs_subscription)}]),
    mnesia:add_table_copy(hs_subscription, node(), ram_copies),

    wocky_publishing_handler:register(?HOME_STREAM_NODE, ?MODULE),
    ejabberd_hooks:add(filter_local_packet, Host,
                       filter_local_packet_hook(_), ?PACKET_FILTER_PRIORITY),
    ejabberd_hooks:add(node_cleanup, global,
                       node_cleanup(_), ?NODE_CLEANUP_PRIORITY),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(node_cleanup, global,
                          node_cleanup(_), ?NODE_CLEANUP_PRIORITY),
    ejabberd_hooks:delete(filter_local_packet, Host,
                          filter_local_packet_hook(_), ?PACKET_FILTER_PRIORITY),
    wocky_publishing_handler:unregister(?HOME_STREAM_NODE, ?MODULE),
    ok.

%%%===================================================================
%%% Publishing callback API
%%%===================================================================

-spec publish(ejabberd:jid(), ejabberd:jid(), pub_item_id(),
              published_stanza()) -> ok.
publish(UserJID = #jid{luser = User, lserver = Server}, From, ID, Stanza) ->
    ItemMap = wocky_db_home_stream:publish(User, Server, ID, Stanza, From),
    send_notifications(UserJID, map_to_item(ItemMap)).

-spec delete(ejabberd:jid(), pub_item_id()) -> ok.
delete(UserJID = #jid{luser = User, lserver = Server}, ID) ->
    ItemMap = wocky_db_home_stream:delete(User, Server, ID),
    send_notifications(UserJID, map_to_item(ItemMap)).

-spec get(ejabberd:jid(), jlib:rsm_in() | pub_item_id()) ->
    {ok, {[published_item()], pub_version(), jlib:rsm_out()}} |
    {ok, {published_item(), pub_version()} | not_found}.
get(#jid{luser = User, lserver = Server}, RSMIn = #rsm_in{}) ->
    Items = wocky_db_home_stream:get(User, Server),
    {Filtered, RSMOut} = rsm_util:filter_with_rsm(Items, RSMIn),
    {ok, {[map_to_item(I) || I <- Filtered],
          version_from_map(Items),
          RSMOut}};

get(#jid{luser = User, lserver = Server}, ID) ->
    Item = wocky_db_home_stream:get(User, Server, ID),
    case Item of
        not_found ->
            {ok, not_found};
        _ ->
            {ok, {map_to_item(Item),
                  wocky_db_home_stream:current_version(User, Server)}}
    end.

-spec available(ejabberd:jid(), pub_version()) -> ok.
available(User, Version) ->
    mnesia:dirty_write(make_record(User)),
    maybe_send_catchup(User, Version),
    ok.

-spec unavailable(ejabberd:jid()) -> ok.
unavailable(User) ->
    mnesia:dirty_delete_object(make_record(User)),
    ok.

make_record(User) ->
    #hs_subscription{jid = jid:to_lower(User),
                     node = node()}.

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

check_publish(From, Stanza) ->
    case xml:get_tag_attr(<<"type">>, Stanza) of
        {value, <<"chat">>} -> {ok, {keep, chat_id(From)}};
        {value, <<"headline">>} -> check_publish_headline(From, Stanza);
        _ -> {error, dont_publish}
    end.

check_publish_headline(From, Stanza) ->
    case xml:get_subtag(Stanza, <<"bot">>) of
        false -> check_publish_non_bot(From, Stanza);
        BotEl -> check_publish_bot(From, BotEl)
    end.

check_publish_non_bot(From, Stanza) ->
    EventNS = xml:get_path_s(Stanza, [{elem, <<"event">>},
                                      {attr, <<"xmlns">>}]),
    BotNode = xml:get_path_s(Stanza, [{elem, <<"event">>},
                                      {attr, <<"node">>}]),
    case {EventNS, BotNode} of
        {?NS_BOT_EVENT, _} when BotNode =/= <<>> ->
            {ok, {drop, bot_event_id(From, BotNode)}};
        {?NS_PUBSUB_EVENT, _} ->
            {error, dont_publish};
        _ ->
            {ok, {keep, chat_id(From)}}
    end.

check_publish_bot(From, BotEl) ->
    case wocky_bot_util:bot_packet_action(BotEl) of
        {none, none}            -> {ok, {keep, chat_id(From)}};
        {JIDBin, show}          -> {ok, {drop, bot_id(JIDBin)}};
        {JIDBin, share}         -> {ok, {drop, bot_id(JIDBin)}};
        {_JIDBin, enter}        -> {ok, {drop, new_id()}};
        {_JIDBin, exit}         -> {ok, {drop, new_id()}};
        {JIDBin, follow_on}     -> {ok, {drop, jid_event_id(
                                                  JIDBin, <<"follow_on">>)}};
        {JIDBin, follow_off}    -> {ok, {drop, jid_event_id(
                                                  JIDBin, <<"follow_off">>)}};
        {JIDBin, follow_expire} -> {ok, {drop, jid_event_id(
                                                 JIDBin, <<"follow_expiry">>)}}
    end.

maybe_drop({ok, drop}, _) -> drop;
maybe_drop(_, P) -> P.

%%%===================================================================
%%% Cleanup hook
%%%===================================================================

node_cleanup(Node) ->
    ToClean = mnesia:dirty_match_object(#hs_subscription{node = Node, _ = '_'}),
    lists:foreach(mnesia:dirty_delete_object(_), ToClean).

%%%===================================================================
%%% Helpers
%%%===================================================================

new_id() ->
    wocky_db:create_id().

map_to_item(#{id := ID, version := Version,
              from := From, stanza := Stanza,
              deleted := Deleted}) ->
    #published_item{id = ID,
                    version = Version,
                    from = From,
                    stanza = Stanza,
                    deleted = Deleted}.

version_from_map([]) -> not_found;
version_from_map([#{version := Version} | _]) -> Version.

maybe_send_catchup(_, undefined) -> ok;
maybe_send_catchup(UserJID = #jid{luser = User, lserver = Server}, Version) ->
    CatchupRows = wocky_db_home_stream:get_catchup(User, Server, Version),
    Items = [map_to_item(R) || R <- CatchupRows],
    lists:foreach(
      wocky_publishing_handler:send_notification(UserJID, ?HOME_STREAM_NODE, _),
      Items).

send_notifications(User, Item) ->
    {U, S} = jid:to_lus(User),
    Subscriptions = mnesia:dirty_match_object(
                      #hs_subscription{jid = {U, S, '_'}, _ = '_'}),
    lists:foreach(send_notification(_, Item), Subscriptions).

send_notification(#hs_subscription{jid = BareJID}, Item) ->
    wocky_publishing_handler:send_notification(jid:make(BareJID),
                                               ?HOME_STREAM_NODE,
                                               Item).

check_server(Server) ->
    case wocky_app:server() of
        Server -> ok;
        _ -> {error, not_local_server}
    end.

bot_id(JIDBin) ->
    JIDBin.

jid_event_id(JIDBin, Event) ->
    <<JIDBin/binary, "/", Event/binary>>.

bot_event_id(#jid{lserver = Server}, BotNode) ->
    jid:to_binary(jid:make(<<>>, Server, <<BotNode/binary, "/event">>)).

chat_id(From) ->
    jid:to_binary(jid:to_bare(From)).
