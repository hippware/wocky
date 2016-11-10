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

-define(SUBSCRIPTION_TABLE, home_stream_subscriptions).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    _ = ets:new(?SUBSCRIPTION_TABLE,
                [named_table, public, {read_concurrency, true}]),
    wocky_publishing_handler:register(?HOME_STREAM_NODE, ?MODULE),
    ejabberd_hooks:add(filter_local_packet, Host,
                       fun filter_local_packet_hook/1, 40),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(filter_local_packet, Host,
                          fun filter_local_packet_hook/1, 40),
    wocky_publishing_handler:unregister(?HOME_STREAM_NODE, ?MODULE),
    ets:delete(?SUBSCRIPTION_TABLE),
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
    SimpleJID = jid:to_lower(User),
    ets:insert(?SUBSCRIPTION_TABLE, SimpleJID),
    maybe_send_catchup(User, Version),
    ok.

-spec unavailable(ejabberd:jid()) -> ok.
unavailable(User) ->
    SimpleJID = jid:to_lower(User),
    ets:delete_object(?SUBSCRIPTION_TABLE, SimpleJID),
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
    _ = do([error_m ||
            wocky_xml:check_attr(<<"type">>, <<"chat">>, Stanza),
            check_server(LServer),
            publish(jid:to_bare(To), From,
                    chat_home_stream_id(From, Stanza), Stanza)]),
    P;

%% Other types of packets we want to go to the home stream should be
%% matched and inserted here

filter_local_packet_hook(Other) -> Other.

%%%===================================================================
%%% Helpers
%%%===================================================================

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
    Subscriptions = ets:match_object(?SUBSCRIPTION_TABLE, {U, S, '_'}),
    lists:foreach(send_notification(_, Item), Subscriptions).

send_notification(UserSimpleJID, Item) ->
    wocky_publishing_handler:send_notification(jid:make(UserSimpleJID),
                                               ?HOME_STREAM_NODE,
                                               Item).

check_server(Server) ->
    case wocky_app:server() of
        Server -> ok;
        _ -> {error, not_local_server}
    end.

%% We don't want bot sharing messages to overwrite other messsage from the user
%% in the home stream, so give them a special ID including the bot as the
%% resource part.
%% All others chats from a user are given the same ID so that the newest
%% overwrites older ones.
chat_home_stream_id(From, Stanza) ->
    case xml:get_path_s(Stanza, [{elem, <<"bot">>}, {elem, <<"jid">>}, cdata]) of
        <<>> ->
            jid:to_binary(jid:to_bare(From));
        JID ->
            jid:to_binary(jid:replace_resource(From, JID))
    end.
