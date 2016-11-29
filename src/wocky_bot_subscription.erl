%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bots - subscription management operations
%%% See https://github.com/hippware/tr-wiki/wiki/Bot
%%%
-module(wocky_bot_subscription).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").
-include("wocky_bot.hrl").

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-export([start/1,
         stop/1]).

-export([handle_subscribe/3,
         handle_unsubscribe/3,
         handle_retrieve_subscribers/3
        ]).

%%%===================================================================
%%% Setup
%%%===================================================================

start(Host) ->
    %% This call may fail if the database hasn't been updated yet. Isolate
    %% it in a temporary process so that the application can start.
    spawn(fun () -> wocky_db_bot:clear_temporary_subscriptions(node()) end),
    ejabberd_hooks:add(node_cleanup, global, fun node_cleanup_hook/1, 90),
    ejabberd_hooks:add(sm_remove_connection_hook, Host,
                       fun remove_connection_hook/4, 90),
    ejabberd_hooks:add(user_send_packet, Host,
                       fun user_send_packet_hook/3, 90).

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host,
                          fun user_send_packet_hook/3, 90),
    ejabberd_hooks:delete(node_cleanup, global, fun node_cleanup_hook/1, 90),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host,
                          fun remove_connection_hook/4, 90),
    wocky_db_bot:clear_temporary_subscriptions(node()).

%%%===================================================================
%%% Action - subscribe
%%%===================================================================

handle_subscribe(From, #jid{lserver = Server}, Attrs) ->
    do([error_m ||
        ID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_access(Server, ID, From),
        subscribe_bot(Server, ID, From),
        {ok, []}
       ]).

subscribe_bot(Server, ID, From) ->
    {ok, wocky_db_bot:subscribe(Server, ID, From)}.

%%%===================================================================
%%% Action - unsubscribe
%%%===================================================================

handle_unsubscribe(From, #jid{lserver = Server}, Attrs) ->
    do([error_m ||
        ID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_bot_exists(Server, ID),
        unsubscribe_bot(Server, ID, From),
        {ok, []}
       ]).

unsubscribe_bot(Server, ID, From) ->
    {ok, wocky_db_bot:unsubscribe(Server, ID, From)}.

%%%===================================================================
%%% Action - retrieve subscribers
%%%===================================================================

handle_retrieve_subscribers(From, #jid{lserver = Server}, Attrs) ->
    do([error_m ||
        ID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_owner(Server, ID, From),
        {ok, make_subscribers_element(Server, ID)}
       ]).

make_subscribers_element(Server, ID) ->
    Subscribers = wocky_db_bot:subscribers(Server, ID),
    #xmlel{name = <<"subscribers">>,
           attrs = wocky_bot_util:list_attrs(ID, Subscribers),
           children = make_subscriber_elements(Subscribers)}.

make_subscriber_elements(Subscribers) ->
    lists:map(fun make_subscriber_element/1, Subscribers).

make_subscriber_element(JID) ->
    #xmlel{name = <<"subscriber">>,
           attrs = [{<<"jid">>, jid:to_binary(JID)}],
           children = [wocky_bot_util:make_follow_element()]}.

%%%===================================================================
%%% Action - subscribe temporary
%%%===================================================================

handle_subscribe_temporary(From, Server, BotID) ->
    case wocky_bot_util:check_access(Server, BotID, From) of
        ok ->
            wocky_db_bot:subscribe_temporary(Server, BotID, From, node());
        _ ->
            ok
    end.

%%%===================================================================
%%% Action - unsubscribe temporary
%%%===================================================================

handle_unsubscribe_temporary(From, Server, BotID) ->
    wocky_db_bot:unsubscribe_temporary(Server, BotID, From).

%%%===================================================================
%%% Hook - node down
%%%===================================================================

node_cleanup_hook(Node) ->
    wocky_db_bot:clear_temporary_subscriptions(Node).

%%%===================================================================
%%% Hook - client connection down
%%%===================================================================

remove_connection_hook(_SID, JID, _Info, _Reason) ->
    wocky_db_bot:clear_temporary_subscriptions(JID).

%%%===================================================================
%%% Hook - client connection down
%%%===================================================================

user_send_packet_hook(From,
                      #jid{user = <<>>, lserver = LServer,
                           resource= <<"bot/", BotID/binary>>},
                      Stanza = #xmlel{name = <<"presence">>, attrs = Attrs}) ->
    case xml:get_attr(<<"type">>, Attrs) of
        {value, <<"unavailable">>} ->
            handle_unsubscribe_temporary(From, LServer, BotID);
        _ ->
            handle_untyped_presence(From, LServer, BotID, Stanza)
    end;
user_send_packet_hook(_, _, _) -> ok.

handle_untyped_presence(From, LServer, BotID, Stanza) ->
    case xml:get_path_s(Stanza, [{elem, <<"query">>}, {attr, <<"xmlns">>}]) of
        ?NS_BOT ->
            handle_subscribe_temporary(From, LServer, BotID);
        _ ->
            ok
    end.
