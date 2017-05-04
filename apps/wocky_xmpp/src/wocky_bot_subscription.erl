%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bots - subscription management operations
%%% See https://github.com/hippware/tr-wiki/wiki/Bot
%%%
-module(wocky_bot_subscription).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").

-export([start/1,
         stop/1]).

-export([subscribe/2,
         unsubscribe/2,
         retrieve_subscribers/1]).

-define(wocky_temp_subscription, 'Elixir.Wocky.Bot.TempSubscription').


%%%===================================================================
%%% Setup
%%%===================================================================

start(Host) ->
    %% This call may fail if the database hasn't been updated yet. Isolate
    %% it in a temporary process so that the application can start.
    spawn(fun () -> ?wocky_temp_subscription:delete(node()) end),
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
    ?wocky_temp_subscription:delete(node()).

%%%===================================================================
%%% Action - subscribe
%%%===================================================================

subscribe(User, Bot) ->
    ?wocky_user:subscribe(User, Bot),
    {ok, make_subscriber_count_element(Bot)}.

%%%===================================================================
%%% Action - unsubscribe
%%%===================================================================

unsubscribe(User, Bot) ->
    ?wocky_user:unsubscribe(User, Bot),
    {ok, make_subscriber_count_element(Bot)}.

%%%===================================================================
%%% Action - retrieve subscribers
%%%===================================================================

retrieve_subscribers(Bot) ->
    {ok, make_subscribers_element(Bot)}.

make_subscribers_element(Bot) ->
    Subscribers = ?wocky_bot:subscribers(Bot),
    #xmlel{name = <<"subscribers">>,
           attrs = list_attrs(Bot, Subscribers),
           children = make_subscriber_elements(Subscribers)}.

make_subscriber_elements(Subscribers) ->
    lists:map(fun make_subscriber_element/1, Subscribers).

make_subscriber_element(User) ->
    #xmlel{name = <<"subscriber">>,
           attrs = [{<<"jid">>, jid:to_binary(?wocky_user:to_jid(User))}]}.

%%%===================================================================
%%% Action - subscribe temporary
%%%===================================================================

handle_subscribe_temporary(From, _Server, BotID) ->
    do([error_m ||
        Bot <- wocky_bot_util:get_bot(BotID),
        User <- wocky_bot_util:get_user_from_jid(From),
        do_subscribe_temporary(User, Bot)
       ]).

do_subscribe_temporary(User, Bot) ->
    case wocky_bot_util:check_access(User, Bot) of
        ok -> ?wocky_user:subscribe_temporary(User, Bot, node());
        _ -> ok
    end.

%%%===================================================================
%%% Action - unsubscribe temporary
%%%===================================================================

handle_unsubscribe_temporary(From, _Server, BotID) ->
    do([error_m ||
        Bot <- wocky_bot_util:get_bot(BotID),
        User <- wocky_bot_util:get_user_from_jid(From),
        ?wocky_user:unsubscribe_temporary(User, Bot)
       ]).

%%%===================================================================
%%% Hook - node down
%%%===================================================================

node_cleanup_hook(Node) ->
    ?wocky_temp_subscription:delete(Node).

%%%===================================================================
%%% Hook - client connection down
%%%===================================================================

remove_connection_hook(_SID, JID, _Info, _Reason) ->
    do([error_m ||
        User <- wocky_bot_util:get_user_from_jid(JID),
        ?wocky_temp_subscription:delete(User)
       ]).

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

%%%===================================================================
%%% Common helpers
%%%===================================================================

make_subscriber_count_element(Bot) ->
    Count = ?wocky_bot:subscriber_count(Bot),
    wocky_xml:cdata_el(<<"subscriber_count">>, integer_to_binary(Count)).

list_attrs(Bot, List) ->
    [{<<"xmlns">>, ?NS_BOT},
     {<<"node">>, ?wocky_bot:make_node(Bot)},
     {<<"size">>, integer_to_binary(length(List))},
     {<<"hash">>, wocky_bot_util:list_hash(List)}].
