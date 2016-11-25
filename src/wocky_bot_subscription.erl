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

-export([handle_subscribe/3,
         handle_unsubscribe/3,
         handle_retrieve_subscribers/3]).
%    ejabberd_hooks:add(node_cleanup, global, ?MODULE, node_cleanup, 50),
%    ejabberd_hooks:run(sm_remove_connection_hook, JID#jid.lserver,
%                       [SID, JID, Info, Reason]).

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
           attrs = [{<<"jid">>, jid:to_binary(JID)}]}.

