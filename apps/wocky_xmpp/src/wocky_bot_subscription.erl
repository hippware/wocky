%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bots - subscription management operations
%%% See https://github.com/hippware/tr-wiki/wiki/Bot
%%%
-module(wocky_bot_subscription).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").

-export([subscribe/2,
         unsubscribe/2,
         retrieve_subscribers/1]).

%%%===================================================================
%%% Action - subscribe
%%%===================================================================

subscribe(User, Bot) ->
    ?wocky_subscription:put(User, Bot),
    {ok, make_subscriber_count_element(Bot)}.

%%%===================================================================
%%% Action - unsubscribe
%%%===================================================================

unsubscribe(User, Bot) ->
    ?wocky_subscription:delete(User, Bot),
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
