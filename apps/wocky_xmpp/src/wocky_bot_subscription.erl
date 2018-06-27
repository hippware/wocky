%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bots - subscription management operations
%%% See https://github.com/hippware/tr-wiki/wiki/Bot
%%%
-module(wocky_bot_subscription).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").

-export([subscribe/3,
         unsubscribe/2,
         retrieve_subscribers/2,
         get_sub_count/1,
         get_visitor_count/1,
         get_guest_count/1
        ]).

-define(DEFAULT_MAX_SUBS, 50).

%%%===================================================================
%%% Action - subscribe
%%%===================================================================

subscribe(User, Bot, Guest) ->
    ?wocky_bot:subscribe(Bot, User, Guest),
    {ok, make_subscriber_count_element(Bot)}.

%%%===================================================================
%%% Action - unsubscribe
%%%===================================================================

unsubscribe(User, Bot) ->
    case ?wocky_bot:unsubscribe(Bot, User) of
        ok ->
            {ok, make_subscriber_count_element(Bot)};
        {error, _} ->
            {error, ?ERR_FORBIDDEN}
    end.

%%%===================================================================
%%% Action - retrieve subscribers
%%%===================================================================

retrieve_subscribers(Bot, IQ) ->
    RSM = rsm_util:get_rsm(IQ, #rsm_in{max = ?DEFAULT_MAX_SUBS}),
    {ok, make_subscribers_element(Bot, RSM)}.

make_subscribers_element(Bot, RSM) ->
    {Results, RSMOut} = ?wocky_rsm_helper:rsm_query(
                           RSM, ?wocky_bot:subscribers_query(Bot, false),
                           id, {asc, handle}),
    #xmlel{name = <<"subscribers">>,
           attrs = list_attrs(Bot),
           children = make_subscriber_elements(Results) ++
                      jlib:rsm_encode(RSMOut)}.

make_subscriber_elements(Subscribers) ->
    lists:map(fun make_subscriber_element/1, Subscribers).

make_subscriber_element(User) ->
    #xmlel{name = <<"subscriber">>,
           attrs = [{<<"jid">>, jid:to_binary(?wocky_user:to_jid(User))}]}.

%%%===================================================================
%%% Common helpers
%%%===================================================================

make_subscriber_count_element(Bot) ->
    Count = get_sub_count(Bot),
    wocky_xml:cdata_el(<<"subscriber_count">>, integer_to_binary(Count)).

list_attrs(Bot) ->
    [{<<"xmlns">>, ?NS_BOT},
     {<<"node">>, ?wocky_bot:make_node(Bot)},
     {<<"size">>, integer_to_binary(get_sub_count(Bot))}].

get_sub_count(Bot) ->
    ?wocky_query_utils:get_count(
       ?wocky_bot:subscribers_query(Bot, false), user_id).

get_visitor_count(Bot) ->
    ?wocky_query_utils:get_count(?wocky_bot:visitors_query(Bot), user_id).

get_guest_count(Bot) ->
    ?wocky_query_utils:get_count(?wocky_bot:guests_query(Bot), user_id).
