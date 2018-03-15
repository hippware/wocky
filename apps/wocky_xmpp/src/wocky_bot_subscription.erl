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
         adjust_exclude_owner/1
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
    case wocky_bot_util:check_owner(Bot, User) of
        {error, _} ->
            ?wocky_bot:unsubscribe(Bot, User),
            {ok, make_subscriber_count_element(Bot)};
        ok ->
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
    Count = adjust_exclude_owner(?wocky_bot:subscriber_count(Bot)),
    wocky_xml:cdata_el(<<"subscriber_count">>, integer_to_binary(Count)).

list_attrs(Bot = #{subscribers_hash := SubscribersHash,
                   subscribers_count := SubscribersCount}) ->
    [{<<"xmlns">>, ?NS_BOT},
     {<<"node">>, ?wocky_bot:make_node(Bot)},
     % Hack to exclude owner subscription
     {<<"size">>, integer_to_binary(adjust_exclude_owner(SubscribersCount))},
     {<<"hash">>, SubscribersHash}].

% Subtract one from the subscriber count because the client doesn't expect
% us to include the owner's automatic subscription in the count. I've
% encapsulated this in a function to make it easier to track down where it's
% done if the behaviour needs to change in the future.
adjust_exclude_owner(SubscriberCount) -> SubscriberCount - 1.
