%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bots - user management operations
%%% See https://github.com/hippware/tr-wiki/wiki/Bot
%%%
-module(wocky_bot_users).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").
-include("wocky_bot.hrl").

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-export([handle_retrieve_affiliations/3,
         handle_update_affiliations/4,
         handle_subscribe/4,
         handle_unsubscribe/3,
         handle_retrieve_subscribers/3]).

%%%===================================================================
%%% Action - retrieve affiliations
%%%===================================================================

handle_retrieve_affiliations(From, #jid{lserver = Server}, Attrs) ->
    do([error_m ||
        ID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_owner(Server, ID, From),
        {ok, make_affiliations_element(Server, ID)}
       ]).

make_affiliations_element(Server, ID) ->
    Affiliates = wocky_db_bot:affiliations(Server, ID),
    #xmlel{name = <<"affiliations">>,
           attrs = list_attrs(ID, Affiliates),
           children = wocky_bot_util:make_affiliate_elements(Affiliates)}.

%%%===================================================================
%%% Action - update affiliations
%%%===================================================================

handle_update_affiliations(From, To = #jid{lserver = Server},
                           Attrs, Children) ->
    do([error_m ||
        ID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_owner(Server, ID, From),
        DirtyAffiliations <- get_affiliations(Children),
        OwnerRoster <- {ok, wocky_db_bot:owner_roster(Server, ID)},
        Affiliations <- check_affiliations(DirtyAffiliations, OwnerRoster, []),
        update_affiliations(Server, ID, Affiliations),
        wocky_bot_util:notify_affiliates(To, ID, Affiliations),
        {ok, make_affiliations_update_element(Server, ID)}
       ]).

get_affiliations(Elements) ->
    {ok, lists:foldl(fun get_affiliation/2, [], Elements)}.

get_affiliation(El = #xmlel{name = <<"affiliation">>}, Acc) ->
    [element_to_affiliation(El) | Acc];
get_affiliation(_, Acc) -> Acc.

element_to_affiliation(#xmlel{attrs = Attrs}) ->
    JID = xml:get_attr_s(<<"jid">>, Attrs),
    Affiliation = xml:get_attr_s(<<"affiliation">>, Attrs),
    {jid:from_binary(JID), Affiliation}.

check_affiliations([], _OwnerRoster, Acc) -> {ok, Acc};
check_affiliations([Affiliation | Rest], OwnerRoster, Acc) ->
    case check_affiliation(Affiliation, OwnerRoster) of
        {error, E} ->
            {error, E};
        CleanAffiliation ->
            check_affiliations(Rest, OwnerRoster,
                               [CleanAffiliation | Acc])
    end.

check_affiliation({User, <<"none">>}, _OwnerRoster) ->
    {User, none};
check_affiliation({User, <<"spectator">>}, OwnerRoster) ->
    case lists:any(jid:are_equal(User, _), OwnerRoster) of
        true ->
            {User, spectator};
        false ->
            {error, ?ERRT_BAD_REQUEST(
                       ?MYLANG, <<(jid:to_binary(User))/binary,
                                  " is not a friend">>)}
    end;
check_affiliation({_User, Role}, _OwnerRoster) ->
    {error, ?ERRT_BAD_REQUEST(
               ?MYLANG, <<"Invalid affiliate role: ", Role/binary>>)}.

update_affiliations(Server, ID, Affiliations) ->
    wocky_db_bot:update_affiliations(Server, ID, Affiliations).

make_affiliations_update_element(Server, ID) ->
    Affiliations = wocky_db_bot:affiliations(Server, ID),
    #xmlel{name = <<"affiliations">>,
           attrs = list_attrs(ID, Affiliations)}.

%%%===================================================================
%%% Action - subscribe
%%%===================================================================

handle_subscribe(From, #jid{lserver = Server}, Attrs, Children) ->
    do([error_m ||
        ID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_access(Server, ID, From),
        Follow <- get_follow(Children),
        subscribe_bot(Server, ID, From, Follow),
        {ok, []}
       ]).

subscribe_bot(Server, ID, From, Follow) ->
    {ok, wocky_db_bot:subscribe(Server, ID, From, Follow)}.

get_follow([]) -> {ok, false};
get_follow([Child | Rest]) ->
    case Child of
        #xmlel{name = <<"follow">>,
               children = [#xmlcdata{content = <<"1">>}]} -> {ok, true};
        _ -> get_follow(Rest)
    end.

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
           attrs = list_attrs(ID, Subscribers),
           children = make_subscriber_elements(Subscribers)}.

make_subscriber_elements(Subscribers) ->
    lists:map(fun make_subscriber_element/1, Subscribers).

make_subscriber_element({JID, Follow}) ->
    #xmlel{name = <<"subscriber">>,
           attrs = [{<<"jid">>, jid:to_binary(JID)}],
           children = [wocky_bot_util:make_follow_element(Follow)]}.

%%%===================================================================
%%% Action - common helpers
%%%===================================================================

list_attrs(ID, List) ->
    [{<<"xmlns">>, ?NS_BOT},
     {<<"node">>, wocky_bot_util:make_node(ID)},
     {<<"size">>, integer_to_binary(length(List))},
     {<<"hash">>, wocky_bot_util:list_hash(List)}].
