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
-include("wocky_roster.hrl").

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-export([handle_retrieve_affiliations/3,
         handle_update_affiliations/4,
         handle_subscribe/3,
         handle_unsubscribe/3,
         handle_retrieve_subscribers/3,
         notify_new_viewers/4]).

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
        OldAffiliations <- {ok, wocky_db_bot:affiliations(Server, ID)},
        ChangedAffiliations <- get_affiliations(Children),
        Affiliations <- check_affiliations(From, ChangedAffiliations, []),
        update_affiliations(Server, ID, Affiliations),
        wocky_bot_util:notify_affiliates(To, ID, Affiliations),
        NewAffiliations <- {ok, wocky_db_bot:affiliations(Server, ID)},
        notify_affiliates_visibility(Server, ID,
                                     wocky_db_bot:visibility(Server, ID),
                                     OldAffiliations, NewAffiliations),
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

check_affiliations(_From, [], Acc) -> {ok, Acc};
check_affiliations(From, [Affiliation | Rest], Acc) ->
    case check_affiliation(From, Affiliation) of
        {error, E} ->
            {error, E};
        CleanAffiliation ->
            check_affiliations(From, Rest, [CleanAffiliation | Acc])
    end.

check_affiliation(_From, {User, <<"none">>}) ->
    {User, none};
check_affiliation(From, {User, <<"spectator">>}) ->
    case wocky_db_roster:is_friend(From, User) of
        true ->
            {User, spectator};
        false ->
            {error, ?ERRT_BAD_REQUEST(
                       ?MYLANG, <<(jid:to_binary(User))/binary,
                                  " is not a friend">>)}
    end;
check_affiliation(_From, {_User, Role}) ->
    {error, ?ERRT_BAD_REQUEST(
               ?MYLANG, <<"Invalid affiliate role: ", Role/binary>>)}.

update_affiliations(Server, ID, Affiliations) ->
    wocky_db_bot:update_affiliations(Server, ID, Affiliations).

make_affiliations_update_element(Server, ID) ->
    Affiliations = wocky_db_bot:affiliations(Server, ID),
    #xmlel{name = <<"affiliations">>,
           attrs = list_attrs(ID, Affiliations)}.

notify_affiliates_visibility(Server, ID, ?WOCKY_BOT_VIS_WHITELIST,
                             OldAffiliations, NewAffiliations) ->
    AddedAffiliations =
    lists:sort(NewAffiliations) -- lists:sort(OldAffiliations),
    AddedAffiliationsJIDs = [A || {A, _} <- AddedAffiliations],
    notify_new_viewers(Server, ID, AddedAffiliationsJIDs);
notify_affiliates_visibility(_, _, _, _, _) -> ok.

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
           attrs = list_attrs(ID, Subscribers),
           children = make_subscriber_elements(Subscribers)}.

make_subscriber_elements(Subscribers) ->
    lists:map(fun make_subscriber_element/1, Subscribers).

make_subscriber_element(JID) ->
    #xmlel{name = <<"subscriber">>,
           attrs = [{<<"jid">>, jid:to_binary(JID)}]}.

%%%===================================================================
%%% Access change notifications
%%%===================================================================

-spec notify_new_viewers(Server :: ejabberd:lserver(),
                         ID :: wocky_db:id(),
                         OldVisibility :: bot_visibility() | none,
                         NewVisibility :: bot_visibility()) -> ok.

% Unchanged visibility
notify_new_viewers(_, _, Vis, Vis) -> ok;

% Old visibility was followers or public - there will
% never be additional viewers to notify
notify_new_viewers(_, _, ?WOCKY_BOT_VIS_PUBLIC, _) -> ok;
notify_new_viewers(_, _, ?WOCKY_BOT_VIS_FOLLOWERS, _) -> ok;

% General case of changed visibility
notify_new_viewers(Server, ID, OldVisibility, NewVisibility) ->
    Owner = wocky_db_bot:owner(Server, ID),
    OldViewers = get_viewers(Server, ID, Owner, OldVisibility),
    CurrentViewers = get_viewers(Server, ID, Owner, NewVisibility),
    AddedViewers = CurrentViewers -- OldViewers,
    notify_new_viewers(Server, ID, AddedViewers).

notify_new_viewers(Server, ID, NewViewers) ->
    lists:foreach(notify_new_viewer(Server, ID, _), NewViewers).

notify_new_viewer(Server, ID, Viewer) ->
    ejabberd_router:route(jid:make(<<>>, Server, <<>>),
                          Viewer,
                          bot_visible_stanza(Server, ID)).

bot_visible_stanza(Server, ID) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [#xmlel{name = <<"bot">>,
                              attrs = [{<<"xmlns">>, ?NS_BOT}],
                              children = bot_visible_children(Server, ID)}]}.

bot_visible_children(Server, ID) ->
    [cdata_el(<<"jid">>, jid:to_binary(
                           wocky_bot_util:make_jid(Server, ID))),
     cdata_el(<<"id">>, ID),
     cdata_el(<<"server">>, wocky_app:server()),
     cdata_el(<<"action">>, <<"show">>)].

cdata_el(Name, Value) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content = Value}]}.

get_viewers(_, _, _, none) -> [];
get_viewers(_, _, _, ?WOCKY_BOT_VIS_OWNER) -> [];
get_viewers(Server, ID, Owner, ?WOCKY_BOT_VIS_WHITELIST) ->
    [JID || {JID, _} <- wocky_db_bot:affiliations(Server, ID)] -- [Owner];
get_viewers(_, _, #jid{luser = LUser, lserver = LServer},
            ?WOCKY_BOT_VIS_FRIENDS) ->
    [roster_to_jid(R) || R <- wocky_db_roster:friends(LUser, LServer)];
%% Notifiy followers for both 'public' and 'followers' bots:
get_viewers(_, _, #jid{luser = LUser, lserver = LServer}, _) ->
    [roster_to_jid(R) || R <- wocky_db_roster:followers(LUser, LServer)].

roster_to_jid(#wocky_roster{contact_jid = SimpleJID}) ->
    jid:make(SimpleJID).

%%%===================================================================
%%% Common helpers
%%%===================================================================

list_attrs(ID, List) ->
    [{<<"xmlns">>, ?NS_BOT},
     {<<"node">>, wocky_bot_util:make_node(ID)},
     {<<"size">>, integer_to_binary(length(List))},
     {<<"hash">>, wocky_bot_util:list_hash(List)}].
