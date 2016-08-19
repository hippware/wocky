%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing roster subscription
%%% See https://github.com/hippware/tr-wiki/wiki/Roster-subscription
%%%
-module(mod_roster_subscription).

-behaviour(gen_mod).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").
-include("wocky_roster.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).

%% IQ hook
-export([handle_iq/3]).

%% Internal
-export([roster_change/2]).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_WOCKY_ROSTER,
                                  ?MODULE, handle_iq, parallel),
    mod_disco:register_feature(Host, ?NS_WOCKY_ROSTER),
    ejabberd_hooks:add(roster_process_item, Host, fun roster_change/2, 50).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_WOCKY_ROSTER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_WOCKY_ROSTER),
    ejabberd_hooks:delete(roster_process_item, Host, fun roster_change/2, 50).

%%%===================================================================
%%% Event handler
%%%===================================================================

-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> iq().
handle_iq(From, To, IQ) ->
    case handle_iq_type(From, To, IQ) of
        {ok, ResponseIQ} -> ResponseIQ;
        {error, Error} -> wocky_util:make_error_iq_response(IQ, Error)
    end.

% Retrieve
handle_iq_type(From, To, IQ = #iq{type = get,
                                  sub_el = #xmlel{name = <<"query">>}
                                 }) ->
    handle_retrieve(From, To, IQ);

% Subscribe
handle_iq_type(From, To, IQ = #iq{type = set,
                                  sub_el = #xmlel{name = <<"subscribe">>}
                                 }) ->
    handle_subscribe(From, To, IQ);

%Unsubscribe
handle_iq_type(From, To, IQ = #iq{type = set,
                                  sub_el = #xmlel{name = <<"unsubscribe">>}
                                 }) ->
    handle_unsubscribe(From, To, IQ);

handle_iq_type(_From, _To, _IQ) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid query">>)}.

%%%===================================================================
%%% Hook Handler
%%%===================================================================

-spec roster_change(wocky_db_roster:roster_item(), ejabberd:lserver()) ->
    wocky_db_roster:roster_item().
roster_change(Item = #wocky_roster{user = LUser}, LServer) ->
    send_roster_push_el(LUser, LServer, make_item_el(Item)),
    Item.

send_roster_push_el(LUser, LServer, ItemEl) ->
    Query = #xmlel{name = <<"query">>,
                   attrs = [{<<"xmlns">>, ?NS_ROSTER}],
                   children = [ItemEl]},
    IQ = #iq{id = wocky_util:iq_id(), type = set, sub_el = Query},
    route_to_all_subscribers(LUser, LServer, jlib:iq_to_xml(IQ)).

route_to_all_subscribers(LUser, LServer, Packet) ->
    Subscribers = wocky_db_user:get_subscribers(LUser, LServer),
    SubscriberJIDS = [jid:from_binary(S) || S <- Subscribers],
    From = jid:make(LUser, LServer, <<>>),
    lists:foreach(
      ejabberd_router:route(From, _, Packet), SubscriberJIDS
     ).

%%%===================================================================
%%% Action - retrieve
%%%===================================================================

handle_retrieve(From, To, IQ) ->
    do([error_m ||
        check_permissions(From, To),
        Roster <- get_roster(To),
        RosterEl <- make_roster_el(Roster),
        {ok, IQ#iq{type = result, sub_el = RosterEl}}
       ]).

get_roster(#jid{luser = LUser, lserver = LServer}) ->
    case wocky_db_user:does_user_exist(LUser, LServer) of
        false -> {error, ?ERR_ITEM_NOT_FOUND};
        true -> {ok, wocky_db_roster:get_roster(LUser, LServer)}
    end.

make_roster_el(Roster) ->
    {ok, #xmlel{name = <<"query">>,
                attrs = [{<<"xmlns">>, ?NS_WOCKY_ROSTER}],
                children = make_item_els(Roster)}}.

%%%===================================================================
%%% Action - subscribe
%%%===================================================================

handle_subscribe(From, To, IQ) ->
    do([error_m ||
        check_permissions(From, To),
        change_subscription(subscribe, From, To),
        {ok, IQ#iq{type = result}}
       ]).

%%%===================================================================
%%% Action - unsubscribe
%%%===================================================================

handle_unsubscribe(From, To, IQ) ->
    do([error_m ||
        change_subscription(unsubscribe, From, To),
        {ok, IQ#iq{type = result}}
       ]).

%%%===================================================================
%%% Common helpers
%%%===================================================================

check_permissions(_From, _To) ->
    %% TODO: Check permissions once a permissions checking
    %% system exists
    ok.

change_subscription(Operation, From, #jid{luser = LUser, lserver = LServer}) ->
    ok = wocky_db_user:Operation(LUser, LServer, From).

make_item_els(Roster) ->
    [make_item_el(R) || R <- Roster].

make_item_el(#wocky_roster{contact_jid = JID, contact_handle = Name,
                           subscription = Subscription, groups = Groups}) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"jid">>, jid:to_binary(JID)},
                    {<<"name">>, Name} |
                    maybe_subscription_attr(Subscription)],
           children = group_els(Groups)}.

maybe_subscription_attr(remove) ->
    [];
maybe_subscription_attr(Subscription) ->
    [{<<"subscription">>, atom_to_binary(Subscription, utf8)}].

group_els(Groups) ->
    [group_el(G) || G <- Groups].

group_el(Group) ->
    #xmlel{name = <<"group">>, children = [#xmlcdata{content = Group}]}.
