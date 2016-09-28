%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing roster notifications
%%% See https://github.com/hippware/tr-wiki/wiki/Roster-notifications
%%%
-module(mod_wocky_roster_notify).

-behaviour(gen_mod).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").
-include("wocky_roster.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    ejabberd_hooks:add(roster_updated, Host,
                       fun roster_updated/3, 50).

stop(Host) ->
    ejabberd_hooks:delete(roster_updated, Host,
                          fun roster_updated/3, 50).

%%%===================================================================
%%% Roster update hook handler
%%%===================================================================

-spec roster_updated(ejabberd:luser(), ejabberd:lserver(),
                     wocky_roster()) -> ok.
roster_updated(LUser, LServer, Item) ->
    Viewers = wocky_db_user:get_roster_viewers(LUser, LServer),
    lists:foreach(notify_roster_update(LUser, LServer, Item, _), Viewers).

notify_roster_update(LUser, LServer, Item, Viewer) ->
    ejabberd_router:route(jid:make(LUser, LServer, <<>>),
                          jid:from_binary(Viewer),
                          roster_change_packet(Item)).

roster_change_packet(Item) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [make_roster_changed_el(Item)]}.

make_roster_changed_el(Item) ->
    #xmlel{name = <<"roster-changed">>,
           attrs = [{<<"xmlns">>, ?NS_WOCKY_ROSTER}],
           children = [make_item_el(Item)]}.

make_item_el(#wocky_roster{contact_jid = JID, contact_handle = Name,
                           subscription = Subscription, groups = Groups}) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"jid">>, jid:to_binary(JID)},
                    {<<"name">>, Name},
                    {<<"subscription">>, atom_to_binary(Subscription, utf8)}],
           children = group_els(Groups)}.

group_els(Groups) ->
    [group_el(G) || G <- Groups].

group_el(Group) ->
    #xmlel{name = <<"group">>, children = [#xmlcdata{content = Group}]}.
