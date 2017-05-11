%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing user field change notification
%%%
-module(mod_wocky_user_notify).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").

-behaviour(gen_mod).

%% gen_mod handlers
-export([start/2, stop/1]).


%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    ejabberd_hooks:add(wocky_user_updated, Host,
                       fun user_updated/2, 50).

stop(Host) ->
    ejabberd_hooks:delete(wocky_user_updated, Host,
                          fun user_updated/2, 50).

%%%===================================================================
%%% Roster update hook handler
%%%===================================================================

-spec user_updated(ejabberd:luser(), ejabberd:lserver()) -> ok.
user_updated(LUser, _LServer) ->
    User = ?wocky_repo:get(?wocky_user, LUser),
    WithContact = ?wocky_roster_item:find_users_with_contact(LUser),
    lists:foreach(notify_user_update(User, _), WithContact).

notify_user_update(Item = #{id := User, server := Server},
                   #{id := WithContact})  ->
    ejabberd_router:route(jid:make(User, Server, <<>>),
                          jid:make(WithContact, Server, <<>>),
                          user_change_packet(Item)).

user_change_packet(Item) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [make_user_changed_el(Item)]}.

make_user_changed_el(Item) ->
    #xmlel{name = <<"user-changed">>,
           attrs = [{<<"xmlns">>, ?NS_USER}],
           children = [make_item_el(Item)]}.

make_item_el(#{id := User, server := Server}) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"jid">>, jid:to_binary(jid:make(User, Server, <<>>))}]}.
